
/* C compiler file sparc/gen.c :  Copyright (C) Codemist Ltd., 1992.     */
/* version 20a */

/* AM memo: we could allow stack_move to reach -0x20 for better arg      */
/* code (since the window is only the first 0x40 bytes).                 */

/* exports:
   void show_instruction(J_OPCODE op, VRegInt r1, VRegInt r2, VRegInt m);
   RealRegister local_base(Binder *b);
   int32 local_address(Binder *b);
   bool immed_cmp(int32);
      also (beware peephole):
   void setlabel(LabelNumber *);
   void branch_round_literals(LabelNumber *);
*/

#ifdef __STDC__
#  include <string.h>
#else
#  include <strings.h>
#endif

#include <stdlib.h>
#include "globals.h"
#include "builtin.h"
#include "mcdep.h"
#include "mcdpriv.h"
#include "xrefs.h"
#include "ops.h"
#include "jopcode.h"
#include "store.h"
#include "codebuf.h"
#include "regalloc.h"
#include "cg.h"        /* for procflags, greatest_stackdepth */
#include "util.h"      /* padsize */
#include "errors.h"

#define WINDOW_SIZE   0x5c
#define WINDOW_SIZE_D 0x60
#define WINDOW_ARG1   0x44
#define WINDOW_ARG0   0x40
#define SIMM13_MAX TARGET_LDRK_MAX
#define SIMM13_MIN TARGET_LDRK_MIN
#define SIMM13(n) (SIMM13_MIN <= (n) && (n) <= SIMM13_MAX)

#define NONLEAF (PROC_ARGPUSH | PROC_ARGADDR | PROC_BIGSTACK | BLKCALL)

/* vvvvvvvvvvvvvvvvvvvvvv    PEEPHOLER    vvvvvvvvvvvvvvvvvvvv */

int32 sparc_opt = 0;             /* Control optimisations */

enum { OUT_NULL, OUT_COUNT,
       OUT_INSTR, OUT_LABREF,
       OUT_CALL, OUT_EXTCALL,
       OUT_CODELAB, OUT_CODELABHI, OUT_CODELABLO,
       OUT_EXTLABHI, OUT_EXTLABLO };

typedef struct Resources
{
  struct {
    unsigned32   iregs;
    unsigned32   fregs;
    unsigned int cc:1,
                 fcc:1;
  } reads,
    writes;
  unsigned int opclass:8,
               bus:3,
               fpu:1,
               blocked:1;
} Resources;

struct mcpeepdata;

typedef struct depends {
  struct depends *next;
  struct mcpeepdata *inst;
} depends;

typedef struct mcpeepdata {
  struct mcpeepdata *prev;	/* previous instruction in execution order */
  struct mcpeepdata *next;	/* next instruction in execution order */
  int32  prior;     		/* # instructions which must precede this one */
  depends *post;      		/* instructions which must follow this one */
  int32 count;			/* /* for now, length of *post chain */
  int32 type;         		/* e.g. OUT_INSTR */
  int32 w;            		/* 32 bits of data */
  Resources uses;     		/* regs used, usage class, bus activity etc */
  Symstr *s;          		/* external symbol (if needed) */
  unsigned32 off;     		/* offset (if needed) */
  LabelNumber *lab;   		/* if needed */
  int32 reftype;      		/* label reference type */
} mcpeepdata;

static mcpeepdata *mc_peeps;
static struct {
  int bus_count:3;
  int fpu:1;
  } state = {0,0};

typedef struct BasicBlock {
  struct BasicBlock	*cdr;		/* generation order */
  struct BasicBlock	*nextbb;	/* execution order */
  LabelNumber 		*nextlab;	/* at head of ->nextbb */
  LabelNumber 		*head;		/* at head of this bb */
  mcpeepdata  		*code;
  mcpeepdata  		*codeend; 	/* need quick access to both ends */
  int32       		refcount;
  int32       		flags;
} BasicBlock;

BasicBlock *bb_list = NULL,
           *bb_list_end = NULL,
           *current_bb = NULL;

/* static depends *new_dependency (depends *, mcpeepdata *); */
#define new_dependency(a,b) (depends *)syn_list2((int)(a),  (int)(b))
static mcpeepdata *new_mcpeep (void);
static void flush_peepholer(void);
static void add_flush_noop(void);
static bool insert_one_peep (mcpeepdata *);
static bool swappable (mcpeepdata*, mcpeepdata*);
static void end_basic_block (mcpeepdata *, int32, int32);
static void start_basic_block (LabelNumber *);
static void emit_code (void);

/* ^^^^^^^^^^^^^^^^^^^^^^    PEEPHOLER    ^^^^^^^^^^^^^^^^^^^^ */



enum Stack_Correction { EXACT = 0, INWINDOW = -0x18 };
static void correct_stack(enum Stack_Correction);
static int32 stack_safelimit;
static int32 stack_move = 0, fp_minus_sp, firstargoff, firstlocoff, nargwords;
static int32 fptemp;                    /* for FIX/FLT/MOVDIR etc.      */
static bool window_rolled = 0;
static int32 explicit_savesize = 0;     /* only valid if !window_rolled */
static int32 double_lit;
static int32 scc_regs, fcmp_pending = 0, fcmp_used = 0, cmp_pending = 0;
static int32 casebranch_pending = 0, casebranch_r1r;
static int32 adr1, adm, adr2, adconpend;
LabelNumber *base_label;
static LabelNumber *returnlab;

/* For Traps */
#ifdef TARGET_IS_DRS6000
#define SYSTRAP (8)
#else
#define SYSTRAP (0)
#endif

static int32 C_FROMQ(int32 q, int32 fcmp)
      /* turns JOP cond branch to sparc cond field */
{
  if (fcmp)
    switch (q)
      {
	/* case Q_MI:   return C_RN;                     */
	/* case Q_VS:   return C_RV;                     */
      case Q_EQ:   return C_FREQ;
      case Q_NE:   return C_FRNE;
      case Q_GE:   return C_FRGE;
      case Q_GT:   return C_FRGT;
      case Q_LE:   return C_FRLE;
      case Q_LT:   return C_FRLT;
      default: fprintf(stderr, "Floating case Q_EQU=%lx Q_NEU=%lx\n", Q_EQ|Q_UBIT, Q_NE|Q_UBIT);
	syserr(syserr_fromq, (long)q);
      case Q_AL:   return C_FAL;
      }
  else
    switch (q)
      {
	/* case Q_MI:   return C_RN;                     */
	/* case Q_VS:   return C_RV;                     */
      case Q_UEQ:
      case Q_EQ:   return C_REQ;
      case Q_UNE:
      case Q_NE:   return C_RNE;
      case Q_HS:   return C_RGEU;
      case Q_LS:   return C_RLEU;
      case Q_HI:   return C_RGTU;
      case Q_LO:   return C_RLTU;
      case Q_GE:   return C_RGE;
      case Q_GT:   return C_RGT;
      case Q_LE:   return C_RLE;
      case Q_LT:   return C_RLT;
      default: 
	fprintf(stderr, "Q_PL=%lx Q_MI=%lx Q_XXX=%lx\n", Q_PL, Q_MI, Q_XXX);
	syserr(syserr_fromq, (long)q);
      case Q_AL:   return C_AL;
      }
}


/* Try to avoid 32 bit immediate values in the loop optimiser. */
bool immed_cmp(int32 n) { return SIMM13(n); }

static RealRegister map_intreg(RealRegister r)
{
  static RealRegister tab[32] = { 0,1,2,3,4,5,6,7,
				    24,25,26,27,28,29,14,15,
				    16,17,18,19,20,21,22,23,
				    8,9,10,11,12,13,30,31};
  return tab[r];
}

RealRegister local_base(Binder *b)
{
  int32 p = bindaddr_(b);
  switch (p & BINDADDR_MASK)
    { 
    default: syserr(syserr_local_base, (long)p);
    case BINDADDR_ARG:
    case BINDADDR_LOC: return (window_rolled? R_FP: R_SP);
    }
}

int32 local_address(Binder *b)
{
  int32 p = bindaddr_(b);
  int32 q = p & ~BINDADDR_MASK;
  int32 r;
  if (stack_move<0) correct_stack(EXACT);       /* needed? */
  switch (p & BINDADDR_MASK) {
  default: syserr(syserr_local_address, (long)p);
  case BINDADDR_LOC:    /* q = 4, 8, 12, 16, ... */
    r = (window_rolled?
         /* R_FP + */ firstlocoff - q:
         /* R_SP + */ firstlocoff + stack_move + fp_minus_sp - q);
    return r;
  case BINDADDR_ARG:    /* q = 0, 4, 8, 12, ... */
    if (!(procflags & PROC_ARGPUSH)) {
      /* this should be used for LDR(f/v)1 only, check a bit */
      if (nargwords <= NARGREGS || fp_minus_sp != 0)
	syserr(syserr_local_addr);
    }
    r = (window_rolled?
         /* R_FP + */ firstargoff + q:
         /* R_SP + */ firstargoff + stack_move + fp_minus_sp + q);
    return r;
  }
}

static int firstbit(int32 w)
{   int i;
    for (i = 0; i < 32; i++) if (w & ((int32)1<<i)) return i;
    syserr(syserr_firstbit);
    return 0;
}

/* /* the following is very temporary indeed .... */
static void set_reg_uses (Resources *uses, int32 reads, int32 writes)
{
  int32 i;
/*  fprintf(stderr, "set_reg_uses: %lx %lx\n", reads, writes); */
  for (i=0; i<4; i++) {
    int32 r = reg4_(reads);
    if (r ==0) ;
    else if (r < 32) uses->reads.iregs |= regbit(r);
    else if (r < 64) uses->reads.fregs |= (int) regbit(r-32);
    else if (r == R_CC) uses->reads.cc = 1;
    else if (r == R_FCC) uses->reads.fcc = 1;
    else if (R_BUS1 <= r && r <= R_BUS4) {
      if (uses->bus == 0) uses->bus = (int) (r+1-R_BUS1);
      else if (uses->bus != (r+1-R_BUS1)) syserr("set_reg_uses bus %x %lx",
						 uses->bus, r);
    }
    else syserr("set_reg_uses reads %lx from %lx", r, reads);
    reads >>= 7;
  }
  uses->opclass = (int) (writes & R_MASK);
  if (uses->opclass & OP_FLOAT) uses->fpu = 1;
  writes >>= 14;
  for (i=0; i<2; i++) {
    int32 r;
    r = reg4_(writes);
    if (r ==0) ;
    else if (r < 32) uses->writes.iregs |= regbit(r);
    else if (r < 64) uses->writes.fregs |= (int) regbit(r-32);
    else if (r == R_CC) uses->writes.cc = 1;
    else if (r == R_FCC) uses->writes.fcc = 1;
    else if (R_BUS1 <= r && r <= R_BUS4) {
      if (uses->bus == 0) uses->bus = (int) (r+1-R_BUS1);
      else if (uses->bus != (r+1-R_BUS1)) syserr("set_reg_uses bus %x %lx",
						 uses->bus, r);
    }
    else syserr("set_reg_uses writes %lx from %lx", r, writes);
    writes >>= 7;
  }
}

static bool outP32(int32 w, int32 reads, int32 writes)
{
  mcpeepdata *mc_peep1;
/*  fprintf(stderr, "calling outP32 %lx\n", w); */
  if (reg2_(writes) == R_CC) {
    mcpeepdata *mc_peep = mc_peeps;
    while (mc_peep != NULL) {
      if (mc_peep->uses.writes.cc) { /* /* what about .fcc? */
	mc_peep->w &= ~0x00800000; /* Remove CC bits */
	mc_peep->uses.writes.cc = 0; /* Does not write condition code */
	break;			/* Cannot be more than one */
      }
      mc_peep = mc_peep->prev;
    }
  }
  
  mc_peep1 = new_mcpeep ();
  mc_peep1->type = OUT_INSTR;
  mc_peep1->w = w;
  set_reg_uses (&(mc_peep1->uses), reads, writes);
  
  return insert_one_peep (mc_peep1);
}

static void move_register(RealRegister r1, RealRegister r2)
{   /* r1 = r2   */
  if (r1!=r2)
    {
      outP32(OP_ORCC | F_RD(r1) | F_RS1(r2) | F_RS2(R_ZERO),
	     reads_(r2,0,0,0), writes_(r1, R_CC, OP_NULL));
      if (scc_regs & regbit(r2)) scc_regs |= regbit(r1);
      else scc_regs = regbit(r1) | regbit(r2);
    }
}

#define sethi_(r,d) outP32(OP_SETHI | F_RD(r) | (((unsigned32)(d)) >> 10), \
			   reads_(0,0,0,0), writes_(r,0,OP_NULL) )
/* set high 22 bits of register r to high 22 bits of d */

static void load_integer(RealRegister r, int32 n)
     /* Set register r to the integer n. */
{
  if (n==0) move_register (r, R_ZERO);
  else if (SIMM13(n)) {
    outP32(OP_ORCC | F_RD(r) | F_RS1(R_ZERO) | F_SIMM13(n),
	   reads_(0,0,0,0), writes_(r,R_CC,OP_NULL));
    scc_regs = regbit(r);
  }
  else {
    sethi_ (r, n);
    if ((n&0x3ff) == 0) scc_regs &= ~regbit(r);
    else {
      outP32(OP_ORCC | F_RD(r) | F_RS1(r) | F_SIMM13(n&0x3ff),
	     reads_(r,0,0,0), writes_(r,R_CC,OP_NULL));
      scc_regs = regbit(r);
    }
  }
}

static void outrelref(int32 op, RealRegister r1, RealRegister r2, int32 d,
		      int32 reads, int32 writes)
{
  if (r2 >= R_F0) syserr("outrelref r2=%ld",r2); /* /* not possible? */
    if (d == 0)
      outP32(op | F_RD(r1) | F_RS1(r2), reads, writes);
    else if (TARGET_LDRK_MIN <= d && d <= TARGET_LDRK_MAX)
      outP32(op | F_RD(r1) | F_RS1(r2) | F_SIMM13(d), reads, writes);
    else if (r2==R_ZERO) {	/* /* unlikely, if not impossible ? */
      sethi_(R_TM, d);
      set_reg4_(reads, R_TM);
      outP32(op | F_RD(r1) | F_RS1(R_TM) | F_SIMM13(d&0x3ff), reads, writes);
    }
    else {
      load_integer (R_TM, d);
      set_reg4_(reads, R_TM);
      outP32(op | F_RD(r1) | F_RS1(r2) | F_RS2(R_TM), reads, writes);
    }
  /* cannot set scc_regs here */
}

static void outfptempref(int32 op, RealRegister r, int32 d,
		         int32 reads, int32 writes)
{   /* Following code matches local_address...                          */
    RealRegister base = window_rolled ? R_FP : R_SP;
    int32 disp = window_rolled ?
         /* R_FP + */ firstlocoff :
         /* R_SP + */ firstlocoff + stack_move + fp_minus_sp;
    if (fptemp == 0) syserr("outfptempref");
    set_reg3_(reads, base);
    outrelref(op, r, base, disp+d, reads, writes);
}

static bool outRext1(Symstr *name, int32 off, int xr)
     /* the obj_symref and codexrefs data structures may be mergeable. */
     /* /* I'm not using xr - should I be ?? */
{
  mcpeepdata *mc_peep1 = new_mcpeep ();
/*  fprintf(stderr, "calling outRext1\n", op2); */
  /* external refs are absolute refs here */
  mc_peep1->type = OUT_EXTLABHI;
  mc_peep1->w = OP_SETHI | F_RD(R_TM) | F_IMM22(off);
  mc_peep1->uses.writes.iregs = regbit(R_TM);
/*  mc_peep1->uses.opclass = OP_NULL; unnecessary */
  mc_peep1->s = name;
  mc_peep1->off = off;
  return insert_one_peep(mc_peep1);
}

static bool outRext2(int32 op2, Symstr *name, int32 off, int xr,
		     int32 reads, int32 writes)
     /* the obj_symref and codexrefs data structures may be mergeable. */
     /* /* I'm not using xr - should I be ?? */
{
  mcpeepdata *mc_peep1 = new_mcpeep ();
/*  fprintf(stderr, "calling outRext2 %lx\n", op2); */
  /* external refs are absolute refs here */
  set_reg4_ (reads, R_TM);
  mc_peep1->type = OUT_EXTLABLO;
  mc_peep1->w = op2 | F_RS1(R_TM) | F_SIMM13(off);
  set_reg_uses (&(mc_peep1->uses), reads, writes);
  mc_peep1->s = name;
  mc_peep1->off = off;
  return insert_one_peep(mc_peep1);
  
}

static bool outRext(int32 op2, Symstr *name, int32 off, int xr,
		    int32 reads, int32 writes)
     /* the obj_symref and codexrefs data structures may be mergeable. */
     /* /* I'm not using xr - should I be ?? */
{
  (void) outRext1 (name, off, xr);
  return outRext2 (op2, name, off, xr, reads, writes);
}

static void outRext_ebb(int32 op2, Symstr *name, int32 off, int xr, int32 endtype)
     /* the obj_symref and codexrefs data structures may be mergeable. */
     /* /* I'm not using xr - should I be ?? */
{
  mcpeepdata *mc_peep1;
  (void) outRext1 (name, off, xr);
  
  mc_peep1 = new_mcpeep ();
  /* external refs are absolute refs here */
  mc_peep1->type = OUT_EXTLABLO;
  mc_peep1->w = op2 | F_RS1(R_TM) | F_SIMM13(off);
  mc_peep1->uses.reads.iregs = regbit(R_TM);
/*  mc_peep1->uses.opclass = OP_NULL; */
  mc_peep1->s = name;
  mc_peep1->off = off;
  end_basic_block(mc_peep1, 1, endtype);
}

static void outcodexref(int32 op2, LabelNumber *name, int32 off, int xr,
			int32 reads, int32 writes)
     /* the obj_symref and codexrefs data structures may be mergeable. */
     /* /* I'm not using xr - should I be ?? */
{
  mcpeepdata *mc_peep1 = new_mcpeep ();
/*  fprintf(stderr, "calling outcodexref %lx\n", OP_SETHI | F_RD(R_TM) | F_IMM22(off)); */
  mc_peep1->type = OUT_CODELABHI;
  mc_peep1->w = OP_SETHI | F_RD(R_TM) | F_IMM22(off);
  mc_peep1->uses.writes.iregs = regbit(R_TM);
/*  mc_peep1->uses.opclass = OP_NULL; */
  mc_peep1->s = bindsym_(codesegment);
  mc_peep1->off = off;
  mc_peep1->lab = name;
  insert_one_peep(mc_peep1);
  
/*  fprintf(stderr, "calling outcodexref-1 %lx\n", op2); */
  set_reg4_ (reads, R_TM);
  mc_peep1 = new_mcpeep ();
  mc_peep1->type = OUT_CODELABLO;
  mc_peep1->w = op2 | F_RS1(R_TM) | F_SIMM13(off);
  set_reg_uses (&(mc_peep1->uses), reads, writes);
  mc_peep1->s = bindsym_(codesegment);
  mc_peep1->off = off;
  mc_peep1->lab = name;
  insert_one_peep(mc_peep1);
  
}

static void outcall(Symstr *name)
     /* the obj_symref and codexrefs data structures may be mergeable. */
{
  int32 d = obj_symref(name, xr_code, 0);
  mcpeepdata *mc_peep1 = new_mcpeep ();
  
  if (d == -1) {
    mc_peep1->type = OUT_EXTCALL;
    mc_peep1->w    = OP_CALL | 0;
  }
  else {
    mc_peep1->type = OUT_CALL;
    mc_peep1->w    = OP_CALL | (d>>2); /* fix-up at emit stage */
  }
/*  fprintf(stderr, "calling outcall %lx\n", mc_peep1->w); */
  mc_peep1->uses.writes.iregs = regbit(R_LR1);
  mc_peep1->uses.opclass = OP_NULL;
  mc_peep1->s      = name;
  
  end_basic_block (mc_peep1, 1, END_CALL);
}

static void outrestore (void)
{
  /* is there a preceding instruction which can be merged with the RESTORE ? */
  mcpeepdata *mc_peep;
  /* /* nb at the moment mc_peeps points at the *end* of the code */
  for (mc_peep = mc_peeps; mc_peep != NULL; mc_peep = mc_peep->prev) {
    if (mc_peep->post == NULL) { /* no dependencies, can be moved */
      int32 w = mc_peep->w;
      int32 opw = w&0xc1780000,  /* sufficient to detect OP_ADD and OP_OR,
				    ignoring CC bit */
            rdw = D_RD(w,0),
            r1w = D_RS1(w),
	    r2w = D_RS2(w),
	    rsw = D_SIMM13(w);
      int32 cannot_be_merged =
	(mc_peep->type != OUT_INSTR ? 1 :
	 rdw == R_SP ? 1 :	/* and other special cases? */
	 opw == OP_ADD ? 0 :
	 opw != OP_OR ? 1 :
	 r1w == R_ZERO ? 0 :
	 w & A_IMM ? rsw != 0 :
	 r2w != R_ZERO);
      if (cannot_be_merged) continue;
      /* change the ADD/OR into a RESTORE */
      mc_peep->w = OP_RESTORE |
	           F_RD(map_intreg(rdw)) | F_RS1(r1w) |
		   (w & A_IMM ? F_SIMM13(rsw) : F_RS2(r2w));
      mc_peep->uses.reads.iregs = (r1w? regbit(r1w): 0) |
	                          (!(w&A_IMM) && r2w? regbit(r2w): 0) |
			          0xff000000;
	                          /* regbit(R_P1+8)-regbit(R_P1) */
      mc_peep->uses.writes.iregs = (rdw? regbit(rdw): 0) |
	                           0xffffff00;
	                           /* regbit(R_P1+8)-regbit(R_P1)| */
	                           /* regbit(R_V1+8)-regbit(R_V1)| */
	                           /* regbit(R_A1+8)-regbit(R_A1)  */
      mc_peep->uses.writes.cc = 0;
/*    fprintf(stderr,"%s r%ld,%s%ld,r%ld -> RESTORE r%ld,%s%ld,r%ld\n",
 *	      (opw==OP_ADD?"add(cc)":"or(cc)"),
 *	      r1w,(w&A_IMM?"":"r"),(w&A_IMM?rsw:r2w),rdw,
 *	      r1w,(w&A_IMM?"":"r"),(w&A_IMM?rsw:r2w),map_intreg(rdw));
 *    fprintf(stderr,"reads=%.8lx writes=%.8lx\n",
 *	      mc_peep->uses.reads.iregs, mc_peep->uses.writes.iregs);
 */
      {	/* the modified instruction, which logically goes at the end of the
	   basic block, may now depend on some of the instructions which
	   originally followed it (and were rejected as candidates for merging
	   with the RESTORE) */
	mcpeepdata *mc_peep2;
	for (mc_peep2 = mc_peep->next;
	     mc_peep2 != NULL; mc_peep2 = mc_peep2->next) {
	  if (!swappable(mc_peep2, mc_peep)) { /* Have a dependency */
	    mc_peep2->post = new_dependency (mc_peep2->post, mc_peep);
	    mc_peep2->count++;
	    mc_peep->prior++;
	  }
	}
      }
      return;
    }
  }
  /* No candidate found - put in a null RESTORE */
  {
    mcpeepdata *mc_peep1 = new_mcpeep ();
    mc_peep1->type = OUT_INSTR;
    mc_peep1->w = OP_RESTORE | F_RD(R_ZERO) | F_RS1(R_ZERO) | F_RS2(R_ZERO);
    mc_peep1->uses.reads.iregs = 0xff000000;
	                         /* regbit(R_P1+8)-regbit(R_P1) */
    mc_peep1->uses.writes.iregs = 0xffffff00;
	                          /* regbit(R_P1+8)-regbit(R_P1)| */
	                          /* regbit(R_V1+8)-regbit(R_V1)| */
	                          /* regbit(R_A1+8)-regbit(R_A1)  */
  
    insert_one_peep (mc_peep1);
  }
}

#ifdef TARGET_HAS_DEBUGGER
/* The miserable debugger cannot cope with SP relative addresses so we
 * have to calculate FP relative ones specially!
 * Lets hope that we have calculated these correctly!
 * This means that cg.c has set PROC_ARGPUSH which thereby inhibits
 * leaf procedure optimisation (else there would be no FP!)
 * This version of the code has been adapted from a version used
 * with another target (by ACN) and is probably not yet correct!
 */

int32 local_fpaddress(int32 p)   /* /* not adapted for sparc yet !! */
       /* exported for debugger */
{   /* p is bindaddr_() of the relevant binder */
#ifdef FIX_THIS_UP_SOME_DAY_PLEASE
  if (procflags & NONLEAF) switch (p & BINDADDR_MASK)
    {
    case BINDADDR_LOC:
      p &= ~BINDADDR_MASK;
      if (argspushed != 0) p += 4*argspushed;
      return -(p + 12 + 4*bitcount(regmaskvec[0].map & M_VARREGS)
	       + 8*bitcount(regmaskvec[1].map & M_FVARREGS));
    case BINDADDR_ARG:
      p &= ~BINDADDR_MASK;
      return (p >= 4*argsbelowfp) ?
	p - 4*argsbelowfp + 4 :
	  p - 4*argsbelowfp - 4*bitcount(regmaskvec[0].map & M_VARREGS) - 12;
    }
#endif
  syserr(syserr_debug_addr);
  return 0;
}
#endif /* TARGET_HAS_DEBUGGER */

typedef union count_position
{
     int32 i;
     struct s
     {   unsigned int posn:12,
		      line:16,
		      file:4;
     } s;
} count_position;

/* although the idea of setlabel is machine independent, it stays here
   because it back-patches code.  In the long term setlabel should be
   in codebuf.c and call a machine dependent backpatch routine.
*/
void setlabel(LabelNumber *l)
{
  List *p = l->u.frefs;
  
  flush_peepholer();
  if (asmstream) asm_lablist = mkLabList(asm_lablist, l);
  while (p!=NULL)
    {   int32 v = car_(p);
        int32 q = (v & 0x00ffffff);   /* BYTE address */
        unsigned32 w = code_inst_(q);
        unsigned32 wb4 = w;
        switch (v & 0xff000000) {
        case LABREF_DISP22:             /* Bicc and FBfcc instructions only */
          w += (codep-q)>>2;
          if ((wb4 & 0xffc00000) != (w&0xffc00000))
            syserr(syserr_litaddr, (long)w);
          break;
        case LABREF_ABS32:              /* ADCONs (eg BXX)               */
        case LABREF_HI22:               /* SETHI instruction only        */
        case LABREF_LO10:               /* and the instruction following */
          { CodeXref *z = (CodeXref *) (((List3 *)p)->csr);
            z->codexrlitoff += codebase+codep;
            p = (List *)discard3(p);
            continue;
          }
        default:
          syserr(syserr_unknown_labref_type, (long)v);
        }
        code_inst_(q) = w;
        p = (List *)discard2(p);
      }
  lab_setloc_(l, codep /* | 0x80000000 */); /* cheapo union checker for ->frefs */
  scc_regs = 0;		        /* scc no longer known */
}

static int32 litE(int32 w, char *charform)
{
  return lit_findwordaux(w, LIT_FPNUM, charform,
			 LITF_INCODE|LITF_FIRST|LITF_LAST);
}

static int32 litD(int32 w1, int32 w2, char *charform)
     /* beware - alignment should be done elsewhere - is it ? */
     /* /* should I worry about literal pool overflow ? */
{
  int32 litpos;
  double_lit = 7;		/* We need double alignent of data */
  litpos = lit_findwordaux(w1, LIT_FPNUM1, charform,
			   LITF_DOUBLE|LITF_INCODE|LITF_FIRST);
  (void)lit_findwordaux(w2, LIT_FPNUM2, charform,
			LITF_INCODE|LITF_LAST);
  return litpos;
}

static void fpload(int32 mop, RealRegister r1r, int32 fpdouble, FloatCon *fc)
     /* load single & double literals the same way                            */
     /* I do so hope very much that these instructions are used in a          */
     /* compatible way wrt floating point operands.                           */
{
  int32 disp = (fpdouble ?
		litD(fc->floatbin.db.msd, fc->floatbin.db.lsd, fc->floatstr) :
		litE(fc->floatbin.fb.val, fc->floatstr));
  outcodexref (mop | F_RD(r1r), litlab, disp, xr_code,
	       reads_(R_BUS2,0,0,0),
	       writes_(r1r, (fpdouble?r1r+1:0), OP_LOAD|OP_FLOAT));
}

static void fpaddr(bool fpdouble, RealRegister r1r, FloatCon *fc)
     /* take the address of a floating point literal                          */
     /* only used #if SOFTWARE_FLOATING_POINT                                 */
{
  syserr("fpaddr");		/* /* ?? */
    /* fpload(OP_OR, r1r, fpdouble, fc);
       scc_regs &= ~regbit(r1r); */
}

static void imm_op(int32 op, RealRegister r1, RealRegister r2, int32 n)
{ /* r1 = r2 <op> n */
  int32 writes;
  if (n == -SIMM13_MIN)		/* change to r2 <op'> (-n) if possible */
    switch (op) {
    case OP_ADD: case OP_SUB:
      op ^= OP_ADD^OP_SUB;
      n = -n;
      break;
      /* not used
       *  case OP_ADDX: case OP_SUBX:
       *    op ^= OP_ADDX^OP_SUBX;
       *    n = -n;
       *    break;
       */
    case OP_ADDCC: case OP_SUBCC:
      op ^= OP_ADDCC^OP_SUBCC;
      n = -n;
      break;
      /* not used
       *   case OP_ADDXCC: case OP_SUBXCC:
       *    op ^= OP_ADDXCC^OP_SUBXCC;
       *    n = -n;
       */
    }
  switch (op) {
  case OP_ADDCC: case OP_ANDCC: case OP_ORCC: case OP_XORCC:
  case OP_SUBCC: case OP_ANDNCC: case OP_ORNCC: case OP_XNORCC:
    /*  case OP_ADDXCC: case OP_SUBXCC: */
    writes = writes_(r1,R_CC,OP_NULL);
    break;
  case OP_SAVE:
    writes = writes_(map_intreg(r1),0,OP_NULL);
    break;
  case OP_ADD:			/* special for correct_stack only */
    if (r1==R_SP) { writes = writes_(R_SP,R_FP,OP_NULL); break; }
    /* ... prevents swapping of FP and SP instructions */
    /* else drop through */
  default:
    writes = writes_(r1,0,OP_NULL);
  }
  if (SIMM13(n)) outP32(op | F_RD(r1) | F_RS1(r2) | F_SIMM13(n),
			reads_(r2,0,0,0), writes);
  else {
    load_integer(R_TM, n);
    outP32(op | F_RD(r1) | F_RS1(r2) | F_RS2(R_TM),
	   reads_(r2,R_TM,0,0), writes);
  }
  switch (op) {
  case OP_ADDCC: case OP_ANDCC: case OP_ORCC: case OP_XORCC:
  case OP_SUBCC: case OP_ANDNCC: case OP_ORNCC: case OP_XNORCC:
    /*  case OP_ADDXCC: case OP_SUBXCC: */
    scc_regs = regbit(r1);
    break;
  default:
    scc_regs &= ~regbit(r1);
  }
}

static void correct_stack(enum Stack_Correction c)
{
  int32 move = stack_move&(-8); /* keep stack double-word aligned */
  if (c == EXACT && move != 0 || c == INWINDOW && move < stack_safelimit)
  { imm_op(OP_ADD,R_SP,R_SP,move);
    stack_move = stack_move - move; /* = 0 or 4 if EXACT */
  }
}

static void compare_integer(RealRegister r, int32 n, int32 test)
     /* Compare register r with the integer n.                               */
{
  /*  >>correct_stack(EXACT);<<  has been done by caller.               */
  if (n==0)
    { if (!((test==Q_EQ || test==Q_NE || test==Q_UEQ || test==Q_UNE) &&
	    (scc_regs&regbit(r))))
	{ /* Not already in condition codes */
	  outP32 (OP_SUBCC | F_RD(R_ZERO) | F_RS1(r) | F_RS2(R_ZERO),
		  reads_(r,0,0,0), writes_(0,R_CC,OP_NULL));
	  scc_regs = regbit(r);
	}
    }
  else {
    imm_op (OP_SUBCC, R_ZERO, r, n);
    scc_regs = 0;
  }
}

static void sparc_shift(int32 rop, int32 r1r, int32 r2r, int32 m)
{ /* if m>0, r1r = r2r << m
     if m<0, r1r = r2r >> -m, with '>>' = rop (OP_SRL or OP_SRA)
     (or rop=OP_UNIMP if known to be left-shift only) */
  
  if (m == 0) move_register (r1r, r2r);
  else if (m == 1) {
    outP32(OP_ADDCC | F_RD(r1r) | F_RS1(r2r) | F_RS2(r2r),
	   reads_(r2r,0,0,0), writes_(r1r,R_CC,OP_NULL));
    scc_regs = regbit(r1r);
  }
  else {
    int32 op;
    if (m<0) { m=-m; op=rop; }
    else op = OP_SLL;
    if (m>=32) move_register (r1r, R_ZERO);
    else {
      outP32(op | F_RD(r1r) | F_RS1(r2r) | F_SIMM13(m),
	     reads_(r2r,0,0,0), writes_(r1r,0,OP_NULL));
      scc_regs &= ~regbit(r1r);
    }
  }
}

static int32 mulk_unfactored_cost (RealRegister r1, RealRegister r2, int32 m)
{
  int32 i, cost, part_res = R_ZERO;
  if (m==0) cost = 1;
  else if (m==1) cost = (r1=r2? 0: 1);
  else if (bitcount(m)==1) cost = 1;
  else if (m==-1) cost = 1;
  else if (bitcount(-m)==1) cost = 2;
  else {
    cost = 0;
    for (i=0; m!= 0; i++)
      if ((((unsigned32)m)>>i) & 1) {
	if (i==0) part_res = r2;
	else if (part_res == R_ZERO) part_res = r1, cost++; /* shift */
	else cost += 2;		/* shift, add/subtract */
	if ((((unsigned32)m)>>i) & 2) m += 1L<<i;
	else m -= 1L<<i;
      }
  }
  return cost;
}

static int32 mulk_factored_cost (RealRegister r1, RealRegister r2, int32 m)
{
  int32 i, cost;
  if (m==0) cost = 1;
  else if (m==1) cost = (r1=r2? 0: 1);
  else if (bitcount(m)==1) cost = 1;
  else if (m==-1) cost = 1;
  else if (bitcount(-m)==1) cost = 2;
  else {
    cost = mulk_unfactored_cost (r1, r2, m);
    for (i=31; i>0; i--) {
      /* find the largest factor 2^n+1 or 2^n-1 of m which reduces the cost */
      int32 j, divisor;
      for (j=0, divisor=(1L<<i)+1L; j<=1; j++, divisor-=2)
	if (divisor != 1 && divisor < m && (m%divisor)==0) {
	  int32 cost2 = 2 + mulk_factored_cost (r1, r2, m/divisor);
	  if (cost2 < cost) return cost2;
	}
    }
  }
  return cost;
}

static void mulk (RealRegister r1, RealRegister r2, int32 m)
{
  /* r1 = r2 * m */
  /* uses the following algorithm - with a few tweaks
   * R_TM   = r2
   * r1     = 0
   * mshift = 0
   * for (i=0; m!=0; i++) switch ((m>>i)&3)
   * { case 0: case 2: break;
   *   case 1: R_TM = R_TM<<(i-mshift); r1 = r1+R_TM;
   *           mshift = i; m = m-(1<<i); break;
   *   case 3: R_TM = R_TM<<(i-mshift); r1 = r1-R_TM;
   *           mshift = i; m = m+(1<<i); break;
   * }
   */
  
  int32 mshift = 0,
  i,
  shift_reg = r2,
  part_res = R_ZERO,
  sign_wrong = 0;
  
#define add_(x1,x2,x3) outP32(OP_ADDCC | F_RD(x1) | F_RS1(x2) | F_RS2(x3), \
			      reads_((x2),(x3),0,0),                       \
			      writes_((x1),R_CC,OP_NULL)),                 \
				scc_regs = regbit(x1)
#define sub_(x1,x2,x3) outP32(OP_SUBCC | F_RD(x1) | F_RS1(x2) | F_RS2(x3), \
			       reads_((x2),(x3),0,0),                       \
			       writes_((x1),R_CC,OP_NULL)),                 \
			scc_regs = regbit(x1)

   /* the following special cases are detected in cg.c (with the exception of
      bitcount(-m)=1,m!=-1), but are included for completeness and because
      there is a possibility they may be introduced by attempting to factorise
      and invoke mulk recursively */

   if (m==0) move_register(r1,R_ZERO);
   else if (bitcount(m)==1) sparc_shift(OP_UNIMP, r1, r2, firstbit(m));
   else if (m==-1) sub_(r1,R_ZERO,r2);
   else if (bitcount(-m)==1) {
     sparc_shift(OP_UNIMP, r1, r2, firstbit(-m));
     sub_(r1,R_ZERO,r1);
   }
   else {
     /* try to find a factor 2^n+1 or 2^n-1 which reduces the cost */
     for (i=31; i>0; i--) {
       int32 j, divisor, quotient, remainder,
       unfactored_cost = mulk_unfactored_cost (r1, r2, m);
       for (j=0, divisor=(1L<<i)+1; j<=1; j++, divisor-=2) {
	 if (divisor==1 || divisor >= m) continue;
	 quotient = m/divisor, remainder = m%divisor;
	 if (remainder==0 &&
	     (2+mulk_factored_cost(r1,r2,quotient)) < unfactored_cost) {
	   mulk (r1, r2, quotient);
	   sparc_shift (OP_UNIMP, R_TM, r1, i);
	   if (j==0) add_(r1,r1,R_TM);
	   else sub_(r1,R_TM,r1);
	   return;
	 }
       }
     }
     
     for (i=0; m!=0; i++)
       switch ((((unsigned32)m)>>i)&3) {
	 
       case 0:
       case 2:
	 break;
	 
       case 1:
	 if (i==0) part_res = r2;
	 else if (part_res==R_ZERO) {
	   sparc_shift(OP_UNIMP, r1, shift_reg, i-mshift);
	   shift_reg = r1;
	   part_res = r1;
	 }
	 else {
	   sparc_shift(OP_UNIMP, R_TM, shift_reg, i-mshift);
	   shift_reg = R_TM;
	   if (sign_wrong) {
	     sub_(r1, shift_reg, part_res);
	     sign_wrong = 0;
	   }
	   else add_(r1, shift_reg, part_res);
	   part_res = r1;
	 }
	 mshift = i;
	 m -= (1L<<i);
	 break;
	 
       case 3:
	 if (i==0) { part_res = r2; sign_wrong = 1; }
	 else if (part_res==R_ZERO) {
	   sparc_shift(OP_UNIMP, r1, shift_reg, i-mshift);
	   shift_reg = r1;
	   part_res = r1;
	   sign_wrong = 1;
	 }
	 else {
	   sparc_shift(OP_UNIMP, R_TM, shift_reg, i-mshift);
	   shift_reg = R_TM;
	   if (sign_wrong) add_(r1, shift_reg, part_res);
	   else sub_(r1, part_res, shift_reg);
	   part_res = r1;
	 }
	 mshift = i;
	 m += (1L<<i);
       }
   }

#undef add_
#undef sub_
}

static void argpush(int32 n, int32 off)
{
  /* Push arg registers at R_FP+off upwards. Called after window roll,
     so use R_P1 (incoming params) and not R_A1 (outgoing args).
     Wrong if window not rolled */
  off += 4*n;
  while (n)
  { if (n&1 || off&4)
    { off -= 4; n--;
      outrelref(OP_ST, R_P1+n, R_FP, off,
                reads_(R_FP, R_P1+n, 0, 0), writes_(R_BUS4,0,OP_STORE));
    }
    else
    { off -= 8; n -= 2;
      outrelref(OP_STD, R_P1+n, R_FP, off,
                reads_(R_FP, R_P1+n, R_P1+n+1, 0),
		writes_(R_BUS4,0,OP_STORE));
    }
  }
}

static void save_iregs (int32 mask, int32 off)
{
  /* first look for even-odd register pairs, and use STD for them
     (as we know/require that off is double-word aligned)         */
  {
    int32 i, r1r2 = 3;
    for (i=2; i<32; i+=2) { /* need not try 0-1 !
			       could reduce range further by thinking
			       about what bits can possibly be in mask */
      r1r2 <<= 2;
      if ((mask & r1r2) == r1r2) {
	int32 r1r = map_intreg(i);
	outrelref(OP_STD, r1r, R_SP, off,
		  reads_(R_SP,r1r,r1r+1,0),
		  writes_(R_BUS4,0,OP_STORE));
	mask ^= r1r2;
	off +=8;
      }
    }
  }
  while (mask) {
    int32 r1 = firstbit(mask);
    int32 r1r = map_intreg(r1);
    outrelref(OP_ST, r1r, R_SP, off,
	      reads_(R_SP,r1r,0,0), writes_(R_BUS4,0,OP_STORE));
    mask ^= 1L<<r1;
    off +=4;
  }
}

static void restore_iregs (int32 mask, int32 off)
{
  /* first look for even-odd register pairs, and use LDD for them
     (as we know/require that off is double-word aligned)         */
  {
    int32 i, r1r2 = 3;
    for (i=2; i<32; i+=2) { /* need not try 0-1 ! */
      r1r2 <<= 2;
      if ((mask & r1r2) == r1r2) {
	int32 r1r = map_intreg(i);
	outrelref(OP_LDD, r1r, R_SP, off,
		  reads_(R_SP,R_BUS2,0,0), writes_(r1r,r1r+1,OP_LOAD));
	mask ^= r1r2;
	off +=8;
      }
    }
  }
  while (mask) {
    int32 r1 = firstbit(mask);
    int32 r1r = map_intreg(r1);
    outrelref(OP_LD, r1r, R_SP, off,
	      reads_(R_SP,R_BUS2,0,0), writes_(r1r,0,OP_LOAD));
    mask ^= 1L<<r1;
    off +=4;
  }
}

static void save_fregs (int32 fmask, int32 base, int32 off)
{
  while (fmask) {
    int32 r1 = firstbit(fmask);
    int32 r1r = R_F0 + 2*r1;
    outrelref(OP_STDF, r1r, base, off,
	      reads_(r1r,r1r+1,base,0),
	      writes_(R_BUS4,0,OP_STORE|OP_FLOAT));
    fmask ^= 1L<<r1;
    off +=8;
  }
}

static void restore_fregs (int32 fmask, int32 base, int32 off)
{
  while (fmask) {
    int32 r1 = firstbit(fmask);
    int32 r1r = R_F0 + 2*r1;
    outrelref(OP_LDDF, r1r, base, off,
	      reads_(base,R_BUS2,0,0), writes_(r1r,r1r+1,OP_LOAD|OP_FLOAT));
    fmask ^= 1L<<r1;
    off +=8;
  }
}

static void routine_entry(int32 m)
{   int32 n = (m>NARGREGS ? NARGREGS : m); /* regs to be pushed if nec. */
/* Note that M_ARGREGS on the next line refers to %o0-%o5, which of     */
/* course need to be saved if the try to avoid rolling the stack...     */
    int32 mask = regmaskvec.map[0] & (M_VARREGS | M_ARGREGS);
    int32 fmask = regmaskvec.map[1] & M_FVARREGS;
    bool decided_to_roll =    /* /* for now ... */
      (procflags & (BLK0EXIT | NONLEAF)? 1: (bitcount(mask) > 6));
    fptemp = procflags & PROC_FPTEMP ? 8 : 0;
/*        fprintf(stderr,"entry procflags=%lx\n"\
 *                       "      mask=%lx fmask=%lx\n"\
 *                       "      greatest_stackdepth=%lx\n",
 *		procflags, mask, fmask, greatest_stackdepth);
 */
    mc_peeps = NULL;
    fp_minus_sp = 0;
    double_lit = 3;		/* Single alignment is sufficient (?) */
    returnlab = nextlabel();
    nargwords = m;

    state.bus_count = state.fpu = 0;
    start_basic_block (NULL);
    current_bb->refcount++;
    
    if (decided_to_roll)
    {   if (procflags & PROC_ARGPUSH)
        {
#ifdef NEVER
/* The following code can be used to place the args at nicer positions  */
/* (e.g. to allow STD to be used to store them).  Displeases debuggers? */
            if (m <= NARGREGS)
            {   firstargoff = -padsize(4*n, 8);
                firstlocoff = firstargoff - 8*bitcount(fmask);
            }
            else
#endif
            {   firstargoff = WINDOW_ARG1;
                firstlocoff = -8*bitcount(fmask);
            }
        }
        else
        {   firstargoff = WINDOW_ARG1;
            /* relevant only if m>NARGREGS, else all args stay in registers */
            firstlocoff = -8*bitcount(fmask);
        }
        firstlocoff -= fptemp;

        /* stack still double-aligned - now roll window (or save VAR regs) */
        /* /* can the SAVE be avoided in LEAF ?? */
        imm_op(OP_SAVE, R_SP, R_SP, firstlocoff - WINDOW_SIZE_D);
	end_basic_block (NULL, 0, END_DROP); /* /* 'cos SAVE is complicated!!  */
        window_rolled = 1;
        stack_move = 0;
        if (procflags & PROC_ARGPUSH) argpush(n,firstargoff);
        save_fregs(fmask, R_FP, firstlocoff+fptemp);
    }
    else { /* decided not to roll */
/* AM wonders about the following code.  Since the expected number of   */
/* stores per SAVE is quite small (1.2 regs?), it seems rather over-    */
/* keen to save %i0-%i7 instead of rolling.  JPFF observes that this    */
/* does indeed save time.  Note that the regs are numbered 'incorrectly'*/
/* because we haven't rolled.  Hence we only save V-regs (%l) and       */
/* A-regs (%o regs, unusable before roll!)                              */
      int32 savesize = padsize(4*bitcount(mask),8) + 8*bitcount(fmask) + fptemp;
      window_rolled = 0;
/* The next line reuses arg vector space in leaf proc (args in regs).   */
/* We could also use this trick to avoid %sp moves to alloc locals!!    */
      explicit_savesize =
                    (savesize <= 4*NARGREGS) ? 0 : savesize - 4*NARGREGS;
      stack_safelimit = INWINDOW - explicit_savesize;
      if (stack_safelimit > 0) stack_safelimit = 0;
      stack_move = -explicit_savesize;
      if (stack_move<0) correct_stack(INWINDOW);

/* The following two values are w.r.t. fp_minus_sp==0.                   */
      firstargoff = WINDOW_ARG1 + explicit_savesize;
      firstlocoff = WINDOW_ARG0;
      save_iregs(mask, firstlocoff + fptemp + 8*bitcount(fmask));
      save_fregs(fmask, R_SP, firstlocoff + fptemp) ;
    }
    
    scc_regs = 0;
    adconpend = 0;
    cmp_pending = 0;
    fcmp_pending = 0;
  }

static void routine_exit(bool returning)
{
  int32 mask  = regmaskvec.map[0] & (M_VARREGS | M_ARGREGS),
  fmask = regmaskvec.map[1] & M_FVARREGS;
  
  /* integer registers are restored either by rolling the window,
     or by explicit loads. */
  
  if (window_rolled) {
    /* reload float registers as necessary */
    restore_fregs(fmask, R_FP, firstlocoff+fptemp);
    
#ifdef NEVER
/* see corresponding #ifdef NEVER in routine_entry...                   */
    if ((procflags & PROC_ARGPUSH) && (nargwords > NARGREGS)) 
      /* recover the extra space allocated for the pushed register args */
      imm_op (OP_ADD, R_SP, R_SP, stack_move&(-8)+fp_minus_sp+
                                  WINDOW_SIZE+8*bitcount(fmask)+4*NARGREGS);
#endif
    outrestore();
    if (returning) {
      mcpeepdata *mc_peep1 = new_mcpeep ();
      mc_peep1->type = OUT_INSTR;
      mc_peep1->w = OP_RETL;
      mc_peep1->uses.reads.iregs = regbit(R_LR1);
      mc_peep1->uses.opclass = OP_NULL;
      end_basic_block(mc_peep1, 1, END_JMPL);
      /* outP32(OP_RETL, reads_(R_LR1,0,0,0), writes_(0,0,OP_FLUSH)); */
    }
    fp_minus_sp = 0;
    stack_move = 0;
  }
  else {
    /* window not rolled. Restore int ARG and VAR regs, float VAR regs,
       and stack pointer */
    int32 stack_reset = stack_move + fp_minus_sp + explicit_savesize;
    int32 offset = stack_move + fp_minus_sp + firstlocoff + fptemp;

    restore_fregs(fmask, R_SP, offset);
    restore_iregs(mask, offset + 8*bitcount(fmask));
    
    if (stack_reset) imm_op (OP_ADD, R_SP, R_SP, stack_reset);
    
    if (returning) {
      mcpeepdata *mc_peep1 = new_mcpeep ();
      mc_peep1->type = OUT_INSTR;
      mc_peep1->w = OP_RETL;
      mc_peep1->uses.reads.iregs = regbit(R_LR1);
      mc_peep1->uses.opclass = OP_NULL;
      end_basic_block(mc_peep1, 1, END_JMPL);
    }
  }
  flush_peepholer();
}

/* AM: treat the special value RETLAB of destination as return address */
static void conditional_branch_to(int32 condition, int32 fcmp,
				  LabelNumber *destination)
{
  int32 op, endtype;
  mcpeepdata *mc_peep1;
  if (destination == RETLAB)
    {   destination = returnlab;
	if (condition == Q_AL)
	   { /* if get an unconditional return expand it inline */
	     /* and save its address if it was the first        */
	     /* - but ACN suggests we may get better code by not  */
	     /* doing so, waiting instead until we are at ENDPROC */
	     /* if (!lab_isset_(destination)) setlabel(destination); */
	     routine_exit(1);
	     return;
	   }
      }
  mc_peep1 = new_mcpeep ();
  mc_peep1->type = OUT_LABREF;
  if (fcmp && condition != Q_AL) {
    op = OP_BF;
    mc_peep1->uses.reads.fcc = 1;
    mc_peep1->uses.fpu = 1;
    mc_peep1->uses.opclass = OP_FLOAT;
    endtype = END_BCC;
  }
  else {
    op = OP_B;
    if (condition==Q_AL) endtype = END_BA;
    else {
      endtype=END_BCC;
      mc_peep1->uses.reads.cc = 1;
    }
    mc_peep1->uses.opclass = OP_NULL;
  }
  mc_peep1->w = op | C_FROMQ(condition, fcmp) | 0;
  /* This is where I need to discriminate between the two sorts of things  */
  /* that I put in the ->frefs field of a label entry.                     */
  /* u.defn shares with u.frefs */
  /* - except that, since I am now peepholing, all references need to be   */
  /* treated as forward, and sorted out when finally emitted.              */
  
  mc_peep1->lab = destination;
  mc_peep1->reftype = LABREF_DISP22;
  end_basic_block (mc_peep1, 1, endtype);
}

void flush_adcon(void)
{
  if (adconpend) {
    Symstr *name = (Symstr *)adm;
    int32 offset = (int32)adr2;
    outRext(OP_OR | F_RD(adr1), name, offset, xr_data,
	    reads_(0,0,0,0), writes_(adr1,0,OP_NULL));
    scc_regs = scc_regs&(~regbit(adr1));
    adconpend = 0;
  }
}

static int32 movc_small(int32 ld, int32 st, RealRegister r1r, RealRegister r2r,
                        int32 m, int32 q, bool all)
{   while (all ? m>=q : m&q)
    {   m -= q;
        outrelref(ld, R_TM, r2r, m,
		  reads_(r2r,R_BUS2,0,0), writes_(R_TM,0,OP_LOAD));
	outrelref(st, R_TM, r1r, m,
		  reads_(R_TM,r1r,0,0), writes_(R_BUS4,0,OP_STORE));
    }
    return m;
}

static int32 movc_chunk(int32 ld, int32 st, RealRegister r1r, RealRegister r2r,
                        int32 m, int32 q, int32 tmp, bool all)
{   if (all ? m>=q : m&q)
    {   LabelNumber *tt = NULL;
        if (all && m>=2*q) start_basic_block(tt = nextlabel());
        imm_op(OP_SUBCC, tmp, tmp, q);
        outP32(ld | F_RD(R_TM) | F_RS1(r2r) | F_RS2(tmp),
               reads_(r2r,tmp,R_BUS2,0), writes_(R_TM,0,OP_LOAD));
        outP32(st | F_RD(R_TM) | F_RS1(r1r) | F_RS2(tmp),
               reads_(R_TM,r1r,tmp,0), writes_(R_BUS4,0,OP_STORE));
        if (tt) conditional_branch_to(Q_GT, 0, tt);
    }
    return all ? (m & q-1) : (m & ~q); 
}

void show_instruction(J_OPCODE op, VRegInt vr1, VRegInt vr2, VRegInt vm)
     /* The types of the arguments here are rather unsatisfactory - in        */
     /* particular the last one (m) is really a big union.                    */
{
  RealRegister r1 = vr1.r, r2 = vr2.r;
  int32 m = vm.i;
  RealRegister r1r = r1, r2r = r2, mr = m;
  int32 opm;
  int32 dead;
  /* Is this the way to do it?  Concern over code quality - the compiler
   * clearly needs to know how to put union values of size 4 in registers.
   *  union { Symstr *sym; int32 umint; } um;
   *  um.umint = m;
   */
  if (uses_r1(op))
    {
      if (r1r >= NMAGICREGS) syserr(syserr_r1r, (long)r1r); else
	if (r1r >= R_F0) r1r = 2*r1r - 32; else    /* flt reg */
	  if (!window_rolled) r1r = map_intreg(r1r);
    }
  if (uses_r2(op))
    {
      if (r2r >= NMAGICREGS) syserr(syserr_r2r, (long)r2r); else
	if (r2r >= R_F0) r2r = 2*r2r - 32; else    /* flt reg */
	  if (!window_rolled) r2r = map_intreg(r2r);
    }
  if (uses_r3(op))
    {
      if (mr >= NMAGICREGS) syserr(syserr_mr, (long)mr); else
	if (mr >= R_F0) mr = 2*mr - 32; else        /* flt reg */
	  if (!window_rolled) mr = map_intreg(mr);
    }
  if (debugging(DEBUG_CG))
    {   int r1dead = (op&J_DEAD_R1?'#':' ');
	int r2dead = (op&J_DEAD_R2?'#':' ');
	int r3dead = (op&J_DEAD_R3?'#':' ');
	cc_msg("GEN: ");
	jopprint_opname(op);
	cc_msg("%ld%c %ld%c %ld%c\n", (long)r1r, r1dead,
	       (long)r2r, r2dead,
	       (long)mr,  r3dead);
      }
  /* to enable future JOPCODE peephole optimisation expand_jop_macros()
     tries quite hard not to call show_inst() with instructions with
     no effect.
     */
  
  dead = op&J_DEADBITS;	/* Keep just in case */
  op &= ~J_DEADBITS;                         /* ignore as yet */
  opm = op&~(Q_MASK|J_ALIGNMENT);
  
  /* /* lash-up to solve the confusion between integer and float condition
     codes ... */
    if (cmp_pending) {
      if (opm != J_B) cmp_pending = 0, fcmp_pending = 0;
    }
    else if (opm == J_B && (op & Q_MASK) != Q_AL) {
      syserr("gen.c(branch %.8lx without compare)", (long)op);
    }

/* Stack optimisation code */
    if ((r1r==R_SP) || (r2r==R_SP)) {
      if ((opm==J_ADDK) && (r1r==r2r)) {
        stack_move += m;
        if (stack_move<0) correct_stack(INWINDOW);
        return;
      }
      else if ((opm==J_SUBK) && (r1r==r2r)) {
        stack_move -= m;
        if (stack_move<0) correct_stack(INWINDOW);
        return;
      }
      else if ((uses_r1(op) && r1r==R_SP) ||
               (uses_r2(op) && r2r==R_SP) ||
               (uses_r3(op) && mr ==R_SP))
        correct_stack(EXACT);   /* EXACT is safe, but maybe overkeen? */
    }
    else if ((stack_move<0) &&
             ((uses_r1(op) && r1r==R_FP) ||
              (uses_r2(op) && r2r==R_FP) ||
              (uses_r3(op) && mr ==R_FP)))
      correct_stack(INWINDOW);


    if (adconpend &&
        (opm != J_LDRK) && (opm != J_STRK) &&
        ((opm & ~(J_SIGNED|J_UNSIGNED)) != J_LDRBK) && (opm != J_STRBK) &&
        ((opm & ~(J_SIGNED|J_UNSIGNED)) != J_LDRWK) && (opm != J_STRWK))
      flush_adcon();

    switch(opm)
    {
case J_CLRC:
#ifdef never
       /* (see J_MOVC).  Except that alignof_toplevel guarantees.      */
       if ((m&1) != 0) {	/* An odd byte at end */
	 m--;
	 outrelref(OP_STB, R_ZERO, r1r, m,
		   reads_(r1r,R_ZERO,0,0), writes_(R_BUS4,0,OP_STORE));
       }
       if ((m&3) != 0) {	/* An odd halfword at end */
	 m -= 2;
	 outrelref(OP_STH, R_ZERO, r1r, m,
		   reads_(r1r,R_ZERO,0,0), writes_(R_BUS4,0,OP_STORE));
       }
#else
       if (m&3) syserr("CLRC alignment");
#endif
       if (m<=16) {
	 int32 n;
	 for (n=0; n<m; n += 4)
	   outrelref(OP_ST, R_ZERO, r1r, n,
		     reads_(r1r,R_ZERO,0,0), writes_(R_BUS4,0,OP_STORE));
       }
       else {
	 LabelNumber *tt = nextlabel();
	 load_integer(R_TM,m);
	 start_basic_block(tt);
	 imm_op (OP_SUBCC, R_TM, R_TM, 4);
	 outP32(OP_ST | F_RD(R_ZERO) | F_RS1(r1r) | F_RS2(R_TM),
		reads_(R_TM,r1r,R_ZERO,0), writes_(R_BUS4,0,OP_STORE));
	 conditional_branch_to(Q_GT, 0, tt);
       }
       return;
case J_MOVC:
       if (j_aligned(op,J_ALIGN4) && m<=12 && m!=11 ||
           j_aligned(op,J_ALIGN2) && m<=6 || m<=3) {
	 /* do up to three load/store pairs.				*/
         m = movc_small(OP_LDUB, OP_STB, r1r, r2r, m, 1,
                        !j_aligned(op, J_ALIGN2));
	 m = movc_small(OP_LDUH, OP_STH, r1r, r2r, m, 2, 
                        !j_aligned(op, J_ALIGN4));
	 m = movc_small(OP_LD,   OP_ST,  r1r, r2r, m, 4, 1);
	 if (m) syserr("gen.c(movc_small)");
       }
       else {
	 int32 tmp;
	 if (window_rolled) tmp = R_LR1;
	 else {
	   /* maybe roll/unroll is cheaper here?			*/
	   tmp = 2;
	   if (tmp==r1r || tmp==r2r) { /* cannot use 2 */
	     tmp = 3;
	     if (tmp==r1r || tmp==r2r) tmp = 4;
	   }
	   outfptempref(OP_ST, tmp, 0,
		        reads_(tmp,0,0,0), writes_(R_BUS4,0,OP_STORE));
	 }
	 load_integer(tmp,m);
	 m = movc_chunk(OP_LDUB, OP_STB, r1r, r2r, m, 1, tmp,
                        !j_aligned(op, J_ALIGN2));
	 m = movc_chunk(OP_LDUH, OP_STH, r1r, r2r, m, 2, tmp,
                        !j_aligned(op, J_ALIGN4));
	 m = movc_chunk(OP_LD,   OP_ST,  r1r, r2r, m, 4, tmp, 1);
	 if (m) syserr("gen.c(movc_chunk)");
	 /* restore things */
	 if (!window_rolled)
	   outfptempref(OP_LD, tmp, 0,
		        reads_(R_BUS2,0,0,0), writes_(tmp,0,OP_LOAD));
       }
       return;
case J_OPSYSK:
       {
	 LabelNumber * tt = nextlabel();
	 load_integer(1,m);	/* Set %g1 with trap number */
	 flush_peepholer();
	 outP32(OP_TICC | C_AL | A_IMM | SYSTRAP,
		reads_(1,R_A1,R_A1+1,R_A1+2), writes_(R_CC,0,OP_NULL));
	 flush_peepholer();
	 conditional_branch_to(Q_HS, 0, tt);
	 outRext(OP_ST | F_RD(R_A1), targeterrno, 0, xr_data,
	            reads_(R_A1,0,0,0), writes_(R_BUS4,0,OP_STORE));
	 load_integer(R_A1, -1);
	 start_basic_block(tt);
	 scc_regs = 0;
       }
       return;
case J_CALLK:
        correct_stack(EXACT);
        if (stack_move) syserr("CALLK stack mis-aligned\n");
        outcall((Symstr *)m);
        scc_regs = 0;
        return;
case J_TAILCALLK:
       if (window_rolled) {
	 /* /* need to move the outgoing args from R_A1.. to R_P1..;
	    they are in the right place for an ordinary call, but in a
	    tailcall we need to unroll the window before jumping to the
	    target, and this would lose them */
	 /* r2 holds the number of args for the call */
	 int i,
	     rr = (int)(r2<= NARGREGS? r2: NARGREGS);
	 for (i = 0; i < rr; i++)
	   outP32(OP_OR | F_RD(R_P1+i) | F_RS1(R_A1+i) | F_RS2(R_ZERO),
		  reads_(R_A1+i,0,0,0), writes_(R_P1+i,0,OP_NULL));
       }
       else {
	 /* /* need to move the outgoing args from R_P1.. to R_A1..;
	    they would have been put in the right place, except for the
	    register mapping that we are doing because of the non-roll !! */
	 /* r2 holds the number of args for the call */
	 int i,
	     rr = (int)(r2<= NARGREGS? r2: NARGREGS);
	 for (i = 0; i < rr; i++)
	   outP32(OP_OR | F_RD(R_A1+i) | F_RS1(R_P1+i) | F_RS2(R_ZERO),
		  reads_(R_P1+i,0,0,0), writes_(R_A1+i,0,OP_NULL));
       }
       routine_exit(0);   /* effectively includes correct_stack() */

       if (stack_move) syserr("TAILCALLK stack mis-aligned\n");
       outRext_ebb(OP_JMPL | F_RD(R_ZERO), (Symstr *)m, 0, xr_code, END_JMPL);
       scc_regs = 0;
       return;
case J_CALLR:
       correct_stack(EXACT);
       if (stack_move) syserr("CALLR stack mis-aligned\n");
       { mcpeepdata *mc_peep1 = new_mcpeep ();
	 mc_peep1->type = OUT_INSTR;
	 mc_peep1->w = OP_JMPL | F_RD(R_LR1) | F_RS1(mr) | F_RS2(R_ZERO);
	 mc_peep1->uses.reads.iregs = regbit(mr);
	 mc_peep1->uses.writes.iregs = regbit(R_LR1);
	 mc_peep1->uses.opclass = OP_NULL;
	 end_basic_block(mc_peep1, 1, END_CALL); /* nb *not* END_JMPL */
	 scc_regs = 0;
       }
       return;
#ifdef never
/* this code is tentative: mr cannot be anything restored by routine_exit() */
case J_TAILCALLR:
        /* /* need to move the outgoing args from R_P1.. to R_A1..;
	   they are in the right place for an ordinary call, but in a tailcall
           we need to unroll the window before jumping to the target,
           and this would lose them */
	{
	  /*  /* relies on r2 holding the number of args for the call */
	  int i,
	      rr = (int)(r2<= NARGREGS? r2: NARGREGS);
	  for (i = 0; i < rr; i++)
	    outP32(OP_OR | F_RD(R_A1+i) | F_RS1(R_P1+i) | F_RS2(R_ZERO),
		   reads_(R_P1+i,0,0,0), writes_(R_A1+i,0,OP_NULL));
        }
	routine_exit(0);   /* effectively includes correct_stack(EXACT) */

       { mcpeepdata *mc_peep1 = new_mcpeep ();
	 mc_peep1->type = OUT_INSTR;
	 mc_peep1->w = OP_JMPL | F_RD(R_ZERO) | F_RS1(mr) | F_RS2(R_ZERO);
	 mc_peep1->uses.reads.iregs = regbit(mr);
	 mc_peep1->uses.opclass = OP_NULL;
	 end_basic_block(mc_peep1, 1, END_JMPL);
       }
       scc_regs = 0;
       break;
#endif
case J_COUNT:
        correct_stack(EXACT);
/* (int)r1 is ? (I would like the character on the line) ????              */
/* (char *)r2 is the name of the file, and (int)m is the line number       */
/* within that file. I will assume here a sensible limit on the length     */
/* of files and hence pack these two pieces of information into a single   */
/* 32-bit word. The structure used is count_position, and up to 16 files   */
/* can be referenced. If there is any danger of running out of same I will */
/* flush out the table used to decode files names and start again.         */
       { count_position k;
	 mcpeepdata *mc_peep = new_mcpeep ();
	 /* beware that the next line may flush literals etc. */
	 k.s.file = lit_of_count_name((char *)r2);
	 k.s.line = (unsigned int)m;
	 k.s.posn = 0;       /* Not available here */
	 outcall(countroutine);
	 mc_peep->type = OUT_COUNT;
	 mc_peep->w = k.i;
	 end_basic_block (mc_peep, 0, END_DROP);
	 /* ensure count data follows call, and next block is referenced */
       }
       scc_regs = 0;
       return;
case J_ADCON:
        adr1 = r1r; adr2 = r2; adm = m; adconpend = 1;
        return;
case J_STRING:
        /* BEWARE - the following code may fail on literal pool overflow */
	outcodexref (OP_OR | F_RD(r1r), litlab, 4*litpoolp, xr_code,
		     0, writes_(r1r,0,OP_NULL));
        scc_regs = scc_regs&(~regbit(r1r));
        codeseg_stringsegs((StringSegList *)m, 1);
        return;
case J_B:
        correct_stack(EXACT);
	conditional_branch_to(op & Q_MASK, fcmp_pending, (LabelNumber *)m);
	fcmp_used = fcmp_pending;
        cmp_pending = 0;
        return;
case J_CASEBRANCH:
        /* defer any action until the first BXX, when the default label
	   becomes known */
        casebranch_pending = m;
        casebranch_r1r = r1r;
        return;
case J_BXX:             /* Used with case tables */
        if (casebranch_pending) { /* first BXX */
	  LabelNumber *tablelab = nextlabel();
	  r1r = casebranch_r1r;
	  correct_stack(EXACT);
	  compare_integer(r1r, casebranch_pending-1,Q_HS);
	       /* 1 for default */
	  sparc_shift(OP_UNIMP, r1r, r1r, 2);
	  /* nb shift is not *needed* if we take the branch, but does no harm;
	     nbb it must not set R_CC (ok as shift is 2!) */
	  conditional_branch_to(Q_HS, 0, (LabelNumber *)m);
	  outcodexref (OP_OR | F_RD(R_TM), tablelab, 0, xr_code,
		       reads_(0,0,0,0), writes_(R_TM,0,OP_NULL));
	  outP32(OP_LD | F_RD(r1r) | F_RS1(r1r) | F_RS2(R_TM),
		 reads_(r1r,R_TM,R_BUS2,0), writes_(r1r,0,OP_LOAD));
	       /* load address from [tablelab+4*case-value] */
	  {
	    mcpeepdata *mc_peep1 = new_mcpeep ();
	    mc_peep1->type = OUT_INSTR;
	    mc_peep1->w = OP_JMPL | F_RD(R_ZERO) | F_RS1(r1r) | F_RS2(R_ZERO);
	    mc_peep1->uses.reads.iregs = regbit(r1r);
	    mc_peep1->uses.opclass = OP_NULL;
	    end_basic_block(mc_peep1, 1, END_JMPL);
	  }
	  start_basic_block(tablelab);
	  casebranch_pending = 0;
	}

	else {			/* not first BXX */
	  LabelNumber *l = (LabelNumber *)m;
	  mcpeepdata *mc_peep = new_mcpeep ();
	  /* peepholer is flushed already */
	  if (l==RETLAB) l = returnlab;	/* /* yuk!! ?? */
	  if (current_bb == NULL) start_basic_block (NULL);
	  current_bb->refcount = 1; /* implicit reference from CASEBRANCH */
	  mc_peep->type = OUT_CODELAB;
	  mc_peep->lab = l;
	  mc_peep->reftype = LABREF_ABS32;
	  end_basic_block (mc_peep, 0, END_BXX);
	  /* /* to ensure correct ordering ? */
	}
        return;
case J_LABEL:
	{
	  LabelNumber *l = (LabelNumber *)m;
	  if (l->block == DUFF_ADDR && l != returnlab)
	    syserr("Unused label L%ld\n", lab_name_(l)&0x7fffffff);
	  else {
	    correct_stack(EXACT);
	    start_basic_block(l);
	    scc_regs = 0;
	  }
	}
        return;
case J_STACK:
        fp_minus_sp = m;
        stack_move = fp_minus_sp & 4;
	return;
case J_SETSP:
        {   int32 diff, oldstack = (int32)r2, newstack = m;
            if (fp_minus_sp != oldstack)
                syserr("SETSP confused %ld!=%ld %ld",
                       (long)fp_minus_sp, (long)oldstack, (long)newstack);
            diff = newstack - oldstack;
            fp_minus_sp = newstack;
            stack_move -= diff;
        }
        return;

case J_CMPR:
        correct_stack(EXACT);
        outP32(OP_SUBCC | F_RD(R_ZERO) | F_RS1(r2r) | F_RS2(mr),
               reads_(r2r,mr,0,0), writes_(0,R_CC,OP_NULL));
        cmp_pending = 1;
        fcmp_pending = 0;
        scc_regs = 0;
        return;
case J_CMPK:
        correct_stack(EXACT);
        compare_integer(r2r, m, op & Q_MASK);
        cmp_pending = 1;
        fcmp_pending = 0;
        return;
case J_MOVR:
        if (r1r==mr) syserr(syserr_movr);
        move_register(r1r, mr);
        return;
case J_NEGR:
	outP32(OP_SUBCC | F_RD(r1r) | F_RS1(R_ZERO) | F_RS2(mr),
	       reads_(mr,0,0,0), writes_(r1r,R_CC,OP_NULL));
        scc_regs = regbit(r1r);
        return;
case J_NOTR:
	outP32(OP_XNORCC | F_RD(r1r) | F_RS1(mr) | F_RS2(R_ZERO),
	       reads_(mr,0,0,0), writes_(r1r,R_CC,OP_NULL));
        scc_regs = regbit(r1r);
        return;
case J_MOVK:
        load_integer(r1r, m);
        return;

case J_SHRK+J_SIGNED:
case J_SHRK: /*+J_UNSIGNED:*/
        m = -m;
	/* drop through */
case J_SHLK+J_SIGNED:
case J_SHLK: /*+J_UNSIGNED:*/
        sparc_shift((op & J_UNSIGNED ? OP_SRL:OP_SRA), r1r, r2r, m);
        return;

case J_SUBK:
        imm_op (OP_SUBCC, r1r, r2r, m);
        return;
case J_ADDK:
        imm_op (OP_ADDCC, r1r, r2r, m);
        return;

/* the front-end maps MULK to MULR if TARGET_LACKS_MULTIPLY_LITERALS,
   and DIVK/REMK to DIVR/REMR if TARGET_LACKS_DIVIDE_LITERALS.
   TARGET_LACKS_MULDIV_LITERALS is equivalent to setting both of these.

   MULR/DIVR/REMR get mapped to function calls unless TARGET_HAS_MULTIPLY,
   TARGET_HAS_DIVIDE etc. Beware also CONFIG_HAS_MULTIPLY in mcdep.c        */

case J_MULK:
        mulk (r1r, r2r, m);
        return;

case J_ANDK:
        imm_op (OP_ANDCC, r1r, r2r, m);
        return;
case J_ORRK:
        imm_op (OP_ORCC, r1r, r2r, m);
        return;
case J_EORK:
        imm_op (OP_XORCC, r1r, r2r, m);
        return;


case J_ADDR:
	outP32(OP_ADDCC | F_RD(r1r) | F_RS1(r2r) | F_RS2(mr),
	       reads_(r2r,mr,0,0), writes_(r1r, R_CC, OP_NULL));
	scc_regs = regbit(r1r);
        return;
case J_SUBR:
	outP32(OP_SUBCC | F_RD(r1r) | F_RS1(r2r) | F_RS2(mr),
	       reads_(r2r,mr,0,0), writes_(r1r, R_CC, OP_NULL));
        scc_regs = regbit(r1r);
        return;
case J_ANDR:
	outP32(OP_ANDCC | F_RD(r1r) | F_RS1(r2r) | F_RS2(mr),
	       reads_(r2r,mr,0,0), writes_(r1r, R_CC, OP_NULL));
        scc_regs = regbit(r1r);
        return;
case J_ORRR:
	outP32(OP_ORCC | F_RD(r1r) | F_RS1(r2r) | F_RS2(mr),
	       reads_(r2r,mr,0,0), writes_(r1r, R_CC, OP_NULL));
        scc_regs = regbit(r1r);
        return;
case J_EORR:
	outP32(OP_XORCC | F_RD(r1r) | F_RS1(r2r) | F_RS2(mr),
	       reads_(r2r,mr,0,0), writes_(r1r, R_CC, OP_NULL));
        scc_regs = regbit(r1r);
        return;
case J_SHLR+J_SIGNED:
case J_SHLR: /*+J_UNSIGNED:*/
        outP32(OP_SLL | F_RD(r1r) | F_RS1(r2r) | F_RS2(mr),
	       reads_(r2r,mr,0,0), writes_(r1r, 0, OP_NULL));
        scc_regs &= ~regbit(r1r);
	return;
case J_SHRR+J_SIGNED:
case J_SHRR: /*+J_UNSIGNED:*/
        outP32((op & J_UNSIGNED ? OP_SRL : OP_SRA) |
	       F_RD(r1r) | F_RS1(r2r) | F_RS2(mr),
	       reads_(r2r,mr,0,0), writes_(r1r,0,OP_NULL));
        scc_regs &= ~regbit(r1r);
        return;

/* load/store                                                              */

case J_LDRBK: /*+J_UNSIGNED:*/
case J_LDRBK+J_SIGNED:
        if (adconpend) {
          if (r2r==adr1 && dead&J_DEAD_R2) {
            Symstr *name = (Symstr *)adm;
            int32 offset = (int32)adr2 + m;
            outRext((op & J_SIGNED ? OP_LDSB:OP_LDUB) | F_RD(r1r),
                    name, offset, xr_data,
		    reads_(R_BUS2,0,0,0), writes_(r1r, 0, OP_LOAD));
            scc_regs = scc_regs&(~regbit(r1r));
            adconpend = 0;
            return;
          }
          else flush_adcon();
        }
        outrelref(op & J_SIGNED ? OP_LDSB:OP_LDUB, r1r, r2r, m,
		  reads_(r2r,R_BUS2,0,0), writes_(r1r, 0, OP_LOAD));
        scc_regs = scc_regs&(~regbit(r1r));
        return;
case J_STRBK:
        if (adconpend) {
          if (r2r==adr1 && dead&J_DEAD_R2) {
            Symstr *name = (Symstr *)adm;
            int32 offset = (int32)adr2 + m;
            outRext(OP_STB | F_RD(r1r), name, offset, xr_data,
	            reads_(r1r,0,0,0), writes_(R_BUS4,0,OP_STORE));
            adconpend = 0;
            return;
          }
          else flush_adcon();
        }
        outrelref(OP_STB, r1r, r2r, m,
		  reads_(r1r,r2r,0,0), writes_(R_BUS4,0,OP_STORE));
        return;
case J_LDRBR: /*+J_UNSIGNED:*/
case J_LDRBR+J_SIGNED:
        outP32((op & J_SIGNED ? OP_LDSB:OP_LDUB) |
	       F_RD(r1r) | F_RS1(r2r) | F_RS2(mr),
	       reads_(r2r,mr,R_BUS2,0), writes_(r1r,0,OP_LOAD));
        scc_regs = scc_regs&(~regbit(r1r));
        return;
case J_STRBR:
        outP32(OP_STB | F_RD(r1r) | F_RS1(r2r) | F_RS2(mr),
	       reads_(r1r,r2r,mr,0), writes_(R_BUS4,0,OP_STORE));
        return;
case J_LDRWK: /*+J_UNSIGNED:*/
case J_LDRWK+J_SIGNED:
       if (adconpend) {
	 if (r2r==adr1 && dead&J_DEAD_R2) {
	   Symstr *name = (Symstr *)adm;
	   int32 offset = (int32)adr2 + m;
	   outRext((op & J_SIGNED ? OP_LDSH:OP_LDUH) | F_RD(r1r),
		   name, offset, xr_data,
		   reads_(R_BUS2,0,0,0), writes_(r1r,0,OP_LOAD));
	   scc_regs = scc_regs&(~regbit(r1r));
	   adconpend = 0;
	   return;
	 }
	 else flush_adcon();
       }
       outrelref(op & J_SIGNED ? OP_LDSH:OP_LDUH, r1r, r2r, m,
		 reads_(r2r,R_BUS2,0,0), writes_(r1r,0,OP_LOAD));
       scc_regs = scc_regs&(~regbit(r1r));
       return;
case J_STRWK:
        if (adconpend) {
          if (r2r==adr1 && dead&J_DEAD_R2) {
            Symstr *name = (Symstr *)adm;
            int32 offset = (int32)adr2 + m;
            outRext(OP_STH | F_RD(r1r), name, offset, xr_data,
	            reads_(r1r,0,0,0), writes_(R_BUS4,0,OP_STORE));
            adconpend = 0;
            return;
          }
          else flush_adcon();
        }
        outrelref(OP_STH, r1r, r2r, m,
		  reads_(r1r,r2r,0,0), writes_(R_BUS4,0,OP_STORE));
        return;
case J_LDRWR: /*+J_UNSIGNED:*/
case J_LDRWR+J_SIGNED:
       outP32((op & J_SIGNED ? OP_LDSH:OP_LDUH) |
	      F_RD(r1r) | F_RS1(r2r) | F_RS2(mr),
	      reads_(r2r,mr,R_BUS2,0), writes_(r1r,0,OP_LOAD));
       scc_regs = scc_regs&(~regbit(r1r));
       return;
case J_STRWR:
       outP32(OP_STH | F_RD(r1r) | F_RS1(r2r) | F_RS2(mr),
	      reads_(r1r,r2r,mr,0), writes_(R_BUS4,0,OP_STORE));
       return;
case J_LDRK:
        if (adconpend) {
          if (r2r==adr1 && dead&J_DEAD_R2) {
            Symstr *name = (Symstr *)adm;
            int32 offset = (int32)adr2 + m;
            outRext(OP_LD | F_RD(r1r), name, offset, xr_data,
	            reads_(R_BUS2,0,0,0), writes_(r1r,0,OP_LOAD));
            scc_regs = scc_regs&(~regbit(r1r));
            adconpend = 0;
            return;
          }
          else flush_adcon();
        }
        outrelref(OP_LD, r1r, r2r, m,
		  reads_(r2r,R_BUS2,0,0), writes_(r1r,0,OP_LOAD));
        scc_regs = scc_regs&(~regbit(r1r));
        return;
case J_STRK:
        if (adconpend) {
          if (r2r==adr1 && dead&J_DEAD_R2) {
            Symstr *name = (Symstr *)adm;
            int32 offset = (int32)adr2 + m;
            outRext(OP_ST | F_RD(r1r), name, offset, xr_data,
	            reads_(r1r,0,0,0), writes_(R_BUS4,0,OP_STORE));
            adconpend = 0;
            return;
          }
          else flush_adcon();
        }
        outrelref(OP_ST, r1r, r2r, m,
		  reads_(r1r,r2r,0,0), writes_(R_BUS4,0,OP_STORE));
        return;
case J_LDRR:
        outP32(OP_LD | F_RD(r1r) | F_RS1(r2r) | F_RS2(mr),
	       reads_(r2r,mr,R_BUS2,0), writes_(r1r,0,OP_LOAD));
        scc_regs = scc_regs&(~regbit(r1r));
        return;
case J_STRR:
        outP32(OP_ST | F_RD(r1r) | F_RS1(r2r) | F_RS2(mr),
	       reads_(r1r,r2r,mr,0), writes_(R_BUS4,0,OP_STORE));
        return;
case J_ENDPROC:
        /* /* this ought to be done before emit_code, but lab_isset_ will
              need some thought, as it will return false until some code has
	      been emitted, so there is a danger of setting returnlab twice !!
	*/
	if ( 1 /* !lab_isset_(returnlab) && returnlab->u.frefs != NULL */) { 
	  start_basic_block (returnlab);
	  conditional_branch_to(Q_AL, 0, RETLAB);
	}
	/* /* and it gets sillier ... */
	/* Align literals on a double word - taken care of elsewhere (?) */
	/* Align literals on a double word */
        emit_code ();
	if  (((codebase+codep) & double_lit) != 0) {
	  outP32(OP_UNIMP,reads_(0,0,0,0),writes_(0,0,OP_NULL));
	  current_bb->refcount = 1;
	  flush_peepholer();
	  emit_code ();
	}
        dumplits2(0);
        asm_lablist = (LabList *)dreverse((List *)asm_lablist);
        dump_count_names();
        window_rolled = 0;
        return;
#ifdef CPLUSPLUS
case J_ORG:
        flush_peepholer();
        while (copep < m) outP32(OP_NOOP,reads_(0,0,0,0),writes_(0,0,OP_NULL));
        if (codep != m) syserr("J_ORG");
        flush_peepholer();
        break;
#endif
case J_ENTER:
        asm_lablist = 0;
        routine_entry(m);
        return;

case J_PUSHM:
        { int32 i;
	  int32 n = 4*bitcount(m);
	  stack_move -= n;
	  if (stack_move<0) correct_stack(INWINDOW);
	  for (i = (NINTREGS-1); i >= 0; i--) {
	    int32 ii = (window_rolled? i: map_intreg(i));
	    if (m & ((int32)1<<ii)) {
	      n -= 4;
	      outrelref (OP_ST, ii, R_SP, stack_move + WINDOW_SIZE + n,
			 reads_(R_SP,ii,0,0), writes_(R_BUS4,0,OP_STORE));
	    }
	  }
	}
        fp_minus_sp += 4*bitcount(m);
        return;

/* Now the floating point part of the instruction set */

case J_PUSHF:
	stack_move -= 4;
	if (stack_move<0) correct_stack(INWINDOW);
	outrelref (OP_STF, r1r, R_SP, WINDOW_SIZE + stack_move,
		   reads_(R_SP,r1r,0,0),
		   writes_(R_BUS4,0,OP_STORE|OP_FLOAT));
        fp_minus_sp += 4;
        return;
case J_PUSHD:
        stack_move -= 8;
        if (stack_move<0) correct_stack(INWINDOW);
/* The following case arises because, while the SPARC needs doubles to  */
/* be aligned in general, argument passing requires possible unalign.   */
        if (WINDOW_SIZE+stack_move & 4)
        {   outrelref(OP_STF, r1r, R_SP, WINDOW_SIZE + stack_move,
                      reads_(R_SP,r1r,0,0),
		      writes_(R_BUS4,0,OP_STORE|OP_FLOAT));
            outrelref(OP_STF, r1r+1, R_SP, WINDOW_SIZE + stack_move + 4,
                      reads_(R_SP,r1r+1,0,0),
		      writes_(R_BUS4,0,OP_STORE|OP_FLOAT));
        }
        else
            outrelref(OP_STDF, r1r, R_SP, WINDOW_SIZE + stack_move,
                      reads_(R_SP,r1r,r1r+1,0),
                      writes_(R_BUS4,0,OP_STORE|OP_FLOAT));
        fp_minus_sp += 8;
        return;
case J_ADCONF:
        fpaddr(0, r1r, (FloatCon *)m);
        break;
case J_ADCOND:
        fpaddr(1, r1r, (FloatCon *)m);
        break;
case J_MOVFK:
        fpload(OP_LDF, r1r, 0, (FloatCon *)m);
        break;
case J_MOVDK:
        fpload(OP_LDDF, r1r, 1, (FloatCon *)m);
        break;
case J_LDRFK:
        outrelref(OP_LDF, r1r, r2r, m,
		  reads_(r2r,R_BUS2,0,0),
		  writes_(r1r,0,OP_LOAD|OP_FLOAT));
        return;
case J_STRFK:
        outrelref(OP_STF, r1r, r2r, m,
		  reads_(r1r,r2r,0,0),
		  writes_(R_BUS4,0,OP_STORE|OP_FLOAT));
        return;
case J_LDRFR:
        outP32(OP_LDF | F_RD(r1r) | F_RS1(r2r) | F_RS2(mr),
	       reads_(r2r,mr,R_BUS2,0), writes_(r1r,0,OP_LOAD|OP_FLOAT));
	return;
case J_STRFR:
        outP32(OP_STF | F_RD(r1r) | F_RS1(r2r) | F_RS2(mr),
	       reads_(r1r,r2r,mr,0), writes_(R_BUS4,0,OP_STORE|OP_FLOAT));
        return;
case J_LDRDR:
        if (j_aligned(op,J_ALIGN8))
        {   outP32(OP_LDDF | F_RD(r1r) | F_RS1(r2r) | F_RS2(mr),
                   reads_(r2r,mr,R_BUS2,0), writes_(r1r,r1r+1,OP_LOAD|OP_FLOAT));
            return;
        }
        outP32(OP_ADDCC | F_RD(R_TM) | F_RS1(r2r) | F_RS2(mr),
               reads_(r2r,mr,0,0), writes_(R_TM, R_CC, OP_NULL));
        scc_regs = regbit(R_TM);
        r2r = R_TM, m = 0;
        /* drop through */
case J_LDRDK:
/* The de facto SPARC calling standard requires doubles to be possibly  */
/* unaligned -- e.g. consider  f(double a,int b,double c) {g(&a,&c);}   */
/* A further oddity is that J_ALIGN8 is wrong for args since arg1 is    */
/* (BINDADDR_ARG+0) looks aligned, but WINDOW_ARG1 maps it at 0x44.     */
        if ((r2r==R_SP || r2r==R_FP) ? (m & 4)==0 : j_aligned(op,J_ALIGN8))
            outrelref(OP_LDDF, r1r, r2r, m,
                      reads_(r2r,R_BUS2,0,0),
		      writes_(r1r,r1r+1,OP_LOAD|OP_FLOAT));
        else
        {
	  outrelref(OP_LDF, r1r, r2r, m,
		    reads_(r2r,R_BUS2,0,0),
		    writes_(r1r,0,OP_LOAD|OP_FLOAT));
	  outrelref(OP_LDF, r1r+1, r2r, m+4,
		    reads_(r2r,R_BUS2,0,0),
		    writes_(r1r+1,0,OP_LOAD|OP_FLOAT));
        }
        return;
case J_STRDR:
        if (j_aligned(op,J_ALIGN8))
        {   outP32(OP_STDF | F_RD(r1r) | F_RS1(r2r) | F_RS2(mr),
                   reads_(r1r,r1r+1,r2r,mr),
		   writes_(R_BUS4,0,OP_STORE|OP_FLOAT));
            return;
        }
        outP32(OP_ADDCC | F_RD(R_TM) | F_RS1(r2r) | F_RS2(mr),
               reads_(r2r,mr,0,0), writes_(R_TM, R_CC, OP_NULL));
        scc_regs = regbit(R_TM);
        r2r = R_TM, m = 0;
        /* drop through */
case J_STRDK:
        if ((r2r==R_SP || r2r==R_FP) ? (m & 4)==0 : j_aligned(op,J_ALIGN8))
            outrelref(OP_STDF, r1r, r2r, m,
                      reads_(r1r,r1r+1,r2r,0),
                      writes_(R_BUS4,0,OP_STORE|OP_FLOAT));
        else
        {   outrelref(OP_STF, r1r, r2r, m,
                      reads_(r1r,r2r,0,0),
		      writes_(R_BUS4,0,OP_STORE|OP_FLOAT));
            outrelref(OP_STF, r1r+1, r2r, m+4,
                      reads_(r1r+1,r2r,0,0),
		      writes_(R_BUS4,0,OP_STORE|OP_FLOAT));
        }
        return;
/* these next two cases can currently only happen at proc. head. improve? */
case J_MOVIFR:
	/* Load FP register from an integer one */
        /* I really have to go via store !! */
	outfptempref(OP_ST, mr, 0,
		     reads_(mr,0,0,0), writes_(R_BUS4,0,OP_STORE));
	outfptempref(OP_LDF, r1r, 0,
		     reads_(R_BUS2,0,0,0), writes_(r1r,0,OP_LOAD|OP_FLOAT));
	return;
case J_MOVIDR:
	/* Load FP register from 2 integer registers */
        /* I really have to go via store !! */
        if (r2r+1 == mr && (r2r & 1) == 0)
            outfptempref(OP_STD, r2r, 0,
                        reads_(r2r,mr,0,0), writes_(R_BUS4,0,OP_STORE));
        else
        {   outfptempref(OP_ST, r2r, 0,
                        reads_(r2r,0,0,0), writes_(R_BUS4,0,OP_STORE));
            outfptempref(OP_ST, mr, 4,
                        reads_(mr,0,0,0), writes_(R_BUS4,0,OP_STORE));
        }
        outfptempref(OP_LDDF, r1r, 0,
		   reads_(R_BUS2,0,0,0), writes_(r1r,r1r+1,OP_LOAD|OP_FLOAT));
        return;
case J_MOVDIR:
	/* Load 2 integer registers from FP register */
        /* I really have to go via store !! */
        outfptempref(OP_STDF, mr, 0,
		     reads_(mr,mr+1,0,0), writes_(R_BUS4,0,OP_STORE|OP_FLOAT));
        if (r1r+1 == r2r && (r1r & 1) == 0)
            outfptempref(OP_LDD, r1r, 0,
	                 reads_(R_BUS2,0,0,0), writes_(r1r,r2r,OP_LOAD));
        else
        {   outfptempref(OP_LD, r1r, 0,
	                 reads_(R_BUS2,0,0,0), writes_(r1r,0,OP_LOAD));
            outfptempref(OP_LD, r2r, 4,
	                 reads_(R_BUS2,0,0,0), writes_(r2r,0,OP_LOAD));
        }
        return;

case J_MOVFR:
        if (r1r==mr) syserr(syserr_movfdr);
        outP32(OP_FMOVS | F_RD(r1r) | F_RS2(mr),
	       reads_(mr,0,0,0), writes_(r1r,0,OP_FLOAT));
        return;
case J_MOVDR:
        if (r1r==mr) syserr(syserr_movfdr);
        outP32(OP_FMOVS | F_RD(r1r) | F_RS2(mr),
	       reads_(mr,0,0,0), writes_(r1r,0,OP_FLOAT));
        outP32(OP_FMOVS | F_RD(r1r+1) | F_RS2(mr+1),
	       reads_(mr+1,0,0,0), writes_(r1r+1,0,OP_FLOAT));
        return;
case J_NEGFR:
        outP32(OP_FNEGS | F_RD(r1r) | F_RS2(mr),
	       reads_(mr,0,0,0), writes_(r1r,0,OP_FLOAT));
        return;
case J_NEGDR:
        outP32(OP_FNEGS | F_RD(r1r) | F_RS2(mr),
	       reads_(mr,0,0,0), writes_(r1r,0,OP_FLOAT));
        if (r1r != mr) outP32(OP_FMOVS | F_RD(r1r+1) | F_RS2(mr+1),
			      reads_(mr+1,0,0,0), writes_(r1r+1,0,OP_FLOAT));
	return;
/* /* the fix and float sequences are yukky - sparc leaves results in float
      registers, but we are (presumably) trying to move between integer and
      float registers ?!?!? */
case J_FIXFR+J_SIGNED:
        if (r1r >= R_F0) outP32(OP_FSTOI | F_RD(r1r) | F_RS2(mr),
				reads_(mr,0,0,0), writes_(r1r,0,OP_FLOAT));
	else {
	  outP32(OP_FSTOI | F_RD(R_FTMP) | F_RS2(mr),
		 reads_(mr,0,0,0), writes_(R_FTMP, 0, OP_FLOAT));
	  outfptempref(OP_STF, R_FTMP, 0,
		    reads_(R_FTMP,0,0,0), writes_(R_BUS4,0,OP_STORE|OP_FLOAT));
	  outfptempref(OP_LD, r1r, 0,
		    reads_(R_BUS2,0,0,0), writes_(r1r,0,OP_LOAD));
	}
	return;
case J_FIXDR+J_SIGNED:
	if (r1r >= R_F0) outP32(OP_FDTOI | F_RD(r1r) | F_RS2(mr),
				reads_(mr,mr+1,0,0), writes_(r1r,0,OP_FLOAT));
	else {
	  outP32(OP_FDTOI | F_RD(R_FTMP) | F_RS2(mr),
		 reads_(mr,mr+1,0,0), writes_(R_FTMP, 0, OP_FLOAT));
	  outfptempref(OP_STF, R_FTMP, 0,
		    reads_(R_FTMP,0,0,0), writes_(R_BUS4,0,OP_STORE|OP_FLOAT));
	  outfptempref(OP_LD, r1r, 0,
		    reads_(R_BUS2,0,0,0), writes_(r1r,0,OP_LOAD));
	}
	return;
case J_FLTFR+J_SIGNED:
        if (mr < R_F0) {
	  outfptempref(OP_ST, mr, 0,
		       reads_(mr,0,0,0), writes_(R_BUS4,0,OP_STORE));
	  outfptempref(OP_LDF, r1r, 0,
		       reads_(R_BUS2,0,0,0), writes_(r1r,0,OP_LOAD|OP_FLOAT));
	  mr = r1r;
	}
	outP32(OP_FITOS | F_RD(r1r) | F_RS2(mr),
	       reads_(mr,0,0,0), writes_(r1r,0,OP_FLOAT));
	return;
case J_FLTDR+J_SIGNED:
	if (mr < R_F0) {
	  outfptempref(OP_ST, mr, 0,
		       reads_(mr,0,0,0), writes_(R_BUS4,0,OP_STORE));
	  outfptempref(OP_LDF, r1r, 0,
		       reads_(R_BUS2,0,0,0), writes_(r1r,0,OP_LOAD|OP_FLOAT));
	  mr = r1r;
	}
	outP32(OP_FITOD | F_RD(r1r) | F_RS2(mr),
	       reads_(mr,0,0,0), writes_(r1r,r1r+1,OP_FLOAT));
	return;
case J_MOVFDR:
        outP32(OP_FSTOD | F_RD(r1r) | F_RS2(mr),
	       reads_(mr,0,0,0), writes_(r1r,r1r+1,OP_FLOAT));
        return;
case J_MOVDFR:
        outP32(OP_FDTOS | F_RD(r1r) | F_RS2(mr),
	       reads_(mr,mr+1,0,0), writes_(r1r,0,OP_FLOAT));
        return;

case J_ADDFR:
        outP32 (OP_FADDS | F_RD(r1r) | F_RS1(r2r) | F_RS2(mr),
		reads_(r2r,mr,0,0), writes_(r1r,0,OP_FLOAT));
        return;
case J_ADDDR:
        outP32 (OP_FADDD | F_RD(r1r) | F_RS1(r2r) | F_RS2(mr),
		reads_(r2r,r2r+1,mr,mr+1), writes_(r1r,r1r+1,OP_FLOAT));
        return;
case J_SUBFR:
        outP32 (OP_FSUBS | F_RD(r1r) | F_RS1(r2r) | F_RS2(mr),
		reads_(r2r,mr,0,0), writes_(r1r,0,OP_FLOAT));
        return;
case J_SUBDR:
        outP32 (OP_FSUBD | F_RD(r1r) | F_RS1(r2r) | F_RS2(mr),
		reads_(r2r,r2r+1,mr,mr+1), writes_(r1r,r1r+1,OP_FLOAT));
        return;
case J_MULFR:
        outP32 (OP_FMULS | F_RD(r1r) | F_RS1(r2r) | F_RS2(mr),
		reads_(r2r,mr,0,0), writes_(r1r,0,OP_FLOAT));
        return;
case J_MULDR:
        outP32 (OP_FMULD | F_RD(r1r) | F_RS1(r2r) | F_RS2(mr),
		reads_(r2r,r2r+1,mr,mr+1), writes_(r1r,r1r+1,OP_FLOAT));
        return;
case J_DIVFR:
        outP32 (OP_FDIVS | F_RD(r1r) | F_RS1(r2r) | F_RS2(mr),
		reads_(r2r,mr,0,0), writes_(r1r,0,OP_FLOAT));
/*	outP32 (OP_FMOVS | F_RD(r1r) | F_RS2(r1r),
		reads_(r1r,0,0,0), writes_(r1r,0,OP_FLOAT));
	/* /* Seems necessary?? */
        return;
case J_DIVDR:
        outP32 (OP_FDIVD | F_RD(r1r) | F_RS1(r2r) | F_RS2(mr),
		reads_(r2r,r2r+1,mr,mr+1), writes_(r1r,r1r+1,OP_FLOAT));
/*	outP32 (OP_FMOVS | F_RD(r1r) | F_RS2(r1r),
		reads_(r1r,0,0,0), writes_(r1r,0,OP_FLOAT));
	/* /* Seems necessary?? */
        return;
case J_CMPFR:
        correct_stack(EXACT);
        outP32(OP_FCMPS | F_RS1(r2r) | F_RS2(mr),
	       reads_(r2r,mr,0,0), writes_(0,R_FCC,OP_FLOAT));
        cmp_pending = 1;
        fcmp_pending = 1;
	fcmp_used = 0;
	return;
case J_CMPDR:
        correct_stack(EXACT);
        outP32(OP_FCMPD | F_RS1(r2r) | F_RS2(mr),
	       reads_(r2r,r2r+1,mr,mr+1), writes_(0,R_FCC,OP_FLOAT));
        cmp_pending = 1;
        fcmp_pending = 1;
	fcmp_used = 0;
	return;

default:
        syserr(syserr_show_inst, (long)op);
        end_basic_block (NULL, 0, END_DEAD);
        outP32(OP_UNIMP | (0xadbadbad&0x3fffff),
	       0, writes_(0,0,OP_NULL)); /* placeholder */
        end_basic_block (NULL, 0, END_DEAD);
    }
}

/* the next routine is required for the machine independent codebuf.c */
void branch_round_literals(LabelNumber *m)
{
  conditional_branch_to (Q_AL, 0, m);
}

void mcdep_init(void)
{
  bb_list = bb_list_end = current_bb = NULL;
  mc_peeps = NULL;
  state.bus_count = state.fpu = 0;
  cmp_pending = 0; fcmp_pending = 0;
  avoidallocating(R_LR);        /* just for a while */
  obj_symref(targeterrno, 0, 0);
}

void localcg_tidy(void) {}
void localcg_reinit(void) {}
bool alterscc(Icode *ic) { return 1; }


/* vvvvvvvvvvvvvvvvvvvvvv    PEEPHOLER    vvvvvvvvvvvvvvvvvvvv */

/* /* static depends *new_dependency (depends *n, mcpeepdata *p)
    * {
    *   depends *d = (depends *)syn_list2((int)n, (int)p);
    *   /* /* depends *d = malloc (sizeof(depends));
    *         d->next = n;
    *         d->inst = p;
    *   return d;
    * }
    */

static mcpeepdata *new_mcpeep (void)
{
  /* /* mcpeepdata *p = (mcpeepdata *)malloc (sizeof(mcpeepdata)); */
  mcpeepdata *p = (mcpeepdata *)SynAlloc (sizeof(mcpeepdata));
  memclr (p, sizeof(mcpeepdata));
  return p;
}

static BasicBlock *new_BB (LabelNumber *lab)
{
  /* /* BasicBlock *p = (BasicBlock *)malloc (sizeof(BasicBlock)); */
  BasicBlock *p = (BasicBlock *)SynAlloc (sizeof(BasicBlock));
  memclr (p, sizeof(BasicBlock));
  p->head = lab;
  return p;
}

static void delete_dependencies (mcpeepdata *peep)
{
  /* remove this instruction from the dependency count of all following ones */
  depends *follow;
  while ((follow = peep->post) != NULL) {
    if (follow->inst->prior-- < 0) syserr("no dependency to delete");
    peep->post = follow->next;
    peep->count--;
    /* /* free(follow); */
    (void)discard2(follow); /* /* or leave to drop_local_store ?? */
  }

  /* unchain this instruction from mc_peeps */
  if (peep->prev != NULL) (peep->prev)->next = peep->next;
  if (peep->next != NULL) (peep->next)->prev = peep->prev;
  if (peep == mc_peeps) mc_peeps = peep->prev;
}

static void move_one_peeped(mcpeepdata *peep)
{
  /* remove this instruction from the dependency counts of all following ones,
     and from mc_peeps, add it to current_bb->code and ->codeend */
  
  delete_dependencies (peep);
  if (peep->post != NULL) syserr("move_one_peeped still has posts");
  if (peep->count) syserr("move_one_peeped count error");
  if (peep->prior) syserr("move_one_peeped still has priors");
  
  if (current_bb->code == NULL) current_bb->code = peep;
  else current_bb->codeend->next = peep;
  peep->next = NULL;
  peep->prev = current_bb->codeend;
  current_bb->codeend = peep;
}
  
mcpeepdata * find_best_inst(void)
{ /* This function attempts to find the best instruction to schedule next.
     The mechanism is to schedule the instruction with the most instructions
     dependent on it, as this clears problems early.  This is modified after
     a BUS instruction so a second load or store is treated as less good while
     the BUS is active
   */
  mcpeepdata *xx;
  int32 best_penalty = -10;
  int32 num_of_instr = 0,
        blocked = 0;
  mcpeepdata *best = NULL;
  for (xx = mc_peeps; xx != NULL; xx = xx->prev) {
    num_of_instr++;
    if (!xx->prior) {	/* Scheduleable */
      if (xx->uses.blocked) blocked++; /* ... but not to be used */
      else if (NOSCHEDULE) {
                                /* Just take earliest instruction */
        best = xx;
        best_penalty = 1;
      }
      else {
	int32 cnt = xx->count; /* For now just count dependency chain */
	if (state.bus_count == 0) {
	  if (opclass_(xx) & (OP_LOAD|OP_STORE)) {
/*	    if (opclass_(xx) & OP_LOAD) fprintf(stderr, "Important load\n");
 *	    else fprintf(stderr, "Important store\n");
 */
	    cnt = cnt + (opclass_(xx) & OP_LOAD ? 100: 200);
	  }
	}
	else { /* If last was load/store, avoid another one. On a non-stalling
		  machine this instruction should be treated as unschedulable
		  without insertion of no-ops */
/*	  fprintf(stderr, "Bus_count remaining=%ld\n", state.bus_count); */
	  if (opclass_(xx) & (OP_LOAD | OP_STORE))
	    cnt = (opclass_(xx) & OP_LOAD ? -1: -2);
	}
	if (xx->uses.fpu != state.fpu) cnt +=2; /* try to interleave fpu/alu */
	if (cnt >= best_penalty) {
	  best_penalty = cnt, best = xx;
/*	  fprintf(stderr, "best so far %lx(%p) cost %ld\n",
 *		  best->w, best, best_penalty);
 */
	}
      }
    }
  }
  if (best == NULL ) {
    if (!blocked) syserr("No instruction scheduleable");
/* else fprintf(stderr,"No instruction scheduleable, %ld blocked\n",blocked);*/
    return NULL;
  }
/*  fprintf(stderr, "Scheduling %lx out of %ld bus=%d cost=%ld\n",
            best->w, num_of_instr, best->uses.bus, best_penalty); */
  if (best->uses.bus) {
    /* if (state.bus_count) fprintf(stderr, "Stalled %d cycles\n", state.bus_count); */
    state.bus_count = best->uses.bus;
  }
  if (state.bus_count) state.bus_count--;
/*  fprintf(stderr,"New bus count=%ld\n", state.bus_count); */
  return best;
}

void flush_peepholer(void)
{
  /* Print the current state first */
  mcpeepdata *xx;
/*  xx = mc_peeps;
/*  fprintf(stderr, "chain starts with %p\n", xx);
 *  while (xx != NULL) {
 *    depends *yy;
 *    fprintf(stderr,"Inst: %p (%ld, %8lx)\n  prior:%ld",
 *            xx, xx->type, xx->w, xx->prior);
 *    fprintf(stderr,"\n  post:\t");
 *    yy = xx->post;
 *    while (yy !=NULL) {
 *      fprintf(stderr,"%p\t", yy->inst);
 *      yy = yy->next;
 *    }
 *    fprintf(stderr,"\n\n");
 *    xx = xx->prev;
 *  }
 */ 
  while ((mc_peeps != NULL) && (xx = find_best_inst()) != NULL)
    move_one_peeped(xx);	/* remove from mc_peeps, add to current_bb */
}

static void add_flush_noop(void)
{
  mcpeepdata *mc_peep1 = new_mcpeep ();
  mc_peep1->type = OUT_INSTR;
  mc_peep1->w    = OP_NOOP;
  mc_peep1->uses.opclass = OP_NULL;
  insert_one_peep (mc_peep1);
}

static bool intersects(mcpeepdata* a, mcpeepdata* b)
{
  if (a->uses.reads.iregs & b->uses.writes.iregs ||
      a->uses.reads.fregs & b->uses.writes.fregs ||
      a->uses.reads.cc & b->uses.writes.cc ||
      a->uses.reads.fcc & b->uses.writes.fcc ||

      a->uses.writes.iregs & b->uses.reads.iregs ||
      a->uses.writes.fregs & b->uses.reads.fregs ||
      a->uses.writes.cc & b->uses.reads.cc ||
      a->uses.writes.fcc & b->uses.reads.fcc ||

      a->uses.writes.iregs & b->uses.writes.iregs ||
      a->uses.writes.fregs & b->uses.writes.fregs ||
      a->uses.writes.cc & b->uses.writes.cc ||
      a->uses.writes.fcc & b->uses.writes.fcc )
    return YES;
  else return NO;
}

static bool swappable(mcpeepdata *a, mcpeepdata *b)
{
  /*
   * At present two instructions come in the order (a;b) - see if they can be
   * flipped into order (b;a).
   */
  bool ans;
  int32 opca = opclass_(a),
        opcb = opclass_(b);
  if (b->w == OP_RETL && ((a->w) & 0xc1f80000) == OP_RESTORE) {
/*    printf("Swapping RESTORE and RETL\n"); */
				/* Swappable if we change the instruction */
    b->w = OP_RET;
    b->uses.reads.iregs = regbit(R_LR);
    a->uses.writes.iregs &= ~regbit(R_LR); /* Cheat */
    return YES;
  }

  if ((opca & OP_LOAD && opcb & OP_STORE) ||
      (opca & OP_STORE && opcb & (OP_LOAD|OP_STORE)) ||
      intersects(a,b))
    ans = NO;
  else ans = YES;
  return ans;
}

static bool insert_one_peep(mcpeepdata *newp)
{
/*  fprintf(stderr, "Inserting instruction %p\n", newp); */
  if (current_bb == NULL) start_basic_block (NULL);

  newp->prev = mc_peeps;
  newp->next = NULL;
  newp->prior = 0;
  newp->post = NULL;
  newp->count = 0;
  if (mc_peeps != NULL) mc_peeps->next = newp;
				/* Now for dependencies */
  while (mc_peeps != NULL) {	/* Look at all previous instructions */
    if (!swappable(mc_peeps, newp)) { /* Have a dependency */
      newp->prior++;
      mc_peeps->post = new_dependency (mc_peeps->post, newp);
      mc_peeps->count++;
    }
    mc_peeps = mc_peeps->prev;
  }

  mc_peeps = newp;
  return YES;
}

static bool all_dependents_blocked (mcpeepdata *p)
{
  depends *dep;
  for (dep = p->post; dep != NULL; dep = dep->next)
    if (dep->inst->uses.blocked == 0) return NO;
  return YES;
}

static void start_basic_block (LabelNumber *lab)
{
  BasicBlock *bb;

  if (current_bb != NULL)
    /* fprintf(stderr,"start_basic_block calling end_basic_block\n"), */
    end_basic_block (NULL, 0, END_DROP);
  /* if (lab==NULL) fprintf(stderr,"start_basic_block (no label)\n");
   * else fprintf(stderr,"start_basic_block label L%ld\n",
   *              (long)(lab_name_(lab) & 0x7fffffff));
   */

  bb = new_BB(lab);

  /* chain new BasicBlock onto end of list */
  if (bb_list == NULL) bb_list = bb; /* first one */
  else bb_list_end->cdr = bb;	/* chain in order of generation */
  current_bb = bb_list_end = bb;
}

static void delete_instruction (BasicBlock *bb, mcpeepdata *inst)
{
  if (bb->code == inst) {
    if ((bb->code = inst->next) == NULL) bb->codeend = NULL;
    else inst->next->prev = NULL;
    /* /* free (inst); ?? leave to drop_local_store ?? */
  }
  else syserr("deleting non-first instruction - not written yet");
}

static BasicBlock *delete_basic_block (BasicBlock *bb)
{
  BasicBlock *ans = bb->cdr;
  while (bb->code != NULL) delete_instruction (bb, bb->code);
  /* /* free (bb); ?? leave to drop_local_store ?? */
  return ans;
}

/*
 * static char trans_buff[12];
 * static char *trans_type(int32 endtype)
 * {
 * 				/* Translate end types for pretty tracing */
/*   switch (endtype) {
 *   case END_NOOP:
 *     return "NOOP";
 *   case END_DEAD:
 *     return "DEAD";
 *   case END_BXX:
 *     return "BXX";
 *   case END_DROP:
 *     return "DROP";
 *   case END_CALL:
 *     return "CALL";
 *   case END_CALL|END_NOOP:
 *     return "CALL|NOOP";
 *   case END_JMPL:
 *     return "JMPL";
 *   case END_JMPL|END_NOOP:
 *     return "JMPL|NOOP";
 *   case END_BCC:
 *     return "BCC";
 *   case END_BCC|END_NOOP:
 *     return "BCC|NOOP";
 *   case END_BA:
 *     return "BA";
 *   case END_BA|END_NOOP:
 *     return "BA|NOOP";
 *   default:
 *     sprintf(trans_buff,"%lx",endtype);
 *     return trans_buff;
 *   }
 * }
 */

static void end_basic_block (mcpeepdata *newp, int32 noops, int32 endtype)
{
  /* fprintf(stderr,"end_basic_block type %s\n",trans_type(endtype)); */
  if (current_bb == NULL) {
    if (newp==NULL)
      syserr("end_basic_block type %ld no current block", endtype);
    start_basic_block (NULL);
  }
  current_bb->flags = endtype;

  switch (endtype & END_TYPES) {
  default:
    syserr("end_basic_block unknown type %ld", endtype);
  case END_DEAD:
    if (noops) syserr("end_basic_block DEAD %ld no-ops", noops);
    if (newp != NULL) insert_one_peep (newp);
    flush_peepholer();
    break;
  case END_DROP:
    if (noops) syserr("end_basic_block DROP %ld no-ops", noops);
    if (newp != NULL) insert_one_peep (newp);
    /* if (state.bus_count)
       fprintf(stderr,"end_basic_block %d bus cycles left\n", state.bus_count); */
    flush_peepholer();
    break;
  case END_BXX:
    if (newp == NULL) syserr("end_basic_block BXX with no instruction");
    if (newp->lab == NULL) syserr("end_basic_block BXX with no label");
    current_bb->nextlab = newp->lab;
    insert_one_peep (newp);
    flush_peepholer();
    break;
  case END_BCC:
  case END_BA:
    if (newp == NULL)
      syserr("end_basic_block type %ld with no instruction", endtype);
    if (newp->lab == NULL)
      syserr("end_basic_block type %ld with no label", endtype);
    current_bb->nextlab = newp->lab;
    /* drop through */
  case END_CALL:
  case END_JMPL:
    {
      mcpeepdata *mc_peep1, *mc_peep2;
      if (noops != 1)
	syserr("end_basic_block type=%ld %ld no-ops", endtype, noops);
      if (newp == NULL)
	syserr("end_basic_block type %ld with no instruction", endtype);
      /* CALL/JMPL/BCC/BA is not mergeable with anything. Is it swappable
	 with any preceding instruction which has no dependencies? */
      /* /* nb at the moment mc_peeps points at the *end* of the code */
	for (mc_peep1 = mc_peeps; mc_peep1 != NULL; mc_peep1 = mc_peep1->prev) {
	  if (mc_peep1->post == NULL && swappable (mc_peep1, newp)) {
	    /* /* ought to worry about load/store, later .... */
	    mc_peep1->uses.blocked = 1;
	    break;			/* /* This should countdown on noops */
	    }
      }
      if (newp->uses.fpu) {
	/* fbcc is odd, as the instruction preceding it must not be a floating
	   point instruction. Search back for one which is free, or all (one) of
	   whose dependencies are blocked, and which does not have .fpu set */
	for (mc_peep2 = mc_peeps; mc_peep2 != NULL; mc_peep2 = mc_peep2->prev) {
	  if (mc_peep2->uses.fpu == 0 &&
	      mc_peep2->uses.blocked == 0 &&
	      all_dependents_blocked(mc_peep2)) {
	    /* /* ought to worry about load/store, later .... */
	    mc_peep2->uses.blocked = 1;
	    break;
	  }
	}
	flush_peepholer ();
	if (mc_peep2 != NULL) mc_peep2->uses.blocked = 0;
	else add_flush_noop ();
      }    
      
      flush_peepholer ();
      insert_one_peep (newp);
      flush_peepholer();
      if (mc_peep1 != NULL) mc_peep1->uses.blocked = 0;
      else add_flush_noop (), current_bb->flags |= END_NOOP;
      flush_peepholer ();
    }
    if (endtype & (END_BA|END_CALL|END_JMPL)) state.bus_count = state.fpu = 0;
    break;
  }
  current_bb = NULL;
}

static int32 trace_get_label(BasicBlock *bb)
{
  if (bb->head) return lab_name_(bb->head)&0x7fffffff;
  else return 0;
}

static void delete_unreferenced_blocks (void)
{/* if any block has a zero refcount, reduce the refcount of its target block;
    follow chains of such blocks */

  BasicBlock *bb1 = NULL,
             *bb2 = bb_list;
  bool again = YES;
  while (again) {
    again = NO;
    while (bb2 != NULL) {
      if (bb2->refcount == 0) {

	if (bb2->code == NULL)
	  /* fprintf(stderr,"omitting empty block %.8x\n", (int)bb2) */;
	else if (bb2->head == returnlab)
	  /* fprintf(stderr,"omitting unreferenced returnlab\n") */;
	else {
	  if (ALLMSG)
	    fprintf(stderr,"omitting non-empty block L%ld with zero refcount\n",
		    trace_get_label(bb2));
	}

	if (bb2->flags & END_BCC)
	  if (--(bb2->cdr->refcount) == 0) again = YES;
				/* as we may already have visited bb2->cdr */
	if (bb2->nextbb != NULL)
	  if (--(bb2->nextbb->refcount) == 0) again = YES;
				/* as we may already have visited bb2->nextbb */
	if (bb_list_end == bb2) bb_list_end = bb1;
	bb2 = delete_basic_block (bb2);
	if (bb1 == NULL) bb_list = bb2;
	else bb1->cdr = bb2;
      }
      else{
	bb1 = bb2;
	bb2 = bb2->cdr;
      }
    }
  }
}

static void print_block_list()
{
  BasicBlock *bb;
  LabelNumber *l1, *l2;
  for (bb = bb_list; bb != NULL; bb = bb->cdr) {
    l1 = bb->head;
    l2 = bb->nextlab;
    fprintf(stderr,"BasicBlock %p refcount %ld ", bb, bb->refcount);
    if (l1==NULL) fprintf(stderr,"(no label)");
    else fprintf(stderr,"label L%ld", (long)(lab_name_(l1) & 0x7fffffff));
    fprintf(stderr," has target ");
    if (l2==NULL) fprintf(stderr,"(no label)");
    else fprintf(stderr,"label L%ld", (long)(lab_name_(l2) & 0x7fffffff));
    fprintf(stderr,"\n");
  }
}

BasicBlock *last_block_split;

static BasicBlock *split_basic_block (BasicBlock *bb_to, int32 after)
{
  /* Some block bb_from goes to bb_to via a BA or BCC, and bb_from ends with a
     no-op which we wish to replace with a useful instruction. Normally this
     will be the first instruction in bb_to, and we will alter the destination
     of bb_from to be one instruction later, which will necessitate inventing a
     new label and splitting bb_to into two blocks, the first of which has only
     one instruction.
     /* However, there are also a few special cases where we can do somewhat
	better ?? */

				/* There is a serious problem if the block
				 * being split is actually the bb_from block
				 * as there is information in hand -- JPff */

  /* 'after' will always be 1 for sparc, but we generalise slightly in
     anticipation of machines with longer overrun on jumps */
  
  BasicBlock *bb1, *bb2;
  mcpeepdata *inst1, *inst2;

  if (after != 1) syserr("invalid call to split_basic_block (1)");
  if (bb_to == NULL) syserr("invalid call to split_basic_block (2)");
  
  bb1 = bb_to;
  inst1 = bb1->code;
  inst2 = inst1;
  if (ALLMSG) fprintf(stderr,"Block-start:%p instr=%lX\n", bb1, inst2->w);

  while (after-- > 0) {
    while (inst2 == NULL) {
      bb1 = bb1->cdr;		/* Empty block so drop through */
      inst1 = bb1->code;
      inst2 = inst1;
      if (ALLMSG) fprintf(stderr,"Block-start:%p instr=%lX\n", bb1, inst2->w);
      /* fprintf(stderr,"split_basic_block no instruction");/* JPff */
    }
    inst2 = inst2->next;
    if (ALLMSG) {
      if (inst2 != NULL)
	fprintf(stderr,"                     instr=%lX\n", inst2->w);
      else
	fprintf(stderr,"                     instr=NULL\n");
    }
  }
  while (inst2 == NULL) {
    /* no more instructions in this block, move to the next block */
/*    fprintf(stderr,"split_basic_block no more instructions"); */
    bb1 = bb1->cdr;
    if (bb1 == NULL) syserr("split_basic_block no block");
    inst1 = bb1->code;
    inst2 = inst1;
    if (ALLMSG) fprintf(stderr,"Block-start:%p instr=%lX\n", bb1, inst2->w);
  }
  
  last_block_split = bb1;
  /* bb1 is the new target block, inst1 its first instruction, and inst2 the
     instruction within bb1 which is to be the new target instruction, and
     therefore first in a (possibly new) block */
  if (inst2 == inst1) {
    if (ALLMSG) fprintf(stderr, "Block already split return %p\n", bb1);
    /* /* bb1->refcount++; JPFF*/
    return bb1; /* new target already the start of a block */
  }
  
  bb2 = new_BB (NULL);
  bb2->cdr = bb1->cdr;
  bb2->head = nextlabel();
  bb2->code = inst2;
  bb2->codeend = bb1->codeend;
  bb2->refcount = 1;		/* ie bb1 */
  bb2->flags = bb1->flags;

  bb1->cdr = bb2;
  bb1->codeend = inst1;

  if (inst2->next == NULL &&   /* only two instructions in bb1, and ... */
      bb1->flags & (END_BCC|END_BA|END_JMPL|END_CALL)) {
    bb2->nextbb = NULL;
    bb2->nextlab = NULL;
    bb2->flags = END_DEAD;
  }
  else {
    bb2->nextbb = bb1->nextbb;
    bb2->nextlab = bb1->nextlab;
    bb1->nextbb = bb2;
    bb1->nextlab = bb2->head;
    bb1->flags = END_DROP;
  }

  inst1->next = NULL;
  inst2->prev = NULL;

  if (ALLMSG) {
    if (trace_get_label(bb1))
      fprintf(stderr, "Splitting %p with new label L%ld(%p)\n",
	      bb1, trace_get_label(bb2), bb2);
    else 
      fprintf(stderr, "Splitting L%ld(%p) with new label L%ld(%p)\n",
	      trace_get_label(bb1), bb1, trace_get_label(bb2), bb2);
  }
  if (ALLMSG) print_block_list();
  return bb2;
}

static void optimise_block_links (void)
{
  /* optimise links - replace no-ops by useful instructions, annulling the
     preceding branch if appropriate, amending the branch to jump to a new
     label 4 bytes further on, splitting the target block after one
     instruction, incrementing the refcount of the new block and decrementing
     that of the old one. Also some other fancy optimisations */

  BasicBlock *bb1, *bb2;
  bool again = YES, bcc;
  mcpeepdata *inst1, *inst2;
  int32 flags1, flags2;

  /* if any block has a zero refcount, reduce the refcount of its target block;
     follow chains of such blocks */
  delete_unreferenced_blocks ();

  while (again) {
    again = NO;
    if (ALLMSG) fprintf(stderr, "TOP_OF_SCAN\n");

    for (bb1 = bb_list; bb1 != NULL; bb1 = bb1->cdr) {
      flags1 = bb1->flags;
      if (ALLMSG)
	fprintf(stderr, "Looking at block %p with flags %lx\n", bb1, flags1);
      /* /* if (!(bb1->flags & (END_BCC|END_BA|END_CALL))) continue;
	 END_CALL can be optimised in some very particular circumstances -
	 not written yet */
      if (!(flags1 & (END_BCC|END_BA))) continue;
      bcc = (flags1 & END_BCC? YES: NO);
      bb2 = bb1->nextbb;	/* original target block */
      if (ALLMSG) fprintf(stderr, "bb2 = %p\n", bb2);
      if (bb2 == NULL) print_block_list(), syserr("bb2==NULL");
      flags2 = bb2->flags;
      if (flags2 & (END_BA|END_BCC|END_CALL|END_JMPL) &&
	  /* if there is more than just the 'funny' ending to bb2, then the
	     *start* of bb2 is effectively END_DROP */
	  !(bb2->code == NULL ||             /* can this ever happen ?? */
	    bb2->code->next == NULL ||       /* only one instruction;
						the other has been split off */
	    bb2->code->next->next == NULL )) /* only two instructions */
	flags2 = END_DROP; /* ie flags2 now represents the *start* of bb2;
			      bb2->flags still represents the end of bb2 */

      /* /* if bb2 == bb1->cdr (because of deletion of intervening blocks)
	 one or two of the last two instructions in bb1 can be deleted - but
	 what if there are exactly two? - think a bit longer! */
      if (ALLMSG) fprintf(stderr, "flags2 = %lx\n", flags2);
      if ((flags2 & END_BA) &&
	  ((flags1 | flags2) & END_NOOP)) {
	  /* bcc   or ba L1; nop; ... L1: ba L2; xxx
	     ->
	     bcc,a or ba L2; xxx; ... L1: ba L2; xxx */

	  /* bcc   or ba L1; xxx; ... L1: ba L2; nop
	     ->
	     bcc   or ba L2; xxx; ... L1: ba L2; nop 
	        ^^^^ not annuled; CHECK THIS         */

	if (ALLMSG)
	  fprintf(stderr,"optimising %s -> L%ld START_BA -> L%ld\n",
		bcc?"BCC":"BA", lab_name_(bb1->nextlab)&0x7fffffff,
		lab_name_(bb2->nextlab)&0x7fffffff);

	if (ALLMSG && !(flags1&END_NOOP)) fprintf(stderr,"BA;xxx case\n");
	if (bb1->nextlab != bb2->nextlab) { /* *** WORRY WORRY WORRY *** */
	  /* modify the branch instruction */
	  bb1->nextbb = bb2->nextbb;
	  bb1->nextlab = bb2->nextlab;
	  inst1 = bb1->codeend;
	  if (((inst1->w & ~0x3e000000) != OP_B) &&
	      ((inst1->w & ~0x3e000000) != OP_BF))
	    inst1 = inst1->prev; /* ie block bb1 not split at end */
	  inst2 = bb2->code;
	  if (bcc) inst1->w |= A_ANNUL, bb1->flags |= END_ANNUL;
	  inst1->lab = inst2->lab;

	  /* modify the delay slot */
	  inst1 = inst1->next;
	  if (inst1 == NULL)        /* ie block bb1 split at end */
	    inst1 = bb1->cdr->code; /* first instruction in next block */
	  inst2 = inst2->next;
	  if (inst2 == NULL)        /* ie block bb2 split after first */
	    inst2 = bb2->cdr->code; /* first instruction in next block */
	  if (inst2->w == OP_NOOP) again = YES;
	  else {
	    mcpeepdata *inst1p = inst1->prev;
	    *inst1 = *inst2;     /* copy into no-op slot */
	    inst1->next = NULL;
	    inst1->prev = inst1p;
	    bb1->flags &= ~END_NOOP;
	  }

	  if (bb1->nextbb) bb1->nextbb->refcount++;
	  if (--bb2->refcount == 0)
	    again = YES, delete_unreferenced_blocks();

	  if (ALLMSG)
	    fprintf(stderr,"optimised %s -> L%ld\n", bcc?"BCC,a":"BA",
		    lab_name_(bb1->nextlab)&0x7fffffff);
	}
      }
      else if ((flags1 & END_BA) &&
	       (flags2 & END_JMPL) &&
	       (flags1 & END_NOOP)) {
	/* ba  L1; nop;   ... L1: jmp ??; xxx
	   ->
	   jmp ??; xxx;   ... L1: jmp ??; xxx */

	/* /* RGH could also do 
	   ba  L1; xxx;   ... L1: jmp ??; nop
	   ->
	   jmp ??; xxx;   ... L1: jmp ??; nop
	   iff jmp and xxx are swappable - which is too difficult for
	   my poor brain to work out, and which probably doesn't happen
	   very often anyway ? */

	if (ALLMSG)
	  fprintf(stderr,"optimising BA; NOOP -> L%ld JMPL\n",
		lab_name_(bb1->nextlab)&0x7fffffff);

	/* modify the branch instruction */
	bb1->nextbb = NULL;
	bb1->nextlab = NULL;
	inst1 = bb1->codeend;
	if (((inst1->w & ~0x3e000000) != OP_B) &&
	    ((inst1->w & ~0x3e000000) != OP_BF))
	  inst1 = inst1->prev; /* ie block bb1 not split at end */
	inst2 = bb2->code;
	{ mcpeepdata *inst1n = inst1->next,
	             *inst1p = inst1->prev;
	  *inst1 = *inst2;	/* copy the jmp */
	  inst1->next = inst1n;
	  inst1->prev = inst1p;
	}

	/* modify the delay slot */
	inst1 = inst1->next;
	if (inst1 == NULL)        /* ie block bb1 split at end */
	  inst1 = bb1->cdr->code; /* first instruction in next block */
	inst2 = inst2->next;
	if (inst2 == NULL)        /* ie block bb2 split after first */
	  inst2 = bb2->cdr->code; /* first instruction in next block */
	bb1->flags = (bb1->flags & ~END_BA) | END_JMPL;
	if (inst2->w == OP_NOOP) again = YES;
	else {
	  mcpeepdata *inst1p = inst1->prev;
	  *inst1 = *inst2;     /* copy into no-op slot */
	  inst1->next = NULL;
	  inst1->prev = inst1p;
	  bb1->flags &= ~END_NOOP;
	}

	if (--bb2->refcount == 0)
	  again = YES, delete_unreferenced_blocks();

	if (ALLMSG) fprintf(stderr,"optimised JMPL\n");
      }
      else 
	if (!(flags1 & END_NOOP)) continue;
      else 
	if (flags2 & END_CALL) {
	  if (ALLMSG) {
	    fprintf(stderr,"attempting to optimise %s -> L%ld ",
		    bcc?"BCC":"BA", trace_get_label(bb2));
	    fprintf(stderr,"but cannot as target is CALL\n");
	  }
	}
      else
	if (flags2 & END_JMPL && bcc) {
	  if (ALLMSG) {
	    fprintf(stderr,"attempting to optimise BCC -> L%ld ",
		    trace_get_label(bb2));
	    fprintf(stderr,"but cannot as target is JMPL\n");
	  }
	}
      else {
	BasicBlock *bb3;
	if (ALLMSG)
	  fprintf(stderr,"optimising %s -> L%ld \n",
		bcc?"BCC":"BA", trace_get_label(bb2));
	/* bcc   or ba L1; nop; ... L1: xxx; yyy
	   ->
	   bcc,a or ba L2; xxx; ... L1: xxx; L2: yyy */

	if (bb2->code == NULL) {
	  /* no more instructions in this block, move to the next block */
	  if (ALLMSG)
	    fprintf(stderr, "Next block too short; try again\n");
	  bb1->nextbb = bb2->cdr;
	  bb1->nextlab = bb1->nextbb->head;
	  bb1->nextbb->refcount++;
	  inst1 = bb1->codeend;
          if (((inst1->w & ~0x3e000000) != OP_B) &&
	      ((inst1->w & ~0x3e000000) != OP_BF))
	    inst1 = inst1->prev; /* ie block bb1 not split at end */
	  inst1->lab = bb1->nextlab;
	  if (--bb2->refcount == 0) delete_unreferenced_blocks();
	  again = YES;
	}
	else {
	  bb3 = split_basic_block (bb2, 1);
	  if (bb1 == last_block_split) {
	    bb1 = bb3; /* Splitting oneself!! */
	    if (ALLMSG) fprintf(stderr, "Self splitting: was %p now %p\n",
				last_block_split, bb1);
	    again = YES;
	  }
	
	  /* modify the branch instruction */
	  bb1->nextbb = bb3;
	  bb1->nextlab = bb3->head;
	  inst1 = bb1->codeend;
          if (((inst1->w & ~0x3e000000) != OP_B) &&
	      ((inst1->w & ~0x3e000000) != OP_BF))
	    inst1 = inst1->prev; /* ie block bb1 not split at end */
	  if (bcc) inst1->w |= A_ANNUL, bb1->flags |= END_ANNUL;
	  inst1->lab = bb3->head;
	  
	  /* modify the delay slot */
	  inst1 = inst1->next;
	  if (inst1 == NULL)        /* ie block bb1 split at end */
	    inst1 = bb1->cdr->code; /* first instruction in next block */
	  inst2 = bb2->code;
	  if (inst2->w == OP_NOOP) again = YES;
	  else {
	    mcpeepdata *inst1p = inst1->prev;
	    *inst1 = *inst2;     /* copy into no-op slot */
	    inst1->prev = inst1p;
	    bb1->flags &= ~END_NOOP;
	  }
	  
	  bb3->refcount++;
	  if (--bb2->refcount == 0)
	    again = YES, delete_unreferenced_blocks();
	  if (ALLMSG)
	    fprintf(stderr,"optimised %s -> L%ld (%ld)(%ld)\n",
		    bcc?"BCC,a":"BA", trace_get_label(bb3),
		    bb2->refcount, bb3->refcount);
	}
      }
    }
  }
  
  if (ALLMSG) fprintf(stderr, "TOP_OF_FINAL_SCAN\n");
  for (bb1 = bb_list; bb1 != NULL; bb1 = bb1->cdr) {
    flags1 = bb1->flags;
    if (ALLMSG)
      fprintf(stderr, "Looking at block %p with flags %lx\n", bb1, flags1);
    if (!(flags1 & (END_BCC|END_BA|END_JMPL))) continue;
    bcc = (flags1 & END_BCC? YES: NO);
    bb2 = bb1->cdr;
    if (ALLMSG) fprintf(stderr, "bb2 = %p\n", bb2);
    
    if (flags1 & (END_BA|END_BCC) && !(flags1 & END_ANNUL) &&
	bb2==bb1->nextbb) {
      /* ba or bcc L1; xxx; L1: yyy   (but not bcc,a)
	 ->
	 	       xxx; L1: yyy */

      inst1 = bb1->codeend;
      if (((inst1->w & ~0x3e000000) != OP_B) &&
	  ((inst1->w & ~0x3e000000) != OP_BF))
	inst1 = inst1->prev; /* ie block bb1 not split at end */
      if (inst1->prev == NULL) bb1->code = inst1->next;
      else inst1->prev->next = inst1->next;
      if (inst1->next == NULL) bb1->codeend = inst1->prev;
      else inst1->next->prev = inst1->prev;
      bb1->flags = (bb1->flags & ~(END_BA|END_BCC) | END_DROP);
      if (bcc) bb2->refcount--;
    }
    else if ((!bcc || (flags1 & END_ANNUL)) && bb2 != NULL) {
      /* ba L1 or bcc,a L1 or jmp; xxx; L2: xxx   (incl L1=L2)
	 ->
	 ba L1 or bcc   L1 or jmp;      L2: xxx */

      inst1 = bb1->codeend;
      if (((inst1->w & ~0x3e000000) == OP_B) ||
	  ((inst1->w & ~0x3e000000) == OP_BF))
	continue;		/* ie block bb1 split at end - so first xxx
				   is labelled, so cannot eliminate it.  */
      while (bb2->code == NULL) bb2 = bb2->cdr;	/* skip empty blocks */
      inst2 = bb2->code;
      if (inst1->type    != inst2->type    ||
	  inst1->w       != inst2->w       ||
	  inst1->s       != inst2->s       ||
	  inst1->off     != inst2->off     ||
	  inst1->lab     != inst2->lab     ||
	  inst1->reftype != inst2->reftype ) {
	/* So, look for the case
	   ba L1; nop -> ba,a L1 */
	if ((flags1 & (END_BA|END_NOOP)) == (END_BA|END_NOOP)) {
	  inst1 = inst1->prev; /* the branch instruction */
	  inst1->w |= A_ANNUL;
	  bb1->flags = (bb1->flags & END_NOOP) | END_ANNUL;
	  inst1->next = NULL;  bb1->codeend = inst1;
	}
	continue;
      }
      inst1 = inst1->prev;	/* the branch instruction */
      inst1->next = NULL;
      bb1->codeend = inst1;
      if (bcc) inst1->w &= ~A_ANNUL, bb1->flags &= ~END_ANNUL;
    }
    else if ((flags1 & (END_BA|END_NOOP)) == (END_BA|END_NOOP)) {
      /* Look for the case
	 ba L1; nop  at end of function
	 -> ba,a L1; nop */
      if (ALLMSG) printf("Special base of ba;nop at end of function\n");
      bb1->codeend->prev->w |= A_ANNUL;
      bb1->flags |= END_ANNUL;
    }
    else if ((flags1 & (END_BCC|END_NOOP)) == (END_BCC|END_NOOP)) {
      /* bcc L1; nop; L2: xxx
	 ->
	 bcc L1;      L2: xxx
	 only if xxx is 'safe' ie does not write any regs or to store
	 nb can write CC or fCC as these are not remembered after bcc
	 Also must avoid control transfers */
      
      inst1 = bb1->codeend;
      if (((inst1->w & ~0x3e000000) == OP_B) ||
	  ((inst1->w & ~0x3e000000) == OP_BF))
	continue; /* ie block bb1 split at end - so nop is labelled,
		     so cannot eliminate it */
      while (bb2->code == NULL) bb2 = bb2->cdr;	/* skip empty blocks */
      inst2 = bb2->code;
      if (inst2->uses.writes.iregs != 0 ||
	  inst2->uses.writes.fregs != 0 ||
	  opclass_(inst2) & OP_STORE ||
	  (inst2->w & ~0x3e000000) == OP_B ||
	  (inst2->w & ~0x3e000000) == OP_BF ||
	  (inst2->w &  0xc1f80000) == OP_JMPL ||
	  (inst2->w &  0xc0000000) == OP_CALL)
	continue; /* not safe to put inst2 into inst1's delay slot */
      inst1 = inst1->prev;	/* the branch instruction */
      inst1->next = NULL;
      inst1->w &= ~A_ANNUL;
      bb1->codeend = inst1;
      bb1->flags &= ~(END_NOOP | END_ANNUL);
    }

  }
}

static void emit_one_peeped(mcpeepdata *peep)
{
  switch (peep->type) {
  case OUT_NULL:
    break;
  case OUT_COUNT:		/* /* nb this emits two - does it matter? */
    codexrefs = (CodeXref *)global_list4(SU_Xref, codexrefs,
					 X_absreloc|(codebase+codep),
					 bindsym_(datasegment), dataloc);
    outcodeword(dataloc, LIT_ADCON);
    gendcI(4,0);        /* Generate the slot */
    outcodeword(peep->w, LIT_NUMBER);
    break;
  case OUT_LABREF:
    if (lab_isset_(peep->lab)) {
      int32 w1 = ((peep->lab->u.defn & 0x00ffffff) - codep) >> 2;
                 /* words! */
      if ((-w1) > 0x001fffff) syserr(syserr_back_coderef, (long)peep->w);
      peep->w |=  (w1 & 0x3fffff);
    }
    else addfref_(peep->lab, codep | peep->reftype);
    /* drop through */
  case OUT_INSTR:
    outcodeword(peep->w, LIT_OPCODE);
    break;
  case OUT_CALL:
    {
      int32 d = (peep->w & ~OP_CALL) << 2;
      d = (((unsigned32) (d-codebase-codep)) >> 2);
      outcodewordaux(OP_CALL | d, LIT_OPCODE, peep->s);
    }
    break;
  case OUT_EXTCALL:
    codexrefs = (CodeXref *)global_list3(
                             SU_Xref, codexrefs,
                             X_PCw30reloc | (codebase+codep), peep->s);
    outcodewordaux(OP_CALL | 0, LIT_RELADDR, peep->s);
    break;
  case OUT_CODELAB:		/* BXX only ? */
    if lab_isset_(peep->lab) { /* /* only if returnlab? never? */
      codexrefs = (CodeXref *)global_list4 (SU_Xref, codexrefs,
				X_absreloc | (codebase+codep),
				bindsym_(codesegment),
				codebase + (peep->lab->u.defn & 0x00ffffff));
    } else {
      codexrefs = (CodeXref *)global_list4 (SU_Xref, codexrefs,
				X_absreloc | (codebase+codep),
				bindsym_(codesegment), 0);
      addlongfref_ (peep->lab, codep|LABREF_ABS32, codexrefs);
    }
    outcodewordaux (0, LIT_ADCON, bindsym_(codesegment));
    break;
  case OUT_CODELABHI:
  case OUT_EXTLABHI:
    codexrefs = (CodeXref *)global_list4(
                             SU_Xref, codexrefs,
                             X_DataAddr | (codebase+codep),
                             peep->s, peep->off);
    if (peep->type == OUT_CODELABHI)
      addlongfref_(peep->lab, codep | LABREF_HI22, codexrefs);
    outcodewordaux(peep->w, LIT_RELADDR, peep->s);
    break;
  case OUT_CODELABLO:
  case OUT_EXTLABLO:
    codexrefs = (CodeXref *)global_list4(
                             SU_Xref, codexrefs,
                             X_DataAddr1 | (codebase+codep),
                             peep->s, peep->off);
    if (peep->type == OUT_CODELABLO)
      addlongfref_(peep->lab, codep | LABREF_LO10, codexrefs);
    outcodewordaux(peep->w, LIT_RELADDR, peep->s);
    break;
  default:
    syserr("emit_one_peeped unknown");
  }
}

static void emit_code (void)
{
  /* /* the following sequence is a first attempt at what is required here.. */

  /* end_basic_block if one is open */
  if (current_bb != NULL) end_basic_block (NULL, 0, END_DEAD);

  /* The code within each basic block has already been scheduled by
     end_basic_block, and the resulting sequence chained off the
     BasicBlock->code and BasicBlock->codeend fields (the latter in reverse
     order)
  */

  /* link basic blocks in execution order */
  { BasicBlock *bb1, *bb2;
    for (bb1 = bb_list; bb1 != NULL; bb1 = bb1->cdr) {
      switch (bb1->flags & END_TYPES) {
      default:
	syserr("link basic blocks unknown type %ld", bb1->flags);
      case END_DEAD:		/* goes nowhere */
      case END_JMPL:		/* could go anywhere */
	break;
      case END_DROP:		/* drop through to next block */
      case END_CALL:		/* call+1 instrn, then drop through */
	bb2 = bb1->cdr;
	bb1->nextbb = bb2;
	bb1->nextlab = bb2->head;
	bb2->refcount++;
	break;
      case END_BCC:		/* conditional branch, has two targets */
	(bb1->cdr->refcount)++;	/* increment count on drop-through */
	/* now find the target block when the branch is taken */
	/* drop through */
      case END_BXX:		/* case branch */
      case END_BA:		/* unconditional branch */
	for (bb2 = bb_list; bb2 != NULL; bb2 = bb2->cdr) {
	  if (bb2->head == bb1->nextlab) {
	    bb1->nextbb = bb2;
	    (bb2->refcount)++;
	    break;
	  }
	}
	if (bb2 == NULL) syserr("cannot find BA/BCC target label L%ld\n",
				(long)(lab_name_(bb1->nextlab) & 0x7fffffff));
	break;
      }
    }
  }


  if (ALLMSG) print_block_list(); 
      
  /*  replace no-ops by useful instructions wherever possible */
  optimise_block_links ();

  /* emit blocks */
  { BasicBlock *bb;
    for (bb = bb_list; bb != NULL; bb = bb->cdr) {
      mcpeepdata *xx;
      if (bb->refcount == 0) syserr("emitting unreferenced block");
      if (bb->head != NULL) setlabel(bb->head);
      for (xx = bb->code; xx != NULL; xx = xx->next)
	emit_one_peeped (xx);
    }
  }

  /* delete basic blocks - recover store */
  while (bb_list != NULL) bb_list = delete_basic_block (bb_list);
  bb_list_end = NULL;
}

/* ^^^^^^^^^^^^^^^^^^^^^^    PEEPHOLER    ^^^^^^^^^^^^^^^^^^^^ */

/* End of section sparc/gen.c */
