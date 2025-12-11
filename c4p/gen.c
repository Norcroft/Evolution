
/* C compiler file c4p/gen.c :  Copyright (C) Codemist Ltd., 1996-2005. */
/* version 2 */

/* exports:
   void show_instruction(J_OPCODE op, VRegInt r1, VRegInt r2, VRegInt m);
   RealRegister local_base(Binder *b);
   int32 local_address(Binder *b);
   bool immed_cmp(int32);
*/

/* we use OP_NOP for RAMA/RAMB waits, OP_WAITM for other memory.        */

#define xinDRAM (pp_pragmavec['m'-'a'] > 0)          /* @@@ NOW DEAD */
#define xinSHIFT (pp_pragmavec['b'-'a'] != 0)
#define xinSTDok (!(pp_pragmavec['k'-'a']+1 & 1))
#define xinLDDok (!(pp_pragmavec['k'-'a']+1 & 2))
#define xinSTKUSE (pp_pragmavec['u'-'a']<=0 ? 3 : pp_pragmavec['u'-'a'])

#define xinCALL(procflags) ((procflags) & BLKCALL && xinSTKUSE < 2)

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
#include "bind.h"      /* for sym_insert_id()                */
#include "util.h"      /* padsize */
#include "errors.h"

#define R_LL R_LR      /* was R_IP */

#define OP_ILLEGAL 0xffff

#define STACK_NEG_EXTENSION (0)
   /* allow 0 words preceeding SP to be written before we update it.    */

/* Bits for MOVHI given that the low bits are sign-extended:            */
#define hi1(x) (((x) + 0x8000) >> 16)

#define opisLDH_STH(op) ((op) == OP_LD_Wr || (op) == OP_ST_Wr)
#define opisLDW_STW(op) ((op) == OP_LD_Lr || (op) == OP_ST_Lr)
#define opisLOAD(op) ((op) == OP_LD_Br || (op) == OP_LD_Wr || (op) == OP_LD_Lr)

#define TRUE    (1)
#define FALSE   (0)

static bool lock_r0;
static bool xin_stkerrmsg;

static void outi1(int32);

static  SET_BITMAP waitm_list = 0, wait1_list = 0;

static SET_BITMAP chkreg(RealRegister r)
{
    if ((r & ~7) != 0) syserr("Negative register in ensure_reg");
    if (xinSTKUSE>=4 && r==R_SP && !xin_stkerrmsg)
        xin_stkerrmsg = 1,
        cc_err("Use of r7 inside interrupt-only (-zpu4) routine $r",
                currentfunction.symstr);
    return (SET_BITMAP)1 << r;
}

static void ensure_reg_rw(RealRegister r, bool wr)
{
    SET_BITMAP m = chkreg(r);
    if (debugging(DEBUG_LOCALCG))
        cc_msg("...ensure_reg_rw(%ld,%d)\n", r, wr);
    /* Don't start a fast (ALU-like) write to a register if there may   */
    /* be a slow write to that register (e.g. from a volatile load)     */
    /* still outstanding (think overtaking!).                           */
    if (wr && (m & waitm_list)) { outi1(OP_WAITM); }
    /* Don't do a read from a register until enough time has passed.    */
    if (!wr && (m & waitm_list)) { outi1(OP_WAITM); }
    if (!wr && (m & wait1_list)) { outi1(OP_NOP); }
}

static void ensure_reg_pair(RealRegister r1, RealRegister r2, int wr1)
{   /* check two regs at once, so we can prefer a WAITM over a NOP.     */
    /* wr1=0 means r1 is read; wr1=1 means r1 is written, but not       */
    /* in the next cycle; wr1=2 means r1 is written in the next cycle   */
    /* *without* being read (e.g. OP_MOVr), so we need to beware it     */
    /* overtaking a previous load from zero page memory.                */
    if (debugging(DEBUG_LOCALCG))
        cc_msg("...ensure_reg_pair(%ld,%ld,%d)\n", r1, r2, wr1);
    { SET_BITMAP m = chkreg(r1) | chkreg(r2);
      SET_BITMAP m2 = wr1==1 ? chkreg(r2) : m;
      if (m & waitm_list) { outi1(OP_WAITM); }
      if (m2 & wait1_list) { outi1(OP_NOP); }
    }
}

static void ensure_wait(bool wait1too)
{
    if (waitm_list != 0) { outi1(OP_WAITM); }
    if (wait1_list != 0 && wait1too) { outi1(OP_NOP); }
}

#define set_unready(r,pz)       ((pz) ? (wait1_list |= (1<<(r))) : \
                                        (waitm_list |= (1<<(r))))
#define clear_unready()         (waitm_list = wait1_list = 0)
#define clear_pzunready()       (wait1_list = 0)
#define ensure_reg(r) ensure_reg_rw(r,0)
#define ensure_reg_wr(r) ensure_reg_rw(r,1)
#define ensure_waitm() ensure_wait(0)
#define ensure_waitm_or_nop() ensure_wait(1)

static int isrom(Symstr *s)    /* only used if !xinLDDok */
{
    ExtRef *x = symext_(s);
    if (x == 0) return 0;
    return ((x->extflags & xr_constdata) ? 1 : 0);
}

static int ispagezero(Symstr *s)
{
    ExtRef *x = symext_(s);
    if (x == 0) return 0;
    return (x->extflags & xr_zeropage) ? 1 :
           (x->extflags & xr_immpage2) ? 2 : 0;
}

#define NONLEAF (PROC_ARGPUSH | PROC_ARGADDR | PROC_BIGSTACK | BLKCALL)

static Symstr *c4p_libfn(int op, int32 desc)
{
    switch (op)
    {
#ifdef never
**    case 0: /* Fix up condition codes in signed case.     */
**      return sym_insert_id("__fixup");
#endif
    case 1:
      return sym_insert_id("__shiftrs");
    case 2:
      return sym_insert_id("__shiftru");
    case 3:
      return sym_insert_id("__shiftl");
    case 4:
      return sym_insert_id("__callr0");
    }
    syserr("c4p_libfn(%d,0x%lx)", op, (long)desc);
    return sym_insert_id("internal-error-fn");
}

static void outcode16(int32 w)
{
    if ((codep & 2) == 0) {
      outcodeword(0xffffffff, LIT_OPCODE);
      codep -= 4;
    }
    code_hword_(codep) = (unsigned16)w;
    codep += 2;
}

/* vvvvvvvvvvvvvvvvvvvvvv    PEEPHOLER    vvvvvvvvvvvvvvvvvvvv */

int32 nec_opt = 0;             /* Control optimisations (via -zh012).   */
int32 c4p_pcs = 0;             /* Proc calling std (via -zhc012456).    */

enum { OUT_EXTCALL, OUT_EXTSYM,
       OUT_ZPSYMb, OUT_ZPSYMh, OUT_ZPSYMw,
       OUT_IN1, OUT_IN2, OUT_IN3,
       OUT_LABREF,
       OUT_LABDEF
     };

/* keep this in step with above enum type */
static int32 X_reloctype[] = { X_PCreloc, X_DataAddr,
                               X_ZPOFFb, X_ZPOFFh, X_ZPOFFw };

typedef struct mcpeepdata {
    struct mcpeepdata *next;
    int32 type;                   /* e.g. OUT_INSTR */
    int32 w;                      /* 32 bits of data */
    Symstr *s;                    /* external symbol (if needed) */
    unsigned32 off;               /* offset (if needed) */
    LabelNumber *lab;             /* if needed */
    int32 reftype;                /* label reference type */
} mcpeepdata;

static mcpeepdata *new_mcpeep(void);
static void emit_one_peeped(mcpeepdata *);
static void emit_all_peeped(void);
static int32 pcdraft;
static struct mcpeepdata *bb_list;

/* ^^^^^^^^^^^^^^^^^^^^^^    PEEPHOLER    ^^^^^^^^^^^^^^^^^^^^ */


#define FORCE 1
#define VIRTUAL 0
static void correct_stack(bool);
static int32 stack_move = 0, fp_minus_sp, firstargoff, firstlocoff, nargwords;
static int32 explicit_savesize = 0;
static int32 casebranch_pending = 0, casebranch_r1r;
static int32 adr1, adm, adr2; static bool adconpend;
static int32 advr1, advm; static bool adconvpend;
static LabelNumber *returnlab;

static int32 scc_regs, cmp_pending = 0;
static int32 cmp_pend_r2, cmp_pend_r3, cmp_pend_m, cmp_pend_q;

static int32 B_FROMQ(int32 q)
{
    switch (q)
      {
      case Q_UEQ:
      case Q_EQ:   return CC_Z;   /* Z Flag set */
      case Q_UNE:
      case Q_NE:   return CC_NZ;  /* Z Flag clear */
      case Q_HS:   return CC_NC;  /* C flag clear */
      case Q_LO:   return CC_C;   /* C flag set */
      case Q_GE:   return CC_NN;  /* ~(N^V) */
      case Q_LT:   return CC_N;   /* (N^V)  */
      /* case Q_PL:   return OP_BP; */
      /* case Q_MI:   return OP_BNP; */
/* The following cases should have been elimated by cmp_undefer...      */
      case Q_LE:
      case Q_GT:
      case Q_LS:
      case Q_HI:
      default:
        fprintf(stderr, "Q_PL=%lx Q_MI=%lx Q_LE=%lx Q_GE=%lx Q_XXX=%lx\n", Q_PL, Q_MI, Q_LE, Q_GE, Q_XXX);
        syserr(syserr_fromq, (long)q);
      case Q_AL:   return CC_AL;
      }
}


/* Try to avoid 32 bit immediate values in the loop optimiser. */
bool immed_cmp(int32 n) { return 0 <= n && n <= 0x7f; }

RealRegister local_base(Binder *b)
{
  int32 p = bindaddr_(b);
  switch (p & BINDADDR_MASK)
    {
    default: syserr(syserr_local_base, (long)p);
    case BINDADDR_ARG:
    case BINDADDR_LOC: return R_SP;
    }
}

int32 local_address(Binder *b)
{
  int32 p = bindaddr_(b);
  int32 q = p & ~BINDADDR_MASK;
  int32 r;
  correct_stack(VIRTUAL);
  switch (p & BINDADDR_MASK) {
  default: syserr(syserr_local_address, (long)p);
  case BINDADDR_LOC:    /* q = 4, 8, 12, 16, ... */
    r = /* R_SP + */ firstlocoff +  fp_minus_sp - q;
/*      printf("local_address(LOC): r=%ld q=%ld firstlocoff=%ld, fp_minus_sp=%ld\n", */
/*             r, q, firstlocoff, fp_minus_sp); */
    return r;
  case BINDADDR_ARG:    /* q = 0, 4, 8, 12, ... */
    correct_stack(FORCE);               /* @@@ check LDRV1 otherwise */
    if (!(procflags & PROC_ARGPUSH)) {
      /* this should be used for LDR(f/v)1 only, check a bit */
      if (nargwords <= NARGREGS || fp_minus_sp != 0)
        syserr(syserr_local_addr);
    }
    r = /* R_SP + */ firstargoff +  fp_minus_sp + q;
/*      printf("local_address(ARG): r=%ld q=%ld firstargoff=%ld, fp_minus_sp=%ld\n", */
/*             r, q, firstargoff, fp_minus_sp); */
    return r;
  }
}

static int firstbit(int32 w)
{
    int i;
    for (i = 0; i < 32; i++) if (w & ((int32)1<<i)) return i;
    syserr(syserr_firstbit);
    return 0;
}

static void outi1(int32 w)
{
  mcpeepdata *mc_peep1 = new_mcpeep();
  mc_peep1->type = OUT_IN1;
  mc_peep1->w = w;
  if (w >> 16) syserr("bad instruction %lx", (long)w);
  emit_one_peeped(mc_peep1);
}

static void outi2(int32 w, int32 d)
{
  mcpeepdata *mc_peep1 = new_mcpeep();
  mc_peep1->type = OUT_IN2;
  mc_peep1->w = w | d<<16;
  emit_one_peeped(mc_peep1);
}

static void outRext(int out_type, int32 op, Symstr *name, int32 off)
{
/* @@@ NB NB NB. If an adcon is just being used once then it is best to */
/* share its 32 bits in a MOVHI and a mem-ref offset.  For multiple     */
/* refs to data segment it is better to load ALL bits with              */
/* MOVHI+MOVEA and then use small displacements.                        */
/*  fprintf(stderr, "calling outRext %lx\n", op); */
    mcpeepdata *mc_peep1 = new_mcpeep();
    mc_peep1->type = out_type;
    mc_peep1->w = op;
    mc_peep1->s = name;
    mc_peep1->off = off;
    emit_one_peeped(mc_peep1);
}

static void outcall(int32 op, Symstr *name)
     /* the obj_symref and codexrefs data structures may be mergeable. */
{
    mcpeepdata *mc_peep1 = new_mcpeep();
    obj_symref(name, xr_code, 0);       /* create symext_() structure. */
    ensure_waitm();
/* We cannot exploit the back-call info possibly given by obj_symref()  */
/* because scheduling may invalidate it.                                */
/* Hence delay such resolution to object module formatting.             */
/* This is a shame because it means we cannot use Bcond for TAILCALL   */
    mc_peep1->type = OUT_EXTCALL;
    mc_peep1->w    = op;
    mc_peep1->s      = name;
    emit_one_peeped(mc_peep1);
}

static void outpush(RealRegister r)
{
    ensure_reg(r);
    outi1(OP_PUSH | RD(r));
}

static void outpop(RealRegister r)
{
    ensure_reg_wr(r);
    outi1(OP_POP | RD(r));
    set_unready(r, TRUE);
}

static void load_integer(RealRegister r, int32 n)
     /* Set register r to the integer n. */
{
    ensure_reg_wr(r);
    if (-128 <= n && n < 128)
    { /* The following line puts in a NOP so that a fast memory load    */
      /* (e.g. POP) cannot get delayed until after the MOV.I.           */
      /* See also the comment for ensure_reg_pair(.,.,2).               */
      ensure_reg(r);
      outi1(OP_MOVI_S | RD(r) | (n&0xff));
    }
    else if (n > -0x7fffff && n < 0x7fffff)
      outi2(OP_MOVI_L | RD(r) | (n>>16)&0xff , n&0xffff);
    else
    {   /* this code is sub-optimal; we can often save a halfword by    */
        /* using any single-halfword instruction (e.g. SHIFT, ADD) etc. */
      outi2(OP_MOVI_D | RD(r), n&0xffff);
      outi1((n>>16)&0xffff);
    }
}

static void move_register(RealRegister r1, RealRegister r2)
{
    if (r1 != r2) {
      ensure_reg_pair(r1, r2, 2);
      outi1(OP_MOVr | RD(r1) | RS(r2));
    }
}

static void imm_op(int32 op, RealRegister r1, RealRegister r2, int32 n)
{
    /* r1 = r2 <op> n */
    if (op == OP_ADDr) {
      /* not yet optimal for plus of 32 bit numbers */
      if (n == 0)
        move_register(r1, r2);
      else if (r1==r2 && immed_cmp(n)) {
        /* MOVI then ADDi allows a larger range of values if r1!=r2 */
        ensure_reg(r1);
        outi1(OP_ADDi | RD(r1) | (n<<1));
      }
      else if (r1==r2 && immed_cmp(-n)) {
        ensure_reg(r1);
        outi1(OP_SUBi | RD(r1) | ((-n)<<1));
      }
      else if (r1==r2) {
        load_integer(R_LL, n);
        if (r1 == R_LL || lock_r0) syserr("immop(OP_ADD)");
        ensure_reg(r1);
        outi1(OP_ADDr | RD(r1) | RS(R_LL));
      }
      else {
        load_integer(r1, n);
        ensure_reg(r2);
        outi1(OP_ADDr | RD(r1) | RS(r2));
      }
    }
    else if (op == OP_CMPr) {  /* asym, but swap cc's? */
      if (immed_cmp(n)) {   /*  **** Check this **** */
        ensure_reg(r2);
        outi1(OP_CMPi | RD(r2) | (n<<1));
      }
      else {
        load_integer(R_LL, n);
        if (r1 == R_LL || lock_r0) syserr("immop(OP_CMP)");
        ensure_reg(r2);
        outi1(OP_CMPr | RD(r2) | RS(R_LL));
      }
    }
    else if (op == OP_SUBr) {  /* asym */
      move_register(r1, r2);
      if (immed_cmp(n)) {
        ensure_reg(r1);
        outi1(OP_SUBi | RD(r1) | (n<<1));
      }
      else if (immed_cmp(-n)) {
        ensure_reg(r1);
        outi1(OP_ADDi | RD(r1) | ((-n)<<1));
      }
      else {
        load_integer(R_LL, n);
        if (r1 == R_LL || lock_r0) syserr("immop(OP_SUB)");
        ensure_reg(r1);
        outi1(OP_SUBr | RD(r1) | RS(R_LL));
      }
    }
    else if (op == OP_ANDr) {
         /* could also do <<k then >>k */
      if (n == -1)
        move_register(r1, r2);
      else if (r1==r2) {
        load_integer(R_LL, n);
        if (r1 == R_LL || lock_r0) syserr("immop(OP_AND)");
        ensure_reg(r1);
        outi1(OP_ANDr | RD(r1) | RS(R_LL));
      }
      else {
        load_integer(r1, n);
        ensure_reg(r2);
        outi1(OP_ANDr | RD(r1) | RS(r2));
      }
    }
    else if (op == OP_ORr || op == OP_XORr) {
      if (n == 0)
        move_register(r1, r2);
      else if (r1==r2) {
        load_integer(R_LL, n);
        if (r1 == R_LL || lock_r0) syserr("immop(OP_OR)");
        ensure_reg(r1);
        outi1(op | RD(r1) | RS(R_LL));
      }
      else {
        load_integer(r1, n);
        ensure_reg(r2);
        outi1(op | RD(r1) | RS(r2));
      }
    }
    else  {
      printf("imm_op %lx %ld %ld %ld failed\n", op, r1, r2, n);
      syserr("imm_op");
    }
}

static void rr_op(int32 op, RealRegister r1, RealRegister r2, RealRegister r3)
{
    if (r1 == r2) {
      ensure_reg_pair(r2, r3, 0);
      outi1(op | RD(r1) | RS(r3));
    }
    else if (r1 == r3) {
      if (op == OP_SUBr) syserr("rr_op(OP_SUB)");
      ensure_reg_pair(r2, r3, 0);
      outi1(op | RD(r1) | RS(r2));
    }
    else {
      /* We *should* do a WAITM here, rather than in ensure_reg(r3)     */
      /* exactly in the circumstance that r3 needs a WAITM and r2 needs */
      /* a NOP.  (Note also that r1!=r2).                               */
      move_register(r1,r2);     /* Effects ensure_reg_pair(r1,r2,2);    */
      ensure_reg(r3);
      outi1(op | RD(r1) | RS(r3));
    }
}

#define cmp_defer(type, a,b,m,q) \
  (cmp_pending=type, cmp_pend_r2=a, cmp_pend_r3=b, cmp_pend_m=m, cmp_pend_q=q)

static int32 Q_gt2ge(int32 cond)
{   switch (cond)
    {
default:   return -1;
case Q_GT: return Q_GE;
case Q_LE: return Q_LT;
case Q_HI: return Q_HS;
case Q_LS: return Q_LO;
    }
}

static int32 cmp_undefer(int32 brcond)
{
    int32 r2 = cmp_pend_r2, r3 = cmp_pend_r3, m = cmp_pend_m;
    int32 cmpcond = cmp_pend_q;
        /* correct_stack(FORCE);   DONE BY CALLER */
    if (!cmp_pending) return brcond;
    if (brcond == Q_AL) return brcond;
    if (cmpcond != brcond)
      syserr("cmp_defer confused %lx, %lx", cmpcond, brcond);

    if (cmp_pending==3) {         /* AND/OR/XOR then CMP #0 type */
      if (stack_move)
        syserr("TSTIBK/stack_move %ld", stack_move), outi1(OP_ILLEGAL);
      return brcond;
    }
    else if (cmp_pending==2) {   /* rr type */
/*        printf("cmp: r2=%ld r3=%ld q=%lx\n", r2, r3, cmpcond); */
      switch (cmpcond) {
case Q_GT: case Q_LE: case Q_HI: case Q_LS:
        { int32 tmp = r2; r2 = r3; r3 = tmp;
          cmpcond = Q_swap(cmpcond);
        }
      }
      rr_op(OP_CMPr, r2, r2, r3);
#ifdef never
**      if (cmpcond==Q_LT || cmpcond==Q_GE)
**        /* If signed cmp then fix up N bit to be N^V */
**        outcall(OP_CALL_L | (CC_V<<8), c4p_libfn(0,0));
#endif
      if (stack_move)
        syserr("CMPr/stack_move %ld", stack_move), outi1(OP_ILLEGAL);
      return cmpcond;
    }
    else {                       /* rk type */
      if (m==0x7fffffff && cmpcond==Q_LE) cmpcond=Q_AL;
      else if (m==0x7fffffff && cmpcond==Q_GT) return Q_NOT;
      else if (m==0xffffffff && cmpcond==Q_LS) cmpcond=Q_AL;
      else if (m==0xFfffffff && cmpcond==Q_HI) return Q_NOT;
      switch (cmpcond) {
case Q_GT: case Q_LE: case Q_HI: case Q_LS:
        cmpcond = Q_gt2ge(cmpcond);
        m = m+1;              /* cannot overflow now */
      }
      imm_op(OP_CMPr, r2, r2, m);
      if (cmpcond==Q_LT || cmpcond==Q_GE)
#ifdef never
**        /* If signed cmp then fix up N bit to be N^V */
**        if (m != 0)
**          /* But only bother when CMP r,0 can actually set V... */
**          outcall(OP_CALL_L | (CC_V<<8), c4p_libfn(0,0));
#endif
      if (stack_move)
        syserr("CMPK/stack_move %ld", stack_move), outi1(OP_ILLEGAL);
      return cmpcond;
    }
}

static void imm_shift(int32 op, RealRegister r1, RealRegister r2, int32 m)
{
    if (m & ~31) syserr("odd shift");
    m = m & 31;
    move_register(r1, r2);
    ensure_reg(r1);
    if (xinSHIFT && m>=24) { 
      outi1(((op&4) ? OP_SHRI24:OP_SHLI24) | RD(r1));
      m -= 24;
    }
    if (xinSHIFT && m>=16) { 
      outi1(((op&4) ? OP_SHRI16:OP_SHLI16) | RD(r1));
      m -= 16;
    }
    while (m>8) {
      outi1(op | RD(r1) | RS(7));
      m -= 8;
    }
    if (m!=0) outi1(op | RD(r1) | RS(m-1));
}

static void imm_shiftur(int32 op, RealRegister r1, RealRegister r2, int32 m)
{
    if (m & ~31) syserr("odd shift");
    m = m & 31;
    move_register(r1, r2);
    if (m>0)
    {   /* Shift single bit,  long,   right,  zero added */
        ensure_reg(r1);
        outi1(OP_SHIFT | (0x2<<5) | (0x1<<2) | 0 | RD(r1));
        imm_shift(op, r1, r1, m-1);
    }
}

static void rr_shift(Symstr *fn, RealRegister r1r, RealRegister r2r,
                     RealRegister mr)
{   /* @@@ ugly, long, code -- simulate C call */
    /* R_A1 = 1 assumed */
    {   if (r1r != 1) outpush(1);
        if (r1r != 2) outpush(2);
        if (r1r != 3) outpush(3);
        if (mr == 1) { move_register(R_LL, mr), mr = R_LL; }
        move_register(1, r2r);
        move_register(2, mr);
        outcall(OP_CALL_L | (CC_AL<<8), fn);
        move_register(r1r, 1);
        if (r1r != 3) outpop(3);
        if (r1r != 2) outpop(2);
        if (r1r != 1) outpop(1);
    }
}

static void imm_extend(RealRegister r1, RealRegister r2, int32 m)
{
    if (!(0<=m && m<=2)) syserr("odd extend");
    move_register(r1, r2);
  /* The code here follows that in flowgraf.c:                    */
    {
        int32 k = (m==2) ? 16 : 24;
        imm_shift(OP_SHLI, r1, r1, k);
        imm_shift(OP_SHRI, r1, r1, k);
    }
}

#define D_div  0
#define D_rem  1

/* Multiply (J_MULK) special case flags */
#define MV_s16 1        /* arg is a (short) value, result needs 32 bits */
#define MV_j16 2        /* result needs (junk padded) 16 bits           */
#define MV_s32 3        /* arg and result are both 32 bit values.       */

static void imm_mult(RealRegister r1, RealRegister r2, int32 n, int validrange)
{
    unsigned32 m = n, s = 0;
    while (m != 0 && (m&1) == 0) m>>=1, s++;
/*      printf("imm_mult n=%ld m=%ld s=%ld validrange=%d\n", n, m, s, validrange); */
    if (n == 0)
      load_integer(r1, 0);
    else if (m == 1)
      imm_shift(OP_SHLI, r1, r2, s);
    else if ((m+1 << s) == 0) {         /* i.e. -1,-2,-4,-8 ... 2^31 */
      imm_shift(OP_SHLI, R_LL, r2, s);
      load_integer(r1, 0);
      rr_op(OP_SUBr, r1, r1, R_LL);
    }
    else if ((m-1 & m-2) == 0)  /* 2^k+1, here (1),(2),3,5,9,17,...2^31+1 */
    {   int k = firstbit(m-1);
        if (r1 == r2) move_register(R_LL, r1);
        imm_shift(OP_SHLI, r1, r2, k);
        rr_op(OP_ADDr, r1, r1, r1==r2 ? R_LL : r2);
        imm_shift(OP_SHLI, r1, r1, s);
    }
    else if ((m+1 & m) == 0)    /* 2^k-1, here (-1)(0)(1)(3),7,15,...2^31-1 */
    {   int k = firstbit(m+1);
        if (r1 == r2) move_register(R_LL, r1);
        imm_shift(OP_SHLI, r1, r2, k);
        rr_op(OP_SUBr, r1, r1, r1==r2 ? R_LL : r2);
        imm_shift(OP_SHLI, r1, r1, s);
    }
#ifdef buggy_because_of_cutandpaste
    else if (((-m)+1 & (-m)) == 0)  /* 1-2^k: (1)(0)(-1),-3,-7,...,(-2^31+1) */
    {   int k = firstbit((-m)+1);
        if (r1 == r2) move_register(R_LL, r1);
        imm_shift(OP_SHLI, r1, r2, k);
        rr_op(OP_SUBr, r1, r1, r1==r2 ? R_LL : r2);
        imm_shift(OP_SHLI, r1, r1, s);
    }
    else if (((-m)-1 & (-m)-2) == 0)  /* -1-2^k: (-2),(-3),-5,...,(-2^31-1) */
    {   int k = firstbit((-m)-1);
        if (r1 == r2) move_register(R_LL, r1);
        imm_shift(OP_SHLI, r1, r2, k);
        rr_op(OP_ADDr, r1, r1, r1==r2 ? R_LL : r2);
        imm_shift(OP_SHLI, r1, r1, s);
        imm_op(OP_SUBr, r1, r1, 0);
    }
#endif
#ifdef not_needed
    else if (validrange == MV_s16)
    {   int32 nhi = ((hi1(n) & 0xffff) ^ 0x8000) - 0x8000;
        int32 nlo = ((n & 0xffff) ^ 0x8000) - 0x8000;
        if (nhi != 0) imm_op(OP_MUL, R_LR, r2, nhi);  /* mulhi #hi,r2,t2 */
        imm_op(OP_MUL, r1, r2, nlo);       /* mulhi #lo,r2,r1 */
        if (nhi != 0)
          {   imm_shift(OP_SHLI, R_LR, R_LR, 16);      /* shl #16,t2 */
          rr_op(OP_ADDr, r1, r1, R_LR);                /* add r1,t2 */
        }
    }
#endif
    else {
      int32 nhi = ((hi1(n) & 0xffff) ^ 0x8000) - 0x8000;
      int32 nlo = ((n & 0xffff) ^ 0x8000) - 0x8000;
      int ra = 4, rc = 5;       /* MUL requires args in r4/r5.          */
      if (r2 == 5) ra = 5, rc = 4;
      if (r1 != 5) outpush(5);
      if (r1 != 4) outpush(4);
      if (r1 != 3) outpush(3);
      if (r1 != 2) outpush(2);
      /* Two special cases of the more general code in the 'else' part. */
      if (nlo == 0)
      { move_register(ra, r2);
        load_integer(rc, nhi);
        outi1(OP_MUL);
        imm_shift(OP_SHLI, r1, 2, 16);
      }
      else if (validrange == MV_j16)
      { move_register(ra, r2);
        load_integer(rc, nlo);
        outi1(OP_MUL);
        move_register(r1, 2);
      }
      else
      { /* fiddle a little to achieve R_LR=r2 PAR ra=r2^0x8000;         */
        if (r2 == ra)
        { move_register(R_LR, r2);          /* A safe temporary   */
          load_integer(rc, 0x8000);         /* Don't corrupt R_LL */
          rr_op(OP_ADDr, ra, ra, rc);       /* ra = arg + 0x8000  */
        }
        else
        { imm_op(OP_ADDr, ra, r2, 0x8000);  /* ra = arg + 0x8000  */
          move_register(R_LR, r2);          /* A safe temporary   */
        }
        imm_shift(OP_SHRI, ra, ra, 16);
        load_integer(rc, nlo);
        outi1(OP_MUL);                    /* mul16 hi(arg),lo(n) */
        move_register(ra, R_LR);          /* get arg back -- or OP_POP?      */
        if (nhi != 0)
        {   if (nhi != nlo) load_integer(rc, nhi);
            outi1(OP_MACC);               /* mul16 lo(arg),hi(n) */
            if (nhi != nlo) load_integer(rc, nlo);
        }
        imm_shift(OP_SHLI, 2, 2, 16);     /* shift partial answer */
        outi1(OP_MACC);                   /* add in mul16 lo(arg),lo(n)   */
        move_register(r1, 2);
      }
      if (r1 != 2) outpop(2);
      if (r1 != 3) outpop(3);
      if (r1 != 4) outpop(4);
      if (r1 != 5) outpop(5);
    }
}

static void outrelref_LDD_synth(RealRegister r1, RealRegister r2, int32 d, int pz)
{
/* Do the following to synthesise load.d from load.w...
**          load.w  r1,(r2+0)   ; lsw
**          load.w  r0,(r2+2)   ; msw -- OK even if r2==r0 (see d>29 above).
**          waitm
**          sla     r0,16
**          sla     r1,16
**          srz.l   r1
**          sra     r1,8
**          sra     r1,7
**          or      r1,r0
*/  
      outi1(OP_LD_Wr | RD(r1) | RS(r2) | d);
      set_unready(r1, pz);
      outi1(OP_LD_Wr | RD(R_LL) | RS(r2) | d+2);
      set_unready(R_LL, pz);
      imm_shift(OP_SHLI, r1, r1, 16);
      imm_shiftur(OP_SHRI, r1, r1, 16);
      imm_shift(OP_SHLI, R_LL, R_LL, 16);
      rr_op(OP_ORr, r1, r1, R_LL);
}

static void outrelref(int32 op, RealRegister r1, RealRegister r2, int32 d, int r1live)
{   /* not valid for OP_ST when r1 = R_LL unless 0<=d<31 */
    /* Note also that we play safe w.r.t. big offsets and use WAITM     */
    /* instead of NOP if the offset exceeds 31.                         */
    ensure_reg_pair(r1, r2, opisLOAD(op));
    if (!(0 <= d && d <= ((op==OP_ST_Lr && !xinSTDok ||
                           op==OP_LD_Lr && !xinLDDok    ) ? 29 : 31))) {
      if (!opisLOAD(op) && r1 == R_LL) syserr("outrelref(R_LR)");
      if (lock_r0) syserr("outrelref(lock_r0)");
      imm_op(OP_ADDr, R_LL, r2, d);
      d = 0; r2 = R_LL;
    }
    if (op==OP_LD_Lr && !xinLDDok) {     /* Nasty 32bit case */
      outrelref_LDD_synth(r1, r2, d, r2==R_SP && xinSTKUSE==3);
    }
    else if (op==OP_ST_Lr && !xinSTDok) {     /* Nasty 32bit case */
      /* outrelref_STD_synth(r1, r2, d, r2==R_SP && xinSTKUSE==3, r1live); */
      outi1(OP_ST_Wr | RD(r1) |  RS(r2) | d);
      /* clear_unready(); */
      imm_shift(OP_SHRI, r1, r1, 16);
      outi1(OP_ST_Wr | RD(r1) | RS(r2) | (d+2));
      if (r1live) {
        /* restore r1 by re-loading it */
        outi1(OP_LD_Lr | RD(r1) | RS(r2) | d);
        set_unready(r1, r2==R_SP && xinSTKUSE==3);
      }
    }
    else {
      outi1(op | RD(r1) | RS(r2) | d);
      if (opisLOAD(op)) set_unready(r1, r2==R_SP && xinSTKUSE==3);
      /* else clear_unready(); */
    }
}

static void correct_signedness(RealRegister r, int32 flgs)
{
    if (flgs & J_UNSIGNED) switch (flgs & 3)
    {
    case 1: imm_op(OP_ANDr, r, r, 255); break;
    case 2: imm_op(OP_ANDr, r, r, 0xffff); break;
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

int32 local_fpaddress(int32 p)   /* not adapted for C4P yet !! */
       /* exported for debugger */
{   /* p is bindaddr_() of the relevant binder */
#ifdef FIX_THIS_UP_SOME_DAY
    if (procflags & NONLEAF) switch (p & BINDADDR_MASK) {
    case BINDADDR_LOC:
      p &= ~BINDADDR_MASK;
      if (argspushed != 0) p += 4*argspushed;
      return -(p + 12 + 4*bitcount(regmask & M_VARREGS));
    case BINDADDR_ARG:
      p &= ~BINDADDR_MASK;
      return (p >= 4*argsbelowfp) ?
        p - 4*argsbelowfp + 4 :
          p - 4*argsbelowfp - 4*bitcount(regmask & M_VARREGS) - 12;
    }
#else
    return 0;
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

static void setlab1(LabelNumber *l)
{
    mcpeepdata *mc_peep1 = new_mcpeep();
/*      printf("setlab1 L%ld\n", lab_name_(l)); */
    ensure_waitm_or_nop();      /* force waitm/nop to be before a label. */
    mc_peep1->type = OUT_LABDEF;
    mc_peep1->lab = l;
    emit_one_peeped(mc_peep1);
}

static void setlab2(LabelNumber *l)
{
    List *p = l->u.frefs;

    if (asmstream) asm_lablist = mkLabList(asm_lablist, l);
    while (p!=NULL) {
      int32 v = car_(p);
      int32 q = (v & 0x00ffffff);    /* byte offset from proc start */
      unsigned32 w = code_hword_(q);
      switch (v & 0xff000000) {
      case LABREF_DISP9:
        { int32 w1 = (codep-q-2) >> 1; /* half-words! */
        if (w1 > 127u) syserr(syserr_back_coderef, (long)w);
        w |= (w1&0xff);
/*          printf("L%ld LABREF_DISP9: codep=%lx q=%lx w=%.4x w1=%.4lx => %.4lx\n", */
/*                 lab_name_(l), codep, q, code_hword_(q), w1, w); */
        }
        break;
      case LABREF_DISP24:
        { int32 w2 = (codep-q-4) >> 1; /* half-words! */
/*            printf("L%ld LABREF_DISP24: codep=%lx q=%lx w=%.4lx %.4x =>", */
/*                   lab_name_(l), codep, q, w, code_hword_(q+2)); */
          w |= (w2 >> 16) & 0xff;
          code_hword_(q+2) = (unsigned16)(w2 & 0xffff);
/*            printf(" %.4lx %.4x\n", w, code_hword_(q+2)); */
        }
        break;
      default:
        syserr(syserr_unknown_labref_type, (long)v);
      }
      code_hword_(q) = (unsigned16)w;
      p = (List *)discard2(p);
    }
    lab_setloc_(l, codep /* | 0x80000000 */); /* cheapo union checker for ->frefs */
}

static void correct_stack(bool exact)
{
    int32 move = stack_move;
    if ((exact && move!=0) || move < STACK_NEG_EXTENSION || move >= 32) {
/*        printf("Stack_move: %ld %ld (%d)\n", stack_move, move, exact); */
      imm_op(OP_ADDr, R_SP, R_SP, move);
      stack_move = stack_move - move; /* = 0 ! */
    }
/*      else printf("Stack_move: do nothing for %ld (%d)\n", stack_move, exact); */
}

static void push_iregs(int32 mask)
{   /* push onto system stack */
    while (mask) {
      int32 r1 = firstbit(mask);
      outpush(r1);
      mask ^= 1L<<r1;
    }
}

static void pop_iregs(int32 mask)
{   /* pop from system stack */
    int r1;
    for (r1 = 31; r1>=0; r1--)
      if (regbit(r1) & mask)
        outpop(r1);
}

static void save_iregs(int32 mask, int32 off, int live)
{
    if (off<0 || off+4*(bitcount(mask)-1) >= 32)
      syserr("save_iregs(%ld)", off);
/*      fprintf(stderr, "mask=%lx\toff=%ld\n",mask, off); */
    while (mask) {
      int32 r1 = firstbit(mask);
      outrelref(OP_ST_Lr, r1, R_SP, off, live);
      mask ^= 1L<<r1;
      off += 4;
    }
}

static void restore_iregs(int32 mask, int32 off)
{
    if (off<0 || off+4*(bitcount(mask)-1) >= 32)
      syserr("restore_iregs(%ld)", off);
    while (mask) {
      int32 r1 = firstbit(mask);
      outrelref(OP_LD_Lr, r1, R_SP, off, 0);
      mask ^= 1L<<r1;
      off += 4;
    }
}

static void routine_entry(int32 mm)
{
    int32 m = k_argwords_(mm);
    int32 n = (m>NARGREGS ? NARGREGS : m); /* regs to be pushed if nec. */
    int32 amask = regbit(R_A1+n) - regbit(R_A1);
    int32 smask = regmask & M_VARREGS;
    int32 vmask = xinCALL(procflags) ? (smask | regbit(R_LR)) : 0;
    int32 vsize = 4*bitcount(vmask);
#ifdef TARGET_CALL_PUSHPOP_LIBRARY
    if (vmask & M_VARREGS && (config & CONFIG_OPTIMISE_TIME)) {
      /* there is no c4p_libfn() to save M_VARREGS without R_LR.      */
      vsize = 4*bitcount(vmask);
    }
#endif
/*      printf("m %lx, amask %lx, vmask %lx, regmask %lx\n", m,amask,vmask,regmask);  */
    fp_minus_sp = 0;
    returnlab = nextlabel();
    nargwords = m;
    explicit_savesize = vsize;
/* The following two values are w.r.t. fp_minus_sp==0.                  */
    firstlocoff = 0;
    firstargoff = vsize;
    if (procflags & (PROC_ARGPUSH|PROC_ARGADDR))
        explicit_savesize += 4*n;
    else
        firstargoff -= 4*n;
    stack_move = -explicit_savesize;
/*      printf("%d: stack_move=%ld firstlocoff=%ld firstargoff=%ld\n", __LINE__, */
/*         stack_move, firstlocoff, firstargoff); */
    if (xinCALL(procflags)) {
      /* Pop return address to r0, except for "__main", the entry point */
      if (strcmp(symname_(currentfunction.symstr), "__main") != 0)
          outpop(R_LR);
      lock_r0 = TRUE;
    }
/* Note the next line is safe w.r.t. lock_r0 since stack_move < 128.    */
    correct_stack(FORCE);     /* since STACK_NEG_EXTENSION==0           */
/* now save the things we've made space for...                          */
    if (procflags & (PROC_ARGPUSH|PROC_ARGADDR))
        save_iregs(amask, firstargoff+stack_move, 1);
/*  printf("%d: stack_move=%ld firstlocoff=%ld firstargoff=%ld\n", __LINE__, */
/*         stack_move, firstlocoff, firstargoff); */
#ifdef TARGET_CALL_PUSHPOP_LIBRARY
    if (vmask && !(config & CONFIG_OPTIMISE_TIME) &&
          /* don't use a proc call for a single push unless ADDI folds. */
        !(vsize == 4 && procflags & (PROC_ARGPUSH|PROC_ARGADDR))) {
      stack_move += vsize;
      correct_stack(FORCE);
      outcall(OP_CALL_L | (CC.AL<<8), c4p_libfn(99, vmask));
      /* stack_move==0 is still correct here. */
    }
    else
#endif
      if (xinCALL(procflags))
        save_iregs(vmask, firstlocoff+stack_move, 0);
      else
        push_iregs(smask);
    lock_r0 = FALSE;
    scc_regs = 0;
    adconpend = 0;
    adconvpend = 0;
    cmp_pending = 0;
}

static void routine_exit(bool returning)
{
    int32 smask = regmask & M_VARREGS;
    int32 vmask = xinCALL(procflags) ? (smask | regbit(R_LR)) : 0;
    int32 vsize = 4*bitcount(vmask);
    int32 stack_reset;
/*      printf("Exit:vmask 0x%lx, vsize=%ld\n", vmask,vsize); */
#ifdef TARGET_CALL_PUSHPOP_LIBRARY
    if (vmask & M_VARREGS && (!(config & CONFIG_OPTIMISE_TIME))) {
      /* there is no c4p_libfn() to restore M_VARREGS without R_LR.   */
      vmask |= regbit(R_LR);
      vsize = 4*bitcount(vmask);
    }
    if (vmask && (!(config & CONFIG_OPTIMISE_TIME)) &&
          /* don't use a proc call for a single pop unless ADDI folds.  */
          !(vsize == 4 && procflags & (PROC_ARGPUSH|PROC_ARGADDR))) {
      stack_move += fp_minus_sp;
      correct_stack(FORCE);    /* (part of offset) stack_move+fp_minus_sp */
      stack_reset = explicit_savesize - vsize;
      if (procflags & BLKCALL) {
        stack_move += 4;
        correct_stack(FORCE);
        ensure_all();
        outi1(OP_MOV2PC);
        returning = 0;
      }
      else if (stack_reset == 0 && returning) {
        correct_stack(FORCE);    /* (part of offset) stack_move+fp_minus_sp */
        outcall(OP_CALL_L | (CC.AL<<8), c4p_libfn(99, vmask)); /* SP+=vsize */
        returning = 0;
      }
      else {
        ensure_all();             /* Finish any loads */
        correct_stack(FORCE);    /* (part of offset) stack_move+fp_minus_sp */
        outi1(OP_RET); /* SP+=vsize */
      }
    }
    else
#endif
    {
      int32 off = stack_move + fp_minus_sp + firstlocoff;
      if (vmask != 0 && (off<0 || off+4*(bitcount(vmask)-1) >= 32)) {
        /* ensure we don't need to use R_LR to calculate an address */
        imm_op(OP_ADDr, R_SP, R_SP, off);
        stack_move -= off;
        off = 0;
      }
      stack_reset = stack_move + fp_minus_sp + explicit_savesize;
      if (xinCALL(procflags))
      {   lock_r0 = TRUE;
          restore_iregs(vmask, off);
      }
      else
          pop_iregs(smask);
    }
    if (stack_reset) stack_move = stack_reset;
    correct_stack(FORCE);
    if (returning) {
      if (xinCALL(procflags)) {
        ensure_waitm();
        ensure_reg(R_LR); outi1(OP_MOV2PC); lock_r0 = FALSE;
      }
      else {
        ensure_waitm();
        outi1(OP_RET);
      }
    }
    else {
      if (xinCALL(procflags)) {
        outpush(R_LR); lock_r0 = FALSE;
      }
    }
}

enum BR_Reason { BR_normal, BR_bxx, BR_endproc };

/* AM: treat the special value RETLAB of destination as return address */
static void conditional_branch_to(int32 condition, LabelNumber *destination,
                                  enum BR_Reason reason)
{
    mcpeepdata *mc_peep1;
    if (condition==Q_NOT) return;
    ensure_waitm();
    if (destination == RETLAB) {
      destination = returnlab;
/*
 * If I have an exit the USUALLY it just gets mapped into a branch to the
 * special label "returnlab". But if I find an unconditional branch then
 * there are two special cases:
 * .  If there were no registers that needed restoring from the stack
 *    then I generate the return sequence in-line. This is because in
 *    such cases the exit code is only 2 or 3 instructions so the branch
 *    away is unnecessary overhead.
 * .  If there ARE registers that need restoring then the first time that
 *    I see an unconditional branch I set returnlabel and expand the
 *    exit in-line. If at the end of a procedure I have not yet set the
 *    destination label but I have referred to it (eg from a conditional
 *    branch) I set it there and insert exit code.
 */
      if (condition == Q_AL && reason != BR_bxx) {
        if (reason == BR_endproc || (config & CONFIG_OPTIMISE_TIME)) {
          /* i.e. the old code, a return at each possible place.  */
          routine_exit(1);
          return;
        }
        if ((regmask & M_VARREGS) == 0) {
          if (destination->lndraft >= 0x7ffffffe && stack_move == 0 &&
              (config & CONFIG_OPTIMISE_SPACE)) {
/*              printf("setlab1 called at line %d\n", __LINE__); */
            setlab1(destination);     /* for Bcond/BXX */
          }
          routine_exit(1);
          return;
        }
        if (stack_move != 0) {
          routine_exit(1);
          return;
        }
            /* >= 0x7ffffffe means 'unset', possibly referenced.        */
        if (destination->lndraft >= 0x7ffffffe) {
          /* correct_stack(FORCE) is a noop given test above.         */
/*            printf("setlab1 called at line %d\n", __LINE__); */
          setlab1(destination);
          routine_exit(1);
          return;
        }
      }
    }
    if (stack_move != 0)
      syserr("conditional_branch_to(%ld!=0)", (long)stack_move);
#define reflab(l) ((l)->lndraft &= ~1)
    reflab(destination);
    mc_peep1 = new_mcpeep();
    /* This is where I need to discriminate between the two sorts of things  */
    /* that I put in the ->frefs field of a label entry.                     */
    /* u.defn shares with u.frefs */
    mc_peep1->type = OUT_LABREF;
    if (reason==BR_bxx) {
      mc_peep1->reftype = LABREF_DISP24;
      mc_peep1->w =  OP_JUMP_L | (B_FROMQ(condition)<<8);
    }
    else {
      mc_peep1->reftype = LABREF_DISP9;
      /* Might output fixup code */
      mc_peep1->w = OP_JUMP_S | (B_FROMQ(condition)<<8);
    }
    mc_peep1->lab = destination;
    emit_one_peeped(mc_peep1);
}

static void flush_adconv(void)
{
    if (adconvpend) {
/*        fprintf(stderr,"flush_adconv %ld %ld", advr1, advm); */
      imm_op(OP_ADDr, advr1, R_SP, advm);
      adconvpend = 0;
    }
}

static void flush_adcon(void)
{
    if (adconpend) {
      Symstr *name = (Symstr *)adm;
      int32 offset = (int32)adr2;
/*        fprintf(stderr,"flush_adcon %ld %ld\n", adr1, adr2); */
      /* OP_MOVI_S won't work for pagezero address (s8 instead of u10). */
      outRext(OUT_EXTSYM, OP_MOVI_L | RD(adr1), name, offset);
      scc_regs = scc_regs&(~regbit(adr1));
      adconpend = 0;
    }
}

static void c4p_clrc(RealRegister r1, int32 off1, int32 m)
{
    if (m != 0) {
      if ((m & 3) == 0 && m < 100) {
        int32 i;
        load_integer(R_LL, 0);
        for (i=0; i<m; i+=4)
          outrelref(OP_ST_Lr, R_LL, r1, off1+i, 0);
      }
      else syserr("c4p_clrc %ld", (long)m);
    }
}

static void c4p_movc(RealRegister r1, int32 off1, RealRegister r2, int32 m)
{
    if (m != 0) {
      if ((m & 3) == 0 && m < 100) {
        int32 i;
        for (i=0; i<m; i+=4) {
          outrelref(OP_LD_Lr, R_LL, r2, i, 0);
          outrelref(OP_ST_Lr, R_LL, r1, off1+i, 0);
        }
      }
      else syserr("c4p_movc %ld", (long)m);
    }
}

static void memi_op(int32 sop, int32 op,
                    RealRegister r1r, RealRegister r2r, int32 m, int32 flgs)
{
    if (xinDRAM) cc_err("-zpm1 no longer supported");
    if (adconpend && r2r==adr1 && flgs&J_DEAD_R2 &&
                    !((flgs & 7)==0 && r2r==r1r)) {
      Symstr *name = (Symstr *)adm;
      int32 offset = (int32)adr2 + m;
/*        printf("memi_op sop/op=%lx/%lx adconpend hit r1r=%ld r2r=%ld m=%ld flgs=%lx\n", */
/*               sop, op, r1r, r2r, m, flgs); */
      int pz = ispagezero(name);
      if (pz==1) {
        ensure_reg_rw(r1r, opisLOAD(op));
        outRext(opisLDH_STH(op) ? OUT_ZPSYMh :
                opisLDW_STW(op) ? OUT_ZPSYMw : OUT_ZPSYMb,
                sop | RD(r1r), name, offset);
        if (opisLOAD(op)) set_unready(r1r, pz);
        /* else clear_unready(); */
      }
      else {
        /* OP_MOVI_S won't work for pagezero address (s8 instead of u10). */
        outRext(OUT_EXTSYM, (OP_MOVI_L | RD(R_LL)), name, offset);
        ensure_reg_rw(r1r, opisLOAD(op));
        if (op==OP_LD_Lr && !xinLDDok && isrom(name)) { /* Nasty 32bit case */
          outrelref_LDD_synth(r1r, R_LL, 0, pz);
        }
        else if (op==OP_ST_Lr && !xinSTDok) {     /* Nasty 32bit case */
          outi1(OP_ST_Wr | RD(r1r) | RS(R_LL));
          /* clear_unready(); */
          imm_shift(OP_SHRI, r1r, r1r, 16);
          outi1(OP_ST_Wr | RD(r1r) | RS(R_LL) | 2);
          if (!(flgs&J_DEAD_R1)) {
            /* restore r1r by re-loading it */
            outi1(OP_LD_Lr | RD(r1r) | RS(R_LL));
            set_unready(r1r, pz);
          }
        }
        else {
          outi1(op | RD(r1r) | RS(R_LL));
          if (opisLOAD(op)) set_unready(r1r, pz);
          /* else clear_unready(); */
        }
      }
      adconpend = 0;
    }
    else {
      flush_adcon();
      outrelref(op, r1r, r2r, m, !(flgs&J_DEAD_R1));
    }
    correct_signedness(r1r, flgs);
}

static void memr_op(int32 op, RealRegister r1r, RealRegister r2r,
                    RealRegister mr, int32 flgs)
{
      RealRegister rt;
      flush_adcon();
      /* re-use r2r or mr if DEAD... */
      if (flgs&J_DEAD_R2 && !(r2r==mr || ((flgs & 7)==0 && r2r==r1r)))
        rt = r2r;
      else if (flgs&J_DEAD_R3 && !(r2r==mr || ((flgs & 7)==0 && mr==r1r)))
        rt = mr;
      else
        rt = R_LL;
      rr_op(OP_ADDr, rt, r2r, mr);
      outrelref(op, r1r, rt, 0, !(flgs&J_DEAD_R1));
      correct_signedness(r1r, flgs);
}

static void show_inst_direct(J_OPCODE op, int32 r1, int32 r2, int32 m,
                             int32 xtra)
     /* The types of the arguments here are rather unsatisfactory - in        */
     /* particular the last one (m) is really a big union.                    */
{
    RealRegister r1r = -1, r2r = -1, mr = -1;
    int32 opm;
    int32 dead;
  /* Is this the way to do it?  Concern over code quality - the compiler
   * clearly needs to know how to put union values of size 4 in registers.
   *  union { Symstr *sym; int32 umint; } um;
   *  um.umint = m;
   */
    if (uses_r1(op)) {
      if (r1r >= NMAGICREGS) syserr(syserr_r1r, (long)r1r);
      r1r = r1&7;
    }
    if (uses_r2(op)) {
      if (r2r >= NMAGICREGS) syserr(syserr_r2r, (long)r2r);
      r2r = r2&7;
    }
    if (uses_r3(op)) {
      if (mr >= NMAGICREGS) syserr(syserr_mr, (long)mr);
      mr = m&7;
    }
    if (debugging(DEBUG_LOCALCG)) {
      int r1dead = (op&J_DEAD_R1?'#':' ');
      int r2dead = (op&J_DEAD_R2?'#':' ');
      int r3dead = (op&J_DEAD_R3?'#':' ');
      cc_msg("GEN: ");
      jopprint_opname(op);
      cc_msg("%ld%c %ld%c %ld%c\n", (long)r1, r1dead,
             (long)r2, r2dead,
             (long)m,  r3dead);
    }
  /* to enable future JOPCODE peephole optimisation expand_jop_macros()
     tries quite hard not to call show_inst() with instructions with
     no effect.
     */

    dead = op&J_DEADBITS;         /* for peepholing */
    op &= ~J_DEADBITS;
    opm = op&~(Q_MASK|J_ALIGNMENT|J_WBIT);

    if (cmp_pending) {
      if (!(opm == J_B)) cmp_pending = 0;
    }
    else if ((opm == J_B) && (op & Q_MASK) != Q_AL) {
      syserr("gen.c(branch %.8lx without compare)", (long)op);
    }

/* Stack optimisation code */
    if (((xtra & Q_MASK) != Q_AL) >
        ((op & J_TABLE_BITS) == J_ANDK  ||
         (op & J_TABLE_BITS) == J_EORK  ||
         (op & J_TABLE_BITS) == J_ORRK  ||
         (op & J_TABLE_BITS) == J_ANDR  ||
         (op & J_TABLE_BITS) == J_EORR  ||
         (op & J_TABLE_BITS) == J_ORRR))
      syserr("xtra not TSTIBK");
    if ((xtra & Q_MASK) != Q_AL)
      correct_stack(FORCE);         /* ensure cmp_undefer safe w.r.t J_B */
    if (r1r==R_SP || mr==R_SP) syserr("gen(SP)");
    if (r2r==R_SP) {
      correct_stack(VIRTUAL);
      switch (op & J_TABLE_BITS) {
      default:      syserr("gen(SP2)");
      case J_LDRK:  case J_STRK:
      case J_LDRWK: case J_STRWK:
      case J_LDRBK: case J_STRBK:
      case J_ADDK:  break;
      }
      m += stack_move;
    }

    if (adconpend &&
        (opm != J_LDRK) && (opm != J_STRK) &&
        ((opm & ~(J_SIGNED|J_UNSIGNED)) != J_LDRWK) && (opm != J_STRWK) &&
        ((opm & ~(J_SIGNED|J_UNSIGNED)) != J_LDRBK) && (opm != J_STRBK) &&
        (opm != J_LDRR) && (opm != J_STRR) &&
        ((opm & ~(J_SIGNED|J_UNSIGNED)) != J_LDRWR) && (opm != J_STRWR) &&
        ((opm & ~(J_SIGNED|J_UNSIGNED)) != J_LDRBR) && (opm != J_STRBR) &&
        (opm != J_ADDK) && (opm != J_SUBK))
      flush_adcon();

    if (adconvpend && !(opm == J_MOVC || opm == J_CLRC))
      flush_adconv();

    switch(opm) {
    case J_CLRC:
      if (adconvpend && r1r==advr1) {
        c4p_clrc(R_SP, advm, m);
        if (dead & J_DEAD_R1) adconvpend = 0;
      }
      else {
        flush_adconv();
        c4p_clrc(r1r, 0, m);
      }
      return;
    case J_MOVC:
      if (adconvpend && r1r==advr1)
        {
          c4p_movc(R_SP, advm, r2r, m);
          if (dead & J_DEAD_R1) adconvpend = 0;
        }
      else
        {
          flush_adconv();
          c4p_movc(r1r, 0, r2r, m);
        }
      return;
    case J_OPSYSK:
      outi1(OP_INT | (m & 7) << 8);
      return;
    case J_WORD:
      /* if ((m & (48<<5)) == (48<<5)) outi2(m,0); else */
      outi1(m);
      return;
    case J_CALLK:
      correct_stack(FORCE);
      if (stack_move) syserr("CALLK stack mis-aligned\n");
      outcall(OP_CALL_L | (CC_AL<<8), (Symstr *)m);
      scc_regs = 0;
      return;
    case J_TAILCALLK:
      routine_exit(0);   /* effectively includes correct_stack(FORCE) */
      if (stack_move) syserr("TAILCALLK stack mis-aligned\n");
      outcall(OP_JUMP_L | (CC_AL<<8), (Symstr *)m);
      scc_regs = 0;
      return;
    case J_CALLR:
      correct_stack(FORCE);
      if (stack_move) syserr("CALLR stack mis-aligned\n");
      move_register(R_LR, mr);
      outcall(OP_CALL_L | (CC_AL<<8), c4p_libfn(4,0));
      return;
    case J_TAILCALLR:
      move_register(R_LR, mr); lock_r0 = TRUE;
      routine_exit(0);          /* effectively includes correct_stack(FORCE) */
                                /* R_LR preserved by routine_exit      */
      ensure_waitm();
      ensure_reg(R_LR); outi1(OP_MOV2PC); lock_r0 = FALSE;
      break;

    case J_INFOSCOPE: break;
    case J_INFOBODY: break;
    case J_INFOLINE: break;

#ifndef TARGET_HAS_HARVARD_SEGS
    case J_COUNT:
      correct_stack(FORCE);
/* (int)r1 is ? (I would like the character on the line) ????              */
/* (char *)r2 is the name of the file, and (int)m is the line number       */
/* within that file. I will assume here a sensible limit on the length     */
/* of files and hence pack these two pieces of information into a single   */
/* 32-bit word. The structure used is count_position, and up to 16 files   */
/* can be referenced. If there is any danger of running out of same I will */
/* flush out the table used to decode files names and start again.         */
       {
         count_position k;
         mcpeepdata *mc_peep = new_mcpeep();
         /* beware that the next line may flush literals etc. */
         k.s.file = lit_of_count_name((char *)r2);
         k.s.line = (unsigned int)m;
         k.s.posn = 0;       /* Not available here */
         outcall(OP_CALL_L | (CC_AL<<8), countroutine);
         mc_peep->type = OUT_COUNT;
         mc_peep->w = k.i;
         emit_one_peeped(mc_peep);
         /* ensure count data follows call, and next block is referenced */
       }
       scc_regs = 0;
       return;
#endif

    case J_ADCON:
      adr1 = r1r; adr2 = r2; adm = m; adconpend = 1;
      return;
    case J_B:
      if (!((LabelNumber *)m == RETLAB && (op & Q_MASK) == Q_AL))
        correct_stack(FORCE);
      conditional_branch_to(cmp_undefer(op&Q_MASK), (LabelNumber *)m, BR_normal);
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
        cmp_defer(1, r1r, -1, casebranch_pending-1, Q_HS);
        correct_stack(FORCE);
        conditional_branch_to(cmp_undefer(Q_HS), (LabelNumber *)m, BR_normal);
        cmp_pending = 0;
        imm_shift(OP_SHLI, r1r, r1r, 2);
        outi1(OP_MOVPC); lock_r0 = TRUE;
        imm_op(OP_ADDr, R_LR, R_LR, 6); /* 6 is length of sequence here */
        rr_op(OP_ADDr, R_LR, R_LR, r1r);
        outi1(OP_MOV2PC); lock_r0 = FALSE;
/*          printf("setlab1 called at line %d\n", __LINE__); */
        setlab1(tablelab);
        casebranch_pending = 0;
      }
      else {                  /* not first BXX */
        LabelNumber *l = (LabelNumber *)m;
        conditional_branch_to(Q_AL, (l==RETLAB) ? returnlab : l, BR_bxx);
      }
        return;
    case J_LABEL:
      {
        LabelNumber *l = (LabelNumber *)m;
        if (l->block == DUFF_ADDR)    /*  && l != returnlab) */
          syserr("Unused label L%ld\n", lab_name_(l)&0x7fffffff);
        else {
          correct_stack(FORCE);
/*            printf("setlab1 called at line %d\n", __LINE__); */
          setlab1(l);
          scc_regs = 0;
        }
      }
      cmp_pending = 0;
      return;
    case J_STACK:
      fp_minus_sp = m;
      stack_move = 0;
      return;
    case J_SETSP:
      {
        int32 diff, oldstack = (int32)r2, newstack = m;
        if (fp_minus_sp != oldstack)
          syserr("SETSP confused %ld!=%ld %ld",
                 (long)fp_minus_sp, (long)oldstack, (long)newstack);
        diff = newstack - oldstack;
        fp_minus_sp = newstack;
        stack_move -= diff;
      }
      return;

    case J_CMPR:
      cmp_defer(2, r2r, mr, 0, op & Q_MASK);
      return;
    case J_CMPK:
      cmp_defer(1, r2r, -1, m, op & Q_MASK);
      return;
    case J_MOVR:
      if (r1r==mr) syserr(syserr_movr);
      move_register(r1r, mr);
      return;
    case J_NEGR:
      if (r1r == mr) {
        move_register(R_LL, mr);
        load_integer(r1r, 0);
        rr_op(OP_SUBr, r1r, r1r, R_LL);
      }
      else {
        load_integer(r1r, 0);
        rr_op(OP_SUBr, r1r, r1r, mr);
      }
      return;
    case J_NOTR:
      imm_op(OP_XORr, r1r, mr, -1);
      return;
    case J_MOVK:
      load_integer(r1r, m);
      return;

    case J_SHRK: /*+J_UNSIGNED:*/
      imm_shiftur(OP_SHRI, r1r, r2r, m);
      return;
    case J_SHRK+J_SIGNED:
      imm_shift(OP_SHRI, r1r, r2r, m);
      return;
    case J_SHLK+J_SIGNED:
    case J_SHLK: /*+J_UNSIGNED:*/
      imm_shift(OP_SHLI, r1r, r2r, m);
      return;
    case J_EXTEND:
      imm_extend(r1r, r2r, m);
      return;

    case J_SUBK: m = -m;           /* beware overflow and SAT arith */
    case J_ADDK:
      if (adconpend && r2r==adr1 && dead&J_DEAD_R2) {
        adr2 += m; adr1 = r1r;
        return;
      }
      flush_adcon();
      if (r2r == R_SP) {
        advr1 = r1r; advm = m; adconvpend = 1;
        return;
      }
      imm_op(OP_ADDr, r1r, r2r, m);
      return;

/* the front-end maps MULK to MULR if TARGET_LACKS_MULTIPLY_LITERALS,
   and DIVK/REMK to DIVR/REMR if TARGET_LACKS_DIVIDE_LITERALS.
   TARGET_LACKS_MULDIV_LITERALS is equivalent to setting both of these.

   MULR/DIVR/REMR get mapped to function calls unless TARGET_HAS_MULTIPLY,
   TARGET_HAS_DIVIDE etc.
*/

    case J_MULK:
        /* we need a 'care only about bottom 16 bits' (or 'know arg is  */
        /* in range') J_opcode.   Currently use MULK+SIGNED+UNSIGNED... */
    case J_MULK+J_SIGNED:
      imm_mult(r1r, r2r, m,
               ((op&(J_SIGNED|J_UNSIGNED))==(J_SIGNED|J_UNSIGNED) ?
                MV_j16:MV_s32)); return;
/*      case J_MULR: */
/*        rr_mult(r1r, r2r, mr); return; */
/*      case J_DIVK+J_SIGNED: */
/*      case J_DIVK+J_UNSIGNED: */
/*        imm_div(D_div, r1r, r2r, m, op); return; */
/*      case J_DIVR+J_SIGNED: */
/*      case J_DIVR+J_UNSIGNED: */
/*        rr_div(D_div, r1r, r2r, mr, op); return; */
/*      case J_REMK+J_SIGNED: */
/*      case J_REMK*+J_UNSIGNED: */
/*        imm_div(D_rem, r1r, r2r, m, op); return; */
/*      case J_REMR+J_SIGNED: */
/*      case J_REMR+J_UNSIGNED: */
/*        rr_div(D_rem, r1r, r2r, mr, op); return; */

    case J_ANDK:
      imm_op(OP_ANDr, r1r, r2r, m);
      if ((xtra & Q_MASK) != Q_AL) cmp_defer(3, r1r, -1, 0, xtra & Q_MASK);
      return;
    case J_ORRK:
      imm_op(OP_ORr, r1r, r2r, m);
      if ((xtra & Q_MASK) != Q_AL) cmp_defer(3, r1r, -1, 0, xtra & Q_MASK);
      return;
    case J_EORK:
      imm_op(OP_XORr, r1r, r2r, m);
        if ((xtra & Q_MASK) != Q_AL) cmp_defer(3, r1r, -1, 0, xtra & Q_MASK);
        return;

case J_ADDR:
        rr_op(OP_ADDr, r1r, r2r, mr);
        return;
case J_SUBR:
        rr_op(OP_SUBr, r1r, r2r, mr);
        return;
case J_ANDR:
        rr_op(OP_ANDr, r1r, r2r, mr);
        if ((xtra & Q_MASK) != Q_AL) cmp_defer(3, r1r, -1, 0, xtra & Q_MASK);
        return;
case J_ORRR:
        rr_op(OP_ORr, r1r, r2r, mr);
        if ((xtra & Q_MASK) != Q_AL) cmp_defer(3, r1r, -1, 0, xtra & Q_MASK);
        return;
case J_EORR:
        rr_op(OP_XORr, r1r, r2r, mr);
        if ((xtra & Q_MASK) != Q_AL) cmp_defer(3, r1r, -1, 0, xtra & Q_MASK);
        return;
case J_SHLR+J_SIGNED:
case J_SHLR: /*+J_UNSIGNED:*/
        /* @@@ ugly, long, code -- simulate C call */
        rr_shift(c4p_libfn(3,0), r1r, r2r, mr);
        return;
case J_SHRR+J_SIGNED:
        rr_shift(c4p_libfn(1,0), r1r, r2r, mr);
        return;
case J_SHRR: /*+J_UNSIGNED:*/
        rr_shift(c4p_libfn(2,0), r1r, r2r, mr);
        return;

/* load/store                                                              */

case J_LDRBK: /*+J_UNSIGNED:*/
case J_LDRBK+J_SIGNED:
        memi_op(OP_LD_Bs, OP_LD_Br, r1r, r2r, m, dead|(op&J_UNSIGNED)|1);
        break;
case J_LDRWK: /*+J_UNSIGNED:*/
case J_LDRWK+J_SIGNED:
        memi_op(OP_LD_Ws, OP_LD_Wr, r1r, r2r, m, dead|(op&J_UNSIGNED)|2);
        break;
case J_LDRK:
        memi_op(OP_LD_Ls, OP_LD_Lr, r1r, r2r, m, dead|4);
        break;
case J_STRBK:
        memi_op(OP_ST_Bs, OP_ST_Br, r1r, r2r, m, dead); break;
case J_STRWK:
        memi_op(OP_ST_Ws, OP_ST_Wr, r1r, r2r, m, dead); break;
case J_STRK:
        memi_op(OP_ST_Ls, OP_ST_Lr, r1r, r2r, m, dead); break;

case J_LDRBR: /*+J_UNSIGNED:*/
case J_LDRBR+J_SIGNED:
        memr_op(OP_LD_Br, r1r, r2r, mr, dead|(op&J_UNSIGNED)|1);
        break;
case J_LDRWR: /*+J_UNSIGNED:*/
case J_LDRWR+J_SIGNED:
        memr_op(OP_LD_Wr, r1r, r2r, mr, dead|(op&J_UNSIGNED)|2);
        break;
case J_LDRR:
        memr_op(OP_LD_Lr, r1r, r2r, mr, dead|4);
        break;

    case J_STRBR:
        memr_op(OP_ST_Br, r1r, r2r, mr, dead); break;
    case J_STRWR:
        memr_op(OP_ST_Wr, r1r, r2r, mr, dead); break;
    case J_STRR:
        memr_op(OP_ST_Lr, r1r, r2r, mr, dead); break;

    case J_ENDPROC:
      if (returnlab->lndraft == 0x7ffffffe) {
           /* i.e. !setlab1(returnlab) && reflab(returnlab) */
/*              printf("setlab1 called at line %d\n", __LINE__); */
            fp_minus_sp = 0; stack_move = 0; clear_unready();
            setlab1(returnlab);
/*      if (stack_move != 0) syserr("ENDPROC stack_move %d", stack_move); */
            conditional_branch_to(Q_AL, RETLAB, BR_endproc);
      }
      bb_list = (struct mcpeepdata *)dreverse((List *)bb_list);
      emit_all_peeped();
      if (codep & 2) outcode16(OP_NOP);
#ifndef TARGET_HAS_HARVARD_SEGS
      dumplits2(0);
      dump_count_names();
#endif
      asm_lablist = (LabList *)dreverse((List *)asm_lablist);
      return;
#ifdef CPLUSPLUS
    case J_ORG:
      while (codep < m) outi1(OP_NOP);
      if (codep != m) syserr("J_ORG");
      break;
#endif
    case J_ENTER:
      lock_r0 = FALSE;
      asm_lablist = 0;
      pcdraft = 0; bb_list = 0;
      xin_stkerrmsg = 0;
      clear_unready();
      routine_entry(m);
      return;

    case J_PUSHM:
      {
        int32 i;
        int32 n = 4*bitcount(m);
        stack_move -= n;
        correct_stack(VIRTUAL);
        for (i = (NINTREGS-1); i >= 0; i--) {
          if (m & ((int32)1<<i)) {
            n -= 4;
/*              printf("pushm: to %ld\n", stack_move + n); */
            outrelref(OP_ST_Lr, i, R_SP, (stack_move + n), 1);
          }
        }
      }
      fp_minus_sp += 4*bitcount(m);
      return;

#ifdef never  /* code literal */
/* The floating point instructions are done by SOFTWARE_FLOATING_POINT. */
    case J_ADCONF:
      fpaddr(0, r1r, (FloatCon *)m);
      break;
    case J_ADCOND:
      fpaddr(1, r1r, (FloatCon *)m);
      break;
#endif

    default:
      syserr(syserr_show_inst, (long)op);
      outi2(-1,0);     /* OP_HALT placeholder */
    }
}

static struct Pending
{
    /* like struct Icode but with RealReg's and a peep field */
    J_OPCODE op;
    int32 xtra;
    RealRegister r1,r2;
/* Maybe an r3 field would facilitate peephole treatment of the J_SHIFTM's  */
/* via a R_INTERNAL idea.                                                   */
    int32 m;
} pending[10];
#define pendingtmp pending[9]

static int pending_cnt;

#define current(i) pending[pending_cnt-((i)+1)]

/* Exported routines...                                               */

/* The peepholer: */
void show_instruction(J_OPCODE op,VRegInt vr1,VRegInt vr2,VRegInt vm)
{
    /* it may be better to arrange for two switches - one on 'op' and    */
    /* one on pending.op                                                 */
    RealRegister r1 = vr1.r, r2 = vr2.r;
    int32 m = vm.i;
    int32 xtra = Q_AL;                /* essentially extra bits for 'op' */
    int i;
    op &= ~J_ALIGNMENT;
/* to enable future JOPCODE peephole optimisation expand_jop_macros()
   tries quite hard not to call show_inst() with instructions with
   no effect.
*/

    if (debugging(DEBUG_LOCALCG)) {
      int r1dead = (op&J_DEAD_R1?'#':' ');
      int r2dead = (op&J_DEAD_R2?'#':' ');
      int r3dead = (op&J_DEAD_R3?'#':' ');
      cc_msg("xxxxxxxxxxxxxxxxxGEN: ");
      jopprint_opname(op);
      cc_msg("%ld%c %ld%c %ld%c\n", (long)r1, r1dead,
             (long)r2, r2dead,
             (long)m,  r3dead);
    }

    if (pending_cnt == 3) {
      show_inst_direct(pending[0].op, pending[0].r1, pending[0].r2,
                       pending[0].m, pending[0].xtra);
      pending[0] = pending[1];
      pending[1] = pending[2];
      pending_cnt--;
    }
    pending[pending_cnt].op = op;
    pending[pending_cnt].r1 = r1;
    pending[pending_cnt].r2 = r2;
    pending[pending_cnt].m = m;
    pending[pending_cnt].xtra = xtra;
    pending_cnt++;
    if (pending_cnt >= 2 &&
        (current(0).op & J_TABLE_BITS) == J_CMPK &&
        ((current(0).op & Q_MASK-Q_UBIT) == Q_NE ||
         (current(0).op & Q_MASK-Q_UBIT) == Q_EQ) &&
        current(0).m == 0 &&
        current(0).r2 == current(1).r1)
      switch (current(1).op & J_TABLE_BITS) {
      case J_ANDK: case J_EORK: case J_ORRK:
        /* the next line is a temp hack (to ensure CC gets set) ... */
        if (!(1 <= current(1).m && current(1).m <= 0x7fff))
          break;
      case J_ANDR: case J_EORR: case J_ORRR:
        current(1).xtra = current(0).op & Q_MASK;
        pending_cnt--;
        return;
      }
      /* re-invent LDRR etc (removed by cg.c for CSE, but optimisation    */
      /* in show_instruction_direct() for ADCON; LDRR...)                 */
    if (pending_cnt >= 2 && (current(1).op & J_TABLE_BITS) == J_ADDR)
      switch (current(0).op & J_TABLE_BITS)
        {
        case J_STRBK: case J_STRWK: case J_STRK:
          if (current(0).r1 == current(0).r2) break;
        case J_LDRBK: case J_LDRWK: case J_LDRK:
          if (current(0).m == 0 &&
              current(0).r2 == current(1).r1 &&
              current(0).op & J_DEAD_R2) {
            current(1).r1 = current(0).r1;   /* leave r2,r3 as in ADDR */
            current(1).op = J_KTOR(current(0).op) & ~(J_DEAD_R2|J_DEAD_R3)
              | current(1).op & (J_DEAD_R2|J_DEAD_R3);
            pending_cnt--;
            return;
          }
        }
/* Turn: (*(signed short *)x & 0xffff) to (*(unsigned short *)x)        */
    if (pending_cnt >= 2 &&
        (current(0).op & J_TABLE_BITS) == J_ANDK &&
        current(0).xtra == Q_AL) switch (current(1).op & J_TABLE_BITS) {
        case J_LDRBK: case J_LDRBR:
          if (current(0).m == 0xff) goto tryrmand;
          break;
        case J_LDRWK: case J_LDRWR:
          if (current(0).m == 0xffff) goto tryrmand;
          break;
        tryrmand:
          if (current(0).op & J_DEAD_R2 &&
              current(0).r2 == current(1).r1) {
            current(1).r1 = current(0).r1;
            current(1).op = current(1).op & ~J_SIGNED | J_UNSIGNED;
            pending_cnt--;
            return;
          }
        }
    /* Turn: ((short)*(unsigned short *)x) to (*(signed short *)x)          */
    if (pending_cnt >= 2 &&
        (current(0).op & J_TABLE_BITS) == J_EXTEND &&
        current(0).xtra == Q_AL) switch (current(1).op & J_TABLE_BITS) {
        case J_LDRBK: case J_LDRBR:
          if (current(0).m == 1) goto tryrmext;
          break;
        case J_LDRWK: case J_LDRWR:
          if (current(0).m == 2) goto tryrmext;
          break;
        tryrmext:
          if (current(0).op & J_DEAD_R2 &&
              current(0).r2 == current(1).r1) {
            current(1).r1 = current(0).r1;
            current(1).op = current(1).op & ~J_UNSIGNED | J_SIGNED;
            pending_cnt--;
            return;
          }
        }
    switch (current(0).op & J_TABLE_BITS) {
      case J_ENTER: case J_ENDPROC: case J_STACK: case J_LABEL:
      case J_SETSP: case J_PUSHM: case J_PUSHR: case J_PUSHW:
      case J_INFOBODY: case J_INFOSCOPE:
      case J_INFOLINE: case J_INFOCOM:
        /* flush these cases immediately for local_address()  */
        for (i = 0; i<pending_cnt; i++)
        show_inst_direct(pending[i].op, pending[i].r1, pending[i].r2,
                         pending[i].m, pending[i].xtra);
        pending_cnt = 0;
        break;
      }

}

void mcdep_init(void)
{
  pending_cnt = 0;
  cmp_pending = 0;
  avoidallocating(R_LL);
  avoidallocating(R_IP);
}

void localcg_tidy(void) {}
void localcg_reinit(void) {}
bool alterscc(Icode *ic) { return 1; }


/* vvvvvvvvvvvvvvvvvvvvvv    PEEPHOLER    vvvvvvvvvvvvvvvvvvvv */

static mcpeepdata *new_mcpeep(void)
{
  mcpeepdata *p = (mcpeepdata *)SynAlloc(sizeof(mcpeepdata));
  memclr((void *)p, sizeof(mcpeepdata));
  return p;
}

static void emit_one_peeped(mcpeepdata *peep)
{
  /* first chain together */
  peep->next = bb_list;
  bb_list = peep;
  /* now (over)estimate size and update pcdraft */
  /* the lsb of pcdraft means 'uncertain size; e.g. after cond branch.  */
  switch (peep->type) {
  case OUT_LABDEF:
    pcdraft = pcdraft & ~1;             /* now know 16-bit aligned.     */
    peep->lab->lndraft = pcdraft;
    return;
  case OUT_LABREF:
    if (peep->lab->lndraft <= pcdraft && pcdraft-256 <= peep->lab->lndraft)
        pcdraft += 2;       /* backward jump, known not to be too far */
    else
    {   pcdraft += 4;       /* 4 for jump.l/call.l   */
        pcdraft |= 1;       /* but also mark size as uncertain...       */
    }
    break;
  case OUT_IN1:
    if (peep->w == OP_WAITM) clear_unready();
    /* drop through */
  case OUT_ZPSYMb: case OUT_ZPSYMh: case OUT_ZPSYMw:
    pcdraft += 2;
    break;
  case OUT_IN2:
    pcdraft += 4;
    break;
  case OUT_IN3:
    pcdraft += 6;
    break;
  case OUT_EXTCALL:
  case OUT_EXTSYM:
    pcdraft += 4;
    break;
  default:
    syserr("emit_one_peeped unknown");
  }
  clear_pzunready();
}

static void emit_two_peeped(mcpeepdata *peep)
{ switch (peep->type) {
  case OUT_LABDEF:
/*      outalign(peep->next); */
    setlab2(peep->lab);
    break;
  case OUT_LABREF:
    if (lab_isset_(peep->lab)) {
      int32 w1 = ((peep->lab->u.defn & 0x00ffffff) - codep - 2) >> 1;
                 /* half-words! */
/*        printf("Patching LABREF: w1=%lx w=%lx\n", w1, peep->w); */
      if (w1 >= -128 && w1 < 128 && peep->reftype == LABREF_DISP9) {
          peep->w |= (w1 & 0xff);
/*            printf("  (DISP9) -> w=%lx\n", peep->w); */
      }
      else if (peep->reftype == LABREF_DISP24/* && peep->w == OP_CALL_L*/) {
        w1 = ((peep->lab->u.defn & 0x00ffffff) - codep - 4)/2;
        peep->w |= (w1>>16)&0xff;
        peep->w |= (w1&0xffff)<<16;
/*          printf("  (DISP24) -> w=%lx\n", peep->w); */
      }
      else
        syserr(syserr_back_coderef, (long)-1);
    }
    else {   /* forward ref */
      addfref_(peep->lab, codep | peep->reftype);
    }
    if (peep->reftype == LABREF_DISP9) goto in1case; else goto in2case;
  case OUT_IN1: in1case:
    outcode16(peep->w &0xffff);
    break;
  case OUT_IN2: in2case:
    outcode16(peep->w & 0xffff);
    outcode16(peep->w >> 16 & 0xffff);
    break;
  case OUT_IN3:
    outcode16(peep->w & 0xffff);
    outcode16(peep->off >> 0 & 0xffff);
    outcode16(peep->off >> 16 & 0xffff);
    break;
  case OUT_EXTCALL:
    /* put the reloc on the first byte of the call instruction.         */
    codexrefs = (CodeXref *)global_list4(SU_Xref, codexrefs,
                             X_reloctype[peep->type] | (codebase+codep),
                             peep->s, 0);
    outcode16(peep->w);
    outcode16(0);
    break;
  case OUT_EXTSYM:
    codexrefs = (CodeXref *)global_list4(SU_Xref, codexrefs,
                             X_reloctype[peep->type] | (codebase+codep),
                             peep->s, peep->off);
    outcode16(peep->w & 0xffff);
    outcode16(peep->w >> 16 & 0xffff);
    break;
  case OUT_ZPSYMb: case OUT_ZPSYMh: case OUT_ZPSYMw:
    codexrefs = (CodeXref *)global_list4(SU_Xref, codexrefs,
                             X_reloctype[peep->type] | (codebase+codep),
                             peep->s, peep->off);
    outcode16(peep->w & 0xffff);
    break;
  default:
    syserr("emit_two_peeped unknown");
  }
}

static int nastybranch(mcpeepdata *peep)
{
    if (lab_isset_(peep->lab)) {
      int32 w1 = ((peep->lab->u.defn & 0x00ffffff) - codep - 2) >> 1;
             /* half-words! */
/*        printf("nastybranch: codep=%lx L%ld set at %lx w1=%ld\n", */
/*               codep, lab_name_(peep->lab), peep->lab->u.defn & 0x00ffffff, w1); */
      if (w1 >= -128) return 0;
    }
    else {   /* forward ref */
/*        printf("nastybranch: codep=%lx L%ld unset draft=%lx\n", */
/*               codep, lab_name_(peep->lab), peep->lab->lndraft); */
      if (codep+254 >= peep->lab->lndraft) return 0;
    }
    return 1;
}

static void emit_all_peeped()
{   mcpeepdata *peep;
    for (peep = bb_list; peep != NULL; peep = peep->next)
    {   if (peep->type == OUT_LABREF) switch (nastybranch(peep))
        {
        case 1:     /* C4P JUMP_S/CALL_S out of range */
#ifdef AM_VERSION_SOMETHING_WRONG
          if ((peep->w & OP_JUMP_L) == 0)       /* i.e. short */
              peep->w ^= OP_JUMP_L^OP_JUMP_S;
#else
          if ((peep->w & OP_JUMP_L) == 0)       /* i.e. short */
              peep->w = (peep->w + (2<<11)) & 0xffff;
#endif
          peep->reftype = LABREF_DISP24;
          break;
        case 0:
          break;
        default:
          syserr("Unexpected nastybranch value");
        }
        emit_two_peeped(peep);
    }
}

/* End of c4p/gen.c */
