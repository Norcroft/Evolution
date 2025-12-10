/*
 * C compiler file mips/gen.c.
 * Copyright (C) Codemist Ltd, 1990-2001.
 */

/* Oct 2000, AM: add MIPS16 support.                                    */

/* current nasty: LIT_OPCODE flags most opcodes, but...                 */
/* LIT_RELADDR flags hi/lo 16 bit relocs and LIT_JMPADDRx flags jmpaddrs.  */
#define LIT_JMPADDRx LIT_OPCODE /* hack */
#define X_JmpAddr X_PCreloc     /* hack until rationalised              */

/* This file contains mainly MIPS R2000 dependent machine routines.     */

/* When doing a forward branch we could put a load in the delay slot,   */
/*   putting a noop at the dest. if needed.                             */
/* RE-munge so that LW r,x; lab:; NOOP; USE r; swaps to NOOP; lab:;     */
/* Also, worry about Xsetlabel(returnlab) in conditional_branch_to &c,  */
/*  which seem to be able to set returnlab at the wrong SP level.       */

/* exports:
   void show_instruction(J_OPCODE op,VRegInt vr1,VRegInt vr2,VRegInt vm);
   RealRegister local_base(Binder *b);
   int32 local_address(Binder *b);
   bool immed_cmp(int32);
   bool fpliteral(FloatCon *val, J_OPCODE op);
      also (unfortunately for peephole):
   void setlabel(LabelNumber *);
   void branch_round_literals(LabelNumber *);
*/

#ifdef __STDC__
#  include <string.h>
#else
#  include <strings.h>
#endif

#include "globals.h"
#include "mcdep.h"
#include "mcdpriv.h"
#include "util.h"
#include "aeops.h"
#include "xrefs.h"
#include "ops.h"
#include "jopcode.h"
#include "store.h"
#include "codebuf.h"
#include "regalloc.h"
#include "builtin.h"   /* for te_xxx */
#include "bind.h"      /* for sym_insert_id */
#include "cg.h"        /* for procflags, greatest_stackdepth */
                       /* @@@ move these from cg.h to flowgraf.h */
#include "errors.h"

#ifdef TARGET_IS_ULTRIX
#define MSR(r) (r)
#define LSR(r) ((r)+16)
#else
#define MSR(r) ((r)+16)
#define LSR(r) (r)
#endif

/* The next line is currently only used for a hack.                     */
#define s24_(x)   ((((x) & 0xffffff) ^ 0x800000) - 0x800000)

#define OP_MIPS16 OP_SPECIAL    /* marker for SEB,ZEB etc in outinstrI. */

/* reclassify these as to whether they look at mc_peep...               */
static void outinstr0(int32, int32, int32);
static void outinstr0x(int32, int32, int32);
static void outswent(LabelNumber *dest);
static void outswtab(LabelNumber **tab, int32 n);
static void outltorg(void);
static void outinstr1(int32, int32, int32);
static void outinstr2(int32 w, int32 reads, int32 writes,
                      LabelNumber *lab, int32 t);
static void outinstr3(int32, Symstr *);
static void conditional_branch_to(int32, LabelNumber *);
static void conditional_branch_to_n(int32, LabelNumber *);
static void conditional_scc_to(int32, RealRegister, int32);
static void conditional_scc_to_n(int32, RealRegister, int32);
static void Xsetlabel(LabelNumber *, bool);
static void outinstr4(int32, RealRegister, unsigned32,
                           LabelNumber *, int32);
static void outinstr4a(int32, RealRegister, Symstr *, int32, int32);
static void external_ref(int32, RealRegister, unsigned32, Symstr *, int32);
static void countdata(int32);
static void flush_peepholer(void);
static void init_peepholer(void);

static void load_integer(RealRegister r, unsigned32 n);
static void add_integer(RealRegister r1, RealRegister r2, unsigned32 n);
#define load_integer_over_zeroreg(r, n) \
              if (-0x8000 <= (n) && (n) < 0) add_integer((r), (r), (n)); \
              else load_integer((r), (n))

static void move_register(RealRegister r1, RealRegister r2);
static void xor_integer(RealRegister, RealRegister, unsigned32);
static void show_fp_inst_direct(J_OPCODE, int32, int32, RealRegister, RealRegister, RealRegister);

#define LABREF_OFF16    0x00000000  /* MIPS addressing modes for forw. refs. */
#define LABREF_LIT32    0x02000000  /* or.h/l as part of 32 bit address   */
#define LABREF_M16_S16  0x04000000  /* MIPS16 EXT/B opcode forw. ref.     */
#define LABREF_M16_S16X 0x05000000  /* MIPS16 EXT/LW(PC) opcode forw. ref. */
/* NB. the 0x08000000 bit is used to mark MIPS16 unextended relocs:       */
#define LABREF_short    0x08000000
#define LABREF_M16_S8   0x08000000  /* MIPS16 BEQ   opcode forw. ref.     */
#define LABREF_M16_S11  0x09000000  /* MIPS16 B     opcode forw. ref.     */
#define LABREF_M16_U8   0x0a000000  /* MIPS16 LA/LW opcode forw. ref.     */

#define NONLEAF (PROC_ARGPUSH | PROC_ARGADDR | PROC_BIGSTACK | BLKCALL)
/* STACKCHECK (NONLEAF subset) defines when stack check is REALLY needed */
#define STACKCHECK (PROC_BIGSTACK | BLKCALL)

/*
 * The following are used to pass on register user information to the
 * machine-dependent peepholer.
 */
#define reads_(r1,r2,r3,r4) (((r1)<<24) | ((r2)<<16) | ((r3)<<8) | (r4))
#define writes_(r1,r2,op) (((r1)<<24) | ((r2)<<16) | (op))

#define reg1_(a) (((a) >> 24) & 0xff)
#define reg2_(a) (((a) >> 16) & 0xff)
#define reg3_(a) (((a) >> 8) & 0xff)
#define reg4_(a) ((a) & 0xff)
#define opclass_(a) ((a) & 0xffff)

#define OP_NULL   0
#define OP_LOAD   1     /* loads from memory */
#define OP_STORE  2     /* stores to memory */
#define OP_BR16   4     /* does a branch (16 bit offset) */
#define OP_BR26   8     /* does a branch (26 bit offset) */
#define OP_JUMP   16    /* does a branch (register destination) */
#define OP_2WORD  32    /* load or store touches 2 registers */
#define OP_WORD   64    /* unknown behaviour! */

static struct {    /* like struct Icode but with RealReg's and a peep field */
  J_OPCODE op;
  int32 xtra;
  RealRegister r1,r2;
/* Maybe an r3 field would facilitate peephole treatment of the J_SHIFTM's  */
/* via a R_INTERNAL idea.                                                   */
  int32 m;
} pending;

/* Memo: many of the routines here can use R_IP as a temporary.         */
/* Hence mips/target.h  reserves 1 register specially which NEVER       */
/* gets allocated by regalloc (see below for avoidallocating(R_IP).     */
#define R_TM  (mips_opt & 1 ? R_R7eqMR3 : R_IP)
#define R_T24 (mips_opt & 1 ? R_R16eqMR24 : R_IP)
#define R_R6eqMR2   6L
#define R_R7eqMR3   7L
#define R_R16eqMR24 16L
#define R_SYSCALLno R_R6eqMR2
#define R_SYSCALLres R_R6eqMR2

/* The next line copes with the calling standard whereby the lowest 16 bytes */
/* of stack at a call are placed where one might store arg1-arg4 ($4-$7).    */
/* Dec 2000: Being changed to support SP_OFFSET = 0.                         */
#define SP_OFFSET (mips_opt & 16*1024 ? 0 : 16)
#define SP_COVER  (mips_opt & 16*1024 ? 0 : 0)
#define ABIWASTE  (!(mips_opt & (16*1024|32*1024)))
static int32 fp_minus_sp,sp_adjust;
static int32 litpool_codelimit;

int32 wasted_in_save = 0, wasted_in_restore = 0;

#define SPALGN 4  /* (mips_opt & 4 ? 0 : 4) */
#define ensure_sp() { add_integer(R_SP, R_SP, sp_adjust & ~SPALGN); \
        sp_adjust &= SPALGN;}
#define change_sp(by) (void)(sp_adjust += (by))
#define adjusted_sp() (void)(sp_adjust = 0)
#define align_sp()    (void)(sp_adjust += (fp_minus_sp + sp_adjust) & SPALGN)

static LabelNumber *returnlab;

static int32 cmp_pending = 0;
static int32 cmp_pend_r2, cmp_pend_r3, cmp_pend_m, cmp_pend_q;

typedef union count_position
{
    int32 i;
    struct s
    {   unsigned int posn:12,
                     line:16,
                     file:4;
    } s;
} count_position;

/* Mapping virtual registers to real ones */
static RealRegister v2p(RealRegister r)
{
  RealRegister ans;
  static char virtual_map[65] =         /* NMAGICREGS > 32 beware */
    {0, 1, 4, 5, 6, 7, 2, 3, 8, 9, 10, 11, 12, 13, 14, 15, 24, 25,
     16, 17, 18, 19, 20, 21, 22, 23,  30, 27, 28, 29, 26, 31,
     0, 2, 4, 6, 8, 10, 16, 18, 12, 14, 20, 22, 24, 26, 28, 30,
     1, 3, 5, 7, 9, 11, 17, 19, 13, 15, 21, 23, 25, 27, 29, 31,
     0 /* HILO for outinstrDST() */ };
  ans = virtual_map[r];
  return ans;
}

bool immed_cmp(int32 n) { return (n & 0xffff0000) == 0; }

RealRegister local_base(Binder *b)
{
    b = b;   /* interface requires; so reference */
    return R_SP;
}

static int32 firstargoff, firstsaveoff;
static int32 argregs_pushed = 0;
static int32 argpush_effective = 0;     /* see PROC_ARGPUSH */
static unsigned32 max_arg_number = 0;

int32 local_address(Binder *b)
{
    int32 p = bindaddr_(b);
    int32 p1 = p & ~BINDADDR_MASK;
    VRegInt gap;
    gap.i = 0;
    show_instruction(J_NOOP, gap, gap, gap);  /* NASTY!!! Ensures fp_minus_sp!! */
    switch (p & BINDADDR_MASK)
    {   default: syserr(syserr_local_address, (long)p);
        case BINDADDR_LOC:
            return fp_minus_sp + sp_adjust + SP_OFFSET - p1;
        case BINDADDR_ARG:
            if ((unsigned32)(p & ~BINDADDR_MASK) >= 4*max_arg_number)
                syserr(syserr_bad_arg, p);
            if (!argpush_effective)
            {   /* this should be used for LDR(f/v)1 only, check a bit */
                if ((p & ~BINDADDR_MASK) < NARGREGS*4 || fp_minus_sp != 0 ||
                     argregs_pushed != (((procflags & NONLEAF) && ABIWASTE) ?
                                       16 : 0))
                    syserr(syserr_local_addr);
                if (!((procflags & NONLEAF) && ABIWASTE)) p1 -= 4*NARGREGS;
            }
            return fp_minus_sp + firstargoff + sp_adjust + SP_OFFSET + p1;
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

#ifdef REL193_HACK
int32 local_fpaddress(Binder const *P)   /* not adapted for MIPS yet !! */
       /* exported for debugger */
{   /* p is bindaddr_() of the relevant binder */
    int32 p = bindaddr_(P);
#else
int32 local_fpaddress(int32 p)   /* not adapted for MIPS yet !! */
       /* exported for debugger */
{   /* p is bindaddr_() of the relevant binder */
#endif
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

/* MIPS16 literal handling */

static Symstr *find_PCliteral(Symstr *s, int32 d)
{
     char v[160];
     if (d == 0)
       sprintf(v, "__lit_%.128s", s==0 ? "" : symname_(s));
     else
       sprintf(v, "__lit_%.8x_%.128s", d, s==0 ? "" : symname_(s));
     return sym_insert_id(v);
}

/* MIPS16 stuff */

#define eqop_pri(s,op) ((((s)^(op)) & MASK_PRI) == 0)

static int mr16[32] = { -1,-1, 2, 3, 4, 5, 6, 7,
                        -1,-1,-1,-1,-1,-1,-1,-1,
                         0, 1,-1,-1,-1,-1,-1,-1,
                        -1,-1,-1,-1,-1,-1,-1,-1 };

static int32 extform(int32 w)
{   return (OP16_EXT | (w >> 11) & 0x1f | w & 0x07e0) << 16  |  (w & 0x1f);
}

static int32 extform_ADDIU3(int32 w)
{   return (OP16_EXT | (w >> 11) & 0x0f | w & 0x07f0) << 16  |  (w & 0x0f);
}

static int32 foo5u(int32 w, int32 op16, int shift, int32 rx, int32 rb)
{   int32 k = (w >> shift) & 0x1f;
    int32 ww = 0;
    if ((k << shift) != s16_(w)) { ww = extform(w), k = 0; }
    return ww | (op16 | rb<<8 | rx<<5 | k);
}

static int32 foo16u(int32 w, int32 op16, int32 rx)
{   int32 k = w & 255;
    int32 ww = 0;
    if (k != u16_(w)) { ww = extform(w), k = 0; }
    return ww | (op16 | rx<<8 | k);
}

static int32 foo8s(int32 w, int32 op16, int shift, int32 rx)
{   int32 k = (w >> shift) & 255;
    int32 ww = 0;
    if ((s8_(k) << shift) != s16_(w)) { ww = extform(w), k = 0; }
    return ww | (op16 | rx<<8 | k);
}

static int32 foo8u(int32 w, int32 op16, int shift, int32 rx)
{   int32 k = (w >> shift) & 255;
    int32 ww = 0;
    if ((k << shift) != s16_(w)) { ww = extform(w), k = 0; }
    return ww | (op16 | rx<<8 | k);
}

static int32 foo4s(int32 w, int32 op16, int32 ry, int32 rx)
{   int32 k = w & 15;
    int32 ww = 0;
    if (s4_(k) != s16_(w)) { ww = extform_ADDIU3(w), k = 0; }
    return ww | (op16 | rx<<8 | ry<<5 | k);
}

static void outinstrNOOP()
{
    if (!(mips_opt & 2))
        outinstr0(OP_NOOP, reads_(0,0,0,0), writes_(0,0,OP_NULL));
    else
        outinstr0(OP16_NOP, reads_(0,0,0,0), writes_(0,0,OP_NULL));
}

static void outinstrF(int32 w, int32 reads, int32 writes)
{
    if (!(mips_opt & 2))
        outinstr0(w, reads, writes);
    else
        syserr("gen.c(no MIPS16 instruction: 0x%.8x)", w);
}

static void outinstrI(int32 w, RealRegister rt, RealRegister rs)
{
    int32 ww = 0;
    int32 postmov = 0;
    RealRegister rd = rt;
    if (!(mips_opt & 2))
        ww = w | F_RT(rt) | F_RS(rs);
    else if (eqop_pri(w,OP_ADDIU) && rt == R_SP && rs == R_SP)
        ww = foo8s(w, OP16_ADJSP, 3, 0);
    else if (rt == R_T24)
    {   int32 rx = mr16[v2p(rs)];
        if (rx < 0)
        {   move_register(R_R7eqMR3, rs); rs = R_R7eqMR3; rx = v2p(rs);
        }
        if (eqop_pri(w,OP_XORI))
            ww = foo16u(w, OP16_CMPI, rx);
        else if (eqop_pri(w,OP_SLTI))
            ww = foo8u(w, OP16_SLTI, 0, rx);
        else if (eqop_pri(w,OP_SLTIU))
            ww = foo8u(w, OP16_SLTIU, 0, rx);
        else
        {   syserr("outinstrI(%.8x, %d, %d)", w, rt, rs);
            ww = OP16_BREAK;
        }
    }
    else
    {   int32 rx = mr16[v2p(rs)];
        int32 rz = mr16[v2p(rt)];
        if (rz < 0)
        {   rt = R_R7eqMR3; rz = v2p(rt);
            postmov = OP16_MOV32R | v2p(rd) & 24 | (v2p(rd) & 7) << 5 | rz;
        }
        if (eqop_pri(w,OP_ORI) && rs == 0)
            ww = foo16u(w, OP16_LI, rz);
        else if (eqop_pri(w,OP_ADDIU))
        {   if (rt != rs && s15_(w) == s16_(w) && rx >= 0)
                ww = foo4s(w, OP16_ADDIU3, rz, rx);
            else if (rs == R_SP)
                ww = foo8u(w, OP16_ADDIU_SP, 2, rz);
            else
            {   move_register(rt, rs); rs = rt;
                ww = foo8s(w, OP16_ADDIU2, 0, rz);
            }
        }
        else if (eqop_pri(w,OP_MIPS16))
        {   move_register(rt, rs); rs = rt;
            ww = w | (rz << 8);
        }
        else
        {   syserr("outinstrI(%.8x, %d, %d)", w, rt, rs);
            ww = OP16_BREAK;
        }
    }
    outinstr0(ww, reads_(rs, 0, 0, 0), writes_(rt, 0, OP_NULL));
    if (postmov)
        outinstr0(postmov, reads_(rt, 0, 0, 0), writes_(rd, 0, OP_NULL));
}

static void outinstrDT(int32 w, RealRegister rd, RealRegister rt, int flg)
{
    /* flg values: 0=normal, 1=use RS instead of RT on MIPS1,           */
    /*             2=use OUT_SWSHIFT on MIPS16.                         */
    int32 ww, n;
    int32 premov = 0;
    int32 postmov = 0;
    RealRegister ru = rd, rs = rt, ru2 = 0;
    if (!(mips_opt & 2))
    {   /* use_S has no semantic effect, but preserves MIPS R3000 code. */
        ww = w | F_RD(rd) | (flg&1 ? F_RS(rt) : F_RT(rt));
        /* another hack to preserve MIPS R3000 code:                    */
        if (!(mips_opt & 1)) ru2 = R_HILO;
    }
    else
    {   if ((mips_opt & 2) && (rd == 0 || rs == 0))
            syserr("gen.c(mips16 move_register(0))");
        switch (w & MASK_PRI+MASK_SPECIAL) {
    case OP_MFHI:
          ww = OP16_MFHI; n = 0; goto mfhilo;
    case OP_MFLO:
          ww = OP16_MFLO; n = 0; goto mfhilo;
    mfhilo:
          { int32 rx = mr16[v2p(ru)];
            if (rx < 0)
            {   ru = R_R7eqMR3; rx = v2p(ru);
                postmov = OP16_MOV32R | v2p(rd) & 24 | (v2p(rd) & 7) << 5 | rx;
            }
            ww |= (rx<<8);
          }
          break;
    case OP_OR:
          ww = OP16_SLL; n = 0; goto shifti;
    case OP_NOR:
          ww = OP16_NOT; n = 8; goto shifti;
    case OP_SUBU:
          ww = OP16_NEG; n = 8; goto shifti;
    case OP_SLL:
          ww = OP16_SLL; n = D_SHAMT(w); goto shifti;
    case OP_SRL:
          ww = OP16_SRL; n = D_SHAMT(w); goto shifti;
    case OP_SRA:
          ww = OP16_SRA; n = D_SHAMT(w); goto shifti;
    shifti:
          { int32 rx = mr16[v2p(ru)];
            int32 ry = mr16[v2p(rs)];
            if (rx < 0)
            {   ru = (ry>=0 && n==0) ? rt : R_R7eqMR3; rx = mr16[v2p(ru)];
                postmov = OP16_MOV32R | v2p(rd) & 24 | (v2p(rd) & 7) << 5 | rx;
            }
            if (ry < 0 || (n == 0 && rx != ry))
            {   rs = ru; ry = rx;
                premov = (OP16_MOVR32 | rx<<5 | v2p(rt));
            }
            if (rx == ry && n == 0)
                ww = 0;
            else
            {   if (!(1<=n && n<=8)) { ww |= extform(SHAMT(n)); n = 0; }
                ww |= (rx<<8 | ry<<5 | (n&7)<<2);
            }
          }
          break;
    default:
          syserr("outinstrDT(%d, %d)", rd, rt);
          ww = OP16_BREAK;
        }
    }
    if (premov)
        outinstr0(premov, reads_(rt, 0, 0, 0), writes_(rs, 0, OP_NULL));
    if (ww)
    {   if (flg & 2)
          outinstr0x(ww, reads_(rs, 0, 0, 0), writes_(ru, ru2, OP_NULL));
        else
          outinstr0(ww, reads_(rs, 0, 0, 0), writes_(ru, ru2, OP_NULL));
    }
    if (postmov)
        outinstr0(postmov, reads_(ru, 0, 0, 0), writes_(rd, 0, OP_NULL));
}

static void outinstrDST(int32 w, RealRegister rd,
                        RealRegister rs, RealRegister rt)
{
    int32 postmov = 0;
    int32 ww;
    RealRegister ru = rd;
    if (!(mips_opt & 2))
        ww = w | F_RD(rd) | F_RS(rs) | F_RT(rt);
    else if (rd == R_T24 || rd == R_HILO)
    {   int32 ry = mr16[v2p(rt)];
        int32 rx = mr16[v2p(rs)];
        if (rx < 0)
        {   RealRegister rs2 = rt==R_R7eqMR3 ? R_R6eqMR2 : R_R7eqMR3;
            move_register(rs2, rs); rs = rs2; rx = v2p(rs);
        }
        if (ry < 0)
        {   RealRegister rt2 = rs==R_R7eqMR3 ? R_R6eqMR2 : R_R7eqMR3;
            move_register(rt2, rt); rt = rt2; ry = v2p(rt);
        }
        if (w == OP_XOR)
            ww = OP16_CMP | rx << 8 | ry << 5;
        else if (w == OP_SLT)
            ww = OP16_SLT | rx << 8 | ry << 5;
        else if (w == OP_SLTU)
            ww = OP16_SLTU | rx << 8 | ry << 5;
        else if (w == OP_MULT)
            ww = OP16_MULT | rx << 8 | ry << 5;
        else if (w == OP_DIV)
            ww = OP16_DIV | rx << 8 | ry << 5;
        else if (w == OP_DIVU)
            ww = OP16_DIVU | rx << 8 | ry << 5;
        else
        {   syserr("outinstrDST(%.8lx, %d, %d, %d)", w, rd, rs, rt);
            ww = OP16_BREAK;
        }
    }
    else
    {   int32 ry = mr16[v2p(rt)];
        int32 rx = mr16[v2p(rs)];
        int32 rz = mr16[v2p(rd)];
        bool asymm = (w == OP_SLLV || w == OP_SRLV || w == OP_SRAV);
        if (asymm)
        {   /* MIPS32 has rs/rt swapped from what MIPS16 expects.       */
            RealRegister t1; int32 t2;
            t1 = rt; rt = rs; rs = t1;
            t2 = ry; ry = rx; rx = t2;
        }
        if (rz < 0)
        {   ru = R_R7eqMR3; rz = v2p(ru);
            postmov = OP16_MOV32R | v2p(rd) & 24 | (v2p(rd) & 7) << 5 | rz;
        }
        if (rx < 0)
        {   RealRegister rs2 = ru!=rt ? ru :
                         rt==R_R7eqMR3 ? R_R6eqMR2 : R_R7eqMR3;
            move_register(rs2, rs); rs = rs2; rx = mr16[v2p(rs)];
        }
        if (ry < 0 || asymm && rz==ry)
        {   RealRegister rt2 = ru!=rs && !asymm ? ru :
                         (rs==R_R7eqMR3 || asymm && ru==R_R7eqMR3) ? R_R6eqMR2 :
                         R_R7eqMR3;
            move_register(rt2, rt); rt = rt2; ry = mr16[v2p(rt)];
        }
        if ((w == OP_ADDU || w == OP_SUBU))
            ww = (w==OP_ADDU ? OP16_ADDU:OP16_SUBU) | rx<<8 | ry<<5 | rz<<2;
        else
        {   if (rz == rx);
            else if (rz == ry)
            {   if (asymm)
                { syserr("outinstrDST(asym=%.8lx, %d, %d, %d)", w, rd, rs, rt);
                  outinstr0(OP16_BREAK, reads_(0,0,0,0), writes_(0,0,OP_NULL));
                }
                ry = rx;
            }
            else
            {    move_register(ru, rs);
                 rs = ru;
            }
            if (w == OP_AND)
                ww = OP16_AND | rz << 8 | ry << 5;
            else if (w == OP_OR)
                ww = OP16_OR | rz << 8 | ry << 5;
            else if (w == OP_XOR)
                ww = OP16_XOR | rz << 8 | ry << 5;
            /* note the following have swapped rx/ry operands */
            else if (w == OP_SLLV)
                ww = OP16_SLLV | ry << 8 | rz << 5;
            else if (w == OP_SRLV)
                ww = OP16_SRLV | ry << 8 | rz << 5;
            else if (w == OP_SRAV)
                ww = OP16_SRAV | ry << 8 | rz << 5;
            else
            { syserr("outinstrDST(%.8lx, %d, %d, %d)", w, rd, rs, rt);
              ww = OP16_BREAK;
            }
        }
    }
    outinstr0(ww, reads_(rs, rt, 0, 0), writes_(ru, 0, OP_NULL));
    if (postmov)
        outinstr0(postmov, reads_(ru, 0, 0, 0), writes_(rd, 0, OP_NULL));
}

static void outinstrDSK(int32 w, RealRegister rd, RealRegister rs, int32 n)
{
    int32 postmov = 0;
    int32 ww;
    RealRegister ru = rd;
    RealRegister rt = R_TM;
    if (!(mips_opt & 2))
    {   load_integer(rt, n);
        ww = w | F_RD(rd) | F_RS(rs) | F_RT(rt);
    }
    else if (rd == R_T24 || rd == R_HILO)
    {   int32 ry;
        int32 rx = mr16[v2p(rs)];
        if (rx < 0)
        {   move_register(R_R7eqMR3, rs); rs = R_R7eqMR3; rx = v2p(rs);
        }
        if (rs == R_R7eqMR3) syserr("gen.c(zh6)");
        rt = R_R7eqMR3; ry = v2p(rt);
        load_integer(rt, n);
        if (w == OP_XOR)
            ww = OP16_CMP | rx << 8 | ry << 5;
        else if (w == OP_SLT)
            ww = OP16_SLT | rx << 8 | ry << 5;
        else if (w == OP_SLTU)
            ww = OP16_SLTU | rx << 8 | ry << 5;
        else if (w == OP_MULT)
            ww = OP16_MULT | rx << 8 | ry << 5;
        else if (w == OP_DIV)
            ww = OP16_DIV | rx << 8 | ry << 5;
        else if (w == OP_DIVU)
            ww = OP16_DIVU | rx << 8 | ry << 5;
        else
        {   syserr("outinstrDSK(%.8lx, %d, %d, %.8lx)", w, rd, rs, n);
            ww = OP16_BREAK;
        }
    }
    else
    {   int32 ry;
        int32 rx = mr16[v2p(rs)];
        int32 rz = mr16[v2p(rd)];
        if (rz < 0)
        {   ru = R_R7eqMR3; rz = v2p(ru);
            postmov = OP16_MOV32R | v2p(rd) & 24 | (v2p(rd) & 7) << 5 | rz;
        }
        if (rx < 0)
        {   move_register(ru, rs); rs = ru; rx = rz;
        }
        if (ru==rs)
        {   if (rs == R_R7eqMR3) syserr("gen.c(zh6)");
            rt = R_R7eqMR3; ry = v2p(rt);
        }
        else
        {   rt = ru!=rs ? ru : rs==R_R6eqMR2 ? R_R7eqMR3 : R_R6eqMR2;
            ry = mr16[v2p(rt)];
        }
        load_integer(rt, n);
        if ((w == OP_ADDU || w == OP_SUBU))
            ww = (w==OP_ADDU ? OP16_ADDU:OP16_SUBU) | rx<<8 | ry<<5 | rz<<2;
        else
        {   /* only symmetric here */
            if (rz == ry) ry = rx;
            if (w == OP_AND)
                ww = OP16_AND | rz << 8 | ry << 5;
            else if (w == OP_OR)
                ww = OP16_OR | rz << 8 | ry << 5;
            else if (w == OP_XOR)
                ww = OP16_XOR | rz << 8 | ry << 5;
            else
            { syserr("outinstrDSK(%.8lx, %d, %d, %.8lx)", w, rd, rs, n);
              ww = OP16_BREAK;
            }
        }
    }
    outinstr0(ww, reads_(rs, rt, 0, 0), writes_(ru, 0, OP_NULL));
    if (postmov)
        outinstr0(postmov, reads_(ru, 0, 0, 0), writes_(rd, 0, OP_NULL));
}

static void outinstrB(int32 w, RealRegister rs, RealRegister rt,
                      LabelNumber *lab)
{
    int32 ww;
    int32 reftype = LABREF_OFF16;
    if (!(mips_opt & 2))
        ww = w | F_RS(rs) | F_RT(rt);
    else
    {   int32 rx;
        if (rt == 0) rt = rs;
        else if (rs != 0) syserr("outinstrB(%d %d)", rs, rt);
        if (!(w == OP_BEQ || w == OP_BNE)) syserr("outinstrB(%.8x)", w);
        reftype = LABREF_M16_S8;
        rx = mr16[v2p(rt)];
        if (rt == 0 && OP_BEQ)
        {   ww = OP16_B, reftype = LABREF_M16_S11;
        }
        else if (rt == R_T24) ww = (w==OP_BEQ ? OP16_BTEQZ : OP16_BTNEZ);
        else
        {   if (rx < 0)
            {   move_register(R_R7eqMR3, rt); rt = R_R7eqMR3; rx = v2p(rt);
            }
            ww = (w==OP_BEQ ? OP16_BEQZ : OP16_BNEZ) | rx<<8;
        }
    }
    outinstr2(ww, reads_(rs, rt, 0, 0), writes_(0,0,OP_BR16), lab, reftype);
}

static void outinstrJR(int32 w, RealRegister rd, RealRegister rs)
{
    int32 ww;
    if (!(mips_opt & 2))
        ww = w | F_RS(rs) | F_RD(rd);
    else
    {   int32 rx = mr16[v2p(rs)];
        if (rs == R_LR)
            ww = OP16_JR_LR;
        else
        {   if (rx < 0)
            {   move_register(R_R7eqMR3, rs); rs = R_R7eqMR3; rx = v2p(rs);
            }
            ww = OP16_JR | (rx << 8);
        }
        if (w == OP_JALR) ww ^= (OP16_JALR^OP16_JR);
    }
    outinstr1(ww, reads_(rs, 0, 0, 0), writes_(rd, 0, OP_JUMP));
}

static void op_muldiv(int32 w, bool lo, RealRegister rd,
                      RealRegister rs, RealRegister rt)
{
    outinstrDST(w, R_HILO, rs, rt);
    outinstrDT((lo ? OP_MFLO : OP_MFHI), rd, R_HILO, 0);
/* R3000: note the curious mult/div: we can load HI/LO straight after   */
/* a mult or div, but not then issue another mult/div for 2 ticks.      */
    if (!(mips_opt & (1+2))) { outinstrNOOP(); outinstrNOOP(); }
}

static void op_muldivK(int32 w, bool lo, RealRegister rd,
                      RealRegister rs, int32 n)
{
    outinstrDSK(w, R_HILO, rs, n);
    outinstrDT((lo ? OP_MFLO : OP_MFHI), rd, R_HILO, 0);
/* R3000: note the curious mult/div: we can load HI/LO straight after   */
/* a mult or div, but not then issue another mult/div for 2 ticks.      */
    if (!(mips_opt & (1+2))) { outinstrNOOP(); outinstrNOOP(); }
}

static void move_register(RealRegister r1, RealRegister r2)
{   /* r1 = r2    */
    if (r1!=r2) outinstrDT(OP_OR, r1, r2, 1);
}

static void negate_register(RealRegister r1, RealRegister r2)
{   /*  r1 = -r2  */
    outinstrDT(OP_SUBU, r1, r2, 0);
}

static void not_register(RealRegister r1, RealRegister r2)
{   /*  r1 = ~r2  */
    outinstrDT(OP_NOR, r1, r2, 0);
}

static void load_integer(RealRegister r, unsigned32 n)
/* Set register r to the integer n.                                      */
{
    if ((n & 0xffff0000) == 0)
        outinstrI(OP_ORI | n, r, 0);
    else if (mips_opt & 1)
    {
        if ((n & 0xffffff00) == 0xffffff00)
        {   outinstrI(OP_ORI | ~n, r, 0);
            not_register(r, r);
        }
        else
            outinstr4a(OP_LW, r, 0, n, writes_(r, r, OP_LOAD));
    }
    else if ((n & 0xffff8000) == 0xffff8000)
        outinstrI(OP_ADDI | (n & 0xffff), r, 0);   /* should be ADDIU? */
    else if ((n & 0xffff) == 0)
        outinstrI(OP_LUI | (n>>16), r, 0);
    else
    {   /* drat -- two instructions */
        outinstrI(OP_LUI | (n>>16), r, 0);
        outinstrI(OP_ORI | (n & 0xffff), r, r);
    }
}

static void add_integer(RealRegister r1, RealRegister r2, unsigned32 n)
{
/* Generate code for r1 = r2 + n.                                        */
    if (n == 0) move_register(r1, r2);
    /* The next line is a historical (m88000) accident (shorter there).  */
    /* Keep for compatibility.                                           */
    else if (n == -n && !(mips_opt & 1))
        xor_integer(r1, r2, n);
    else if ((n & 0xffff8000) == 0 || (n & 0xffff8000) == 0xffff8000)
        outinstrI(OP_ADDIU | (n&0xffff), r1, r2);
    else
    {   /* drat -- three instructions (check) */
        /* FIXME: improve several 2 instruction cases */
        if (r2 == R_TM) syserr(syserr_add_int);
        outinstrDSK(OP_ADDU, r1, r2, n);
    }
}

/* FIXME: compare_type overlaps a great deal with cmp_pending.          */
static int compare_type;        /* 1=int, -1=flt */
static int32 compare_no;
static RealRegister compare_reg;

static void compare_integer(RealRegister r, int32 n, int32 cmp,
                            RealRegister sccdest)
{
  compare_no = n;
  compare_type = 1;
  compare_reg = r;
  if (n==0 && !(mips_opt & 1)) return;
  switch (cmp & Q_MASK) {
  case Q_AL:
    break;
  case Q_GE: case Q_HS:
  case Q_LO: case Q_LT:
    if ((n & 0xffff8000) == 0 || (n & 0xffff8000) == 0xffff8000)
      outinstrI((cmp & Q_UBIT ? OP_SLTIU : OP_SLTI) | (n&0xffff), R_T24, r);
    else {
      outinstrDSK((cmp & Q_UBIT ? OP_SLTU : OP_SLT), R_T24, r, n);
    }
    break;
  case Q_EQ: case Q_UEQ:
  case Q_NE: case Q_UNE:
    if (!(mips_opt & 1))
      load_integer(R_TM, n);
    else if (n != 0) {
      if (-0x3fff <= n && n <= (sccdest!=-1 ? 8 : 0)) {
        /* The above line really is 0xc000 (see ADDIU3 s15_ operand).   */
        /* prefer CMP for BTEQ etc, but ADDIU3 for SCCK.                */
        /* FIXME: could exploit DEADBITS here.                          */
        add_integer(sccdest!=-1 ? sccdest : R_R7eqMR3, r, -n);
        compare_reg = sccdest!=-1 ? sccdest : R_R7eqMR3;
      }
      else {
        xor_integer(R_T24, r, n);
        compare_reg = R_T24;
      }
      compare_no = 0;
    }
    break;
  default:
  case Q_LS: case Q_HI:      /* now dealt with elsewhere */
  case Q_LE: case Q_GT:
    syserr("compare_integer %lx", (long)cmp);
    break;
  }
}

static int32 compare_op, compare_r, compare_w;

static void compare_register(RealRegister r1, RealRegister r2, int32 cmp)
{
  compare_type = 0;
  switch (cmp) {
  case Q_EQ: case Q_UEQ:
    if (mips_opt & 1) goto usecmp;
    compare_op = OP_BEQ | F_RS(r1) | F_RT(r2);
    compare_r = reads_(r1, r2, 0, 0);
    compare_w = writes_(0, 0, OP_BR16);
    break;
  case Q_NE: case Q_UNE:
    if (mips_opt & 1) goto usecmp;
    compare_op = OP_BNE | F_RS(r1) | F_RT(r2);
    compare_r = reads_(r1, r2, 0, 0);
    compare_w = writes_(0, 0, OP_BR16);
    break;
  usecmp:
    outinstrDST(OP_XOR, R_T24, r1, r2);
    break;
  case Q_LT: case Q_GE:
  case Q_LO: case Q_HS:
    outinstrDST((cmp & Q_UBIT ? OP_SLTU : OP_SLT), R_T24, r1, r2);
    break;
  case Q_LE: case Q_GT:
  case Q_LS: case Q_HI:
    outinstrDST((cmp & Q_UBIT ? OP_SLTU : OP_SLT), R_T24, r2, r1);
    break;
  }
}


static void compare_fregister(RealRegister r1, RealRegister r2, int32 cmp,
                              int32 fmt)
{   int32 cmpop;
    compare_type = -1;          /* mark floating */
    switch (cmp) {
default: syserr("compare_fregister");
case Q_EQ: case Q_NE: cmpop = OP_C_EQ_S; break;
case Q_LT: case Q_GE: cmpop = OP_C_LT_S; break;
case Q_LE: case Q_GT: cmpop = OP_C_LE_S; break;
    }
    switch (cmp) {
case Q_EQ: case Q_LT: case Q_LE: compare_op = OP_BC1T; break;
case Q_NE: case Q_GE: case Q_GT: compare_op = OP_BC1F; break;
    }
    compare_r = reads_(R_FCC, 0, 0, 0);
    compare_w = writes_(0, 0, OP_BR16);
    outinstrF(cmpop | fmt | F_FS(r1) | F_FT(r2),
              reads_(r1, r2, 0,0), writes_(R_FCC, R_FCC, OP_NULL));
}


#define cmp_defer(type, a,b,m,q) \
  (cmp_pending=type, cmp_pend_r2=a, cmp_pend_r3=b, cmp_pend_m=m, cmp_pend_q=q)

static int32 cmp_undefer_br(int32 brcond)
{
    int32 r2 = cmp_pend_r2, r3 = cmp_pend_r3, m = cmp_pend_m;
    int32 cmpcond = cmp_pend_q;
        /* correct_stack(FORCE);   DONE BY CALLER */
    if (!cmp_pending) return brcond;
    if (brcond == Q_AL) return brcond;
    if (cmpcond != brcond)
      syserr("cmp_defer confused %lx, %lx", cmpcond, brcond);

    if (cmp_pending==2) {        /* rr type */
        compare_register(r2, r3, cmpcond);
    }
    else {                       /* rk type */
        switch (cmpcond) {
case Q_LE:  if (m == 0x7fffffff) cmpcond = Q_AL;
            else { m++; cmpcond = Q_LT; }
            break;
case Q_GT:  if (m == 0x7fffffff) cmpcond = Q_NOT;
            else { m++; cmpcond = Q_GE; }
            break;
case Q_LS:  if (m == 0xffffffff) cmpcond = Q_AL;
            else { m++; cmpcond = Q_LO; }
            break;
case Q_HI:  if (m == 0xffffffff) cmpcond = Q_NOT;
            else { m++; cmpcond = Q_HS; }
            break;
        }
        compare_integer(r2, m, cmpcond, -1);
    }
    return cmpcond;
}

static int32 cmp_undefer_scc(int32 brcond, int k, RealRegister sccdest)
{
    int32 r2 = cmp_pend_r2, r3 = cmp_pend_r3, m = cmp_pend_m;
    int32 cmpcond = cmp_pend_q;
        /* correct_stack(FORCE);   DONE BY CALLER */
    if (!cmp_pending) return brcond;
    if (brcond == Q_AL) return brcond;
    if (cmpcond != brcond)
      syserr("cmp_defer confused %lx, %lx", cmpcond, brcond);

    if (cmp_pending==2) {        /* rr type */
        compare_register(r2, r3, cmpcond);
    }
    else {                       /* rk type */
        switch (cmpcond) {
case Q_LE:  if (m == 0x7fffffff) cmpcond = Q_AL;
            else { m++; cmpcond = Q_LT; }
            break;
case Q_GT:  if (m == 0x7fffffff) cmpcond = Q_NOT;
            else { m++; cmpcond = Q_GE; }
            break;
case Q_LS:  if (m == 0x7fffffff) cmpcond = Q_AL;
            else { m++; cmpcond = Q_LO; }
            break;
case Q_HI:  if (m == 0x7fffffff) cmpcond = Q_NOT;
            else { m++; cmpcond = Q_HS; }
            break;
        }
/* Invert a SCCK(GE) by turning it into an RR SLT (i.e. SCCK LT).       */
        if (power_of_two(k) != -1)      /* easily done with shift/mov.  */
          switch (cmpcond) {
case Q_GE:  if (m == 0x80000000) { cmpcond = Q_AL; break; }
            goto cmpreg;
case Q_HS:  if (m == 0x00000000) { cmpcond = Q_AL; break; }
cmpreg:     cmpcond = Q_NEGATE(cmpcond);
            load_integer(R_R7eqMR3, m-1);
            compare_register(R_R7eqMR3, r2, cmpcond);
            return cmpcond;
        }
        compare_integer(r2, m, cmpcond, sccdest);
    }
    return cmpcond;
}


static void multiply_integer(RealRegister r1, RealRegister r2, unsigned32 n)
{
/* Generate code for r1 = r2 * n.                                    */
/* @@@ do more tricks soon.    Does LDA.D r1,r2[r2] for 9 speedup?   */
    int32 p;
    if (n==0) load_integer(r1, 0);
    else if (n==1) move_register(r1, r2);
    else if (n==-1) negate_register(r1, r2);
    else if ((p = power_of_two(n)) != -1)
        outinstrDT(OP_SLL | SHAMT(p), r1, r2, 0);
    else if ((p = power_of_two(-n)) != -1)
    {   outinstrDT(OP_SLL | SHAMT(p), r1, r2, 0);
        negate_register(r1, r1);
    }
    else
        op_muldivK(OP_MULT, 1, r1, r2, n);
}

static void divide_integer(RealRegister r1, RealRegister r2, unsigned32 n,
                           bool signedp, bool divnotrem)
{
/* Generate code for r1 = r2 / n.                                    */
    if (divnotrem && n==1) move_register(r1, r2);
    else if (divnotrem && n==-1) negate_register(r1, r2);
    else
        op_muldivK((signedp ? OP_DIV : OP_DIVU), divnotrem, r1, r2, n);
}

static void and_integer(RealRegister r1, RealRegister r2, unsigned32 n)
{
/* Generate code for r1 = r2 & n.                                        */
    if (n == ~(unsigned32)0) move_register(r1, r2);
    else if (n==0) load_integer(r1, 0);
/* To understand the following code, remember that AND immediate         */
/* essentially 0-extends                                                 */
    else if (!(mips_opt & 1) && (n & 0xffff0000) == 0)
        outinstrI(OP_ANDI | n, r1, r2);
    else if ((mips_opt & 2) && (n == 0xffff))
        outinstrI(OP_MIPS16+OP16_ZEH, r1, r2);
    else if ((mips_opt & 2) && (n == 0xff))
        outinstrI(OP_MIPS16+OP16_ZEB, r1, r2);
    else {
        if (mips_opt & 2)
        {   int32 i;
            for (i = 1; i<=8; i++)
            {   if (n == (((~(unsigned32)0)<<i & 0xffffffff) >> i))
                {   outinstrDT(OP_SLL | SHAMT(i), r1, r2, 0);
                    outinstrDT(OP_SRL | SHAMT(i), r1, r1, 0);
                    return;
                }
                if (n == (((unsigned32)0xffffffff >> i ) << i))
                {   outinstrDT(OP_SRL | SHAMT(i), r1, r2, 0);
                    outinstrDT(OP_SLL | SHAMT(i), r1, r1, 0);
                    return;
                }
            }
        }
        outinstrDSK(OP_AND, r1, r2, n);
    }
}

static void or_integer(RealRegister r1, RealRegister r2, unsigned32 n)
{
/* Generate code for r1 = r2 | n.                                        */
    if (n==0) move_register(r1, r2);
    else if (n == ~(unsigned32)0) load_integer(r1, n);
    else if (!(mips_opt & 1) && (n & 0xffff0000) == 0)
        outinstrI(OP_ORI | n, r1, r2);
    else {
       /* drat -- at least two instructions */
        outinstrDSK(OP_OR, r1, r2, n);
    }
}

static void xor_integer(RealRegister r1, RealRegister r2, unsigned32 n)
{
/* Generate code for r1 = r2 ^ n.                                        */
    if (n==0) move_register(r1, r2);
    else if (n == ~(unsigned32)0)
        outinstrDT(OP_NOR, r1, r2, 1);
    else if ((n & 0xffff0000) == 0 && (r1 == R_T24 || !(mips_opt & 1)))
        outinstrI(OP_XORI | n, r1, r2);
    else {
       /* drat -- at least two instructions */
        outinstrDSK(OP_XOR, r1, r2, n);
    }
}

static int32 genfplit(FloatCon *fc, bool fpdouble)
{   int32 disp;
    /* The next line is bit of a hack, but reflects that the following  */
    /* code dumps literals straight into lit_findword instead of the    */
    /* shared literal pool.                                             */
    litpool_codelimit = 0;
    if (fpdouble)
    {   disp = lit_findwordsincurpool(fc->floatbin.irep, 2, LIT_FPNUM);
        if (disp == -1)
        {   (void)lit_findwordaux(fc->floatbin.irep[0],
                                  LIT_FPNUM1, fc->floatstr,
                                  LITF_DOUBLE|LITF_INCODE|LITF_FIRST);
            /* use the second address in case overflow */
            disp = lit_findwordaux(fc->floatbin.irep[1],
                                  LIT_FPNUM2, fc->floatstr,
                                  LITF_INCODE|LITF_LAST) - 4;

        }
    }
    else
        disp = lit_findwordaux(fc->floatbin.irep[0],
                               LIT_FPNUM, fc->floatstr,
                               LITF_INCODE|LITF_FIRST|LITF_LAST);
    return disp;
}

static void outrelref(int32 op, RealRegister rt, RealRegister rs, int32 m,
                      int32 type)
{   RealRegister r1=0, r2=0, w1=0;
    if ((((unsigned32)m + 0x8000) & 0xffff0000) != 0)
    {   /* cg.c tries to avoid the following cases, but it can happen via */
        /* LDRV/LDRVK with big stack frames.                              */
        add_integer(R_TM, rs, ((unsigned32)m + 0x8000) & 0xffff0000);
        rs = R_TM;
    }
    switch (type & (OP_LOAD | OP_2WORD))
    {
case 0:                     /* store single */
            r1 = rt; r2 = rs; break;
case OP_LOAD:               /* load single */
            r1 = rs; w1 = rt; break;
case OP_2WORD:              /* double store */
            r1 = rt; r2 = rt+1; break;
case OP_LOAD+OP_2WORD:      /* double load */
            r1 = rs; w1 = rt; break;
    }
  { int32 ww = 0;
    int32 postmov = 0;
    if (!(mips_opt & 2))
        ww = op | F_RT(rt) | F_RS(rs) | (m & 0xffff);
    else
    {   int32 rx = mr16[v2p(rt)];
        int32 rb = mr16[v2p(rs)];
        if (rx < 0 && !(type & OP_LOAD) && !(rt == R_LR && rs == R_SP))
        {   move_register(R_R7eqMR3, rt); r1 = rt = R_R7eqMR3; rx = v2p(rt);
        }
        if (rb < 0 && !(type & OP_LOAD) && !(rs==R_SP && op==OP_SW))
        {   if (rt == R_R7eqMR3) syserr("gen.c(zh6)");
            move_register(R_R7eqMR3, rs); r2 = rs = R_R7eqMR3; rb = v2p(rs);
        }
        if (rb < 0 && type & OP_LOAD && (rs!=R_SP || op!=OP_LW))
        {   move_register(R_R7eqMR3, rs); r1 = rs = R_R7eqMR3; rb = v2p(rs);
        }
        if (rx < 0 && type & OP_LOAD)
        {   w1 = R_R7eqMR3; rx = v2p(w1);
            postmov = OP16_MOV32R | v2p(rt) & 24 | (v2p(rt) & 7) << 5 | rx;
        }
        if (op == OP_LW && rs == R_SP)
            ww = foo8u(m, OP16_LW_SP, 2, rx);
        else if (op == OP_LW && rb >= 0 && rx >= 0)
            ww = foo5u(m, OP16_LW, 2, rx, rb);
        else if (op == OP_LH && rb >= 0 && rx >= 0)
            ww = foo5u(m, OP16_LH, 1, rx, rb);
        else if (op == OP_LHU && rb >= 0 && rx >= 0)
            ww = foo5u(m, OP16_LHU, 1, rx, rb);
        else if (op == OP_LB && rb >= 0 && rx >= 0)
            ww = foo5u(m, OP16_LB, 0, rx, rb);
        else if (op == OP_LBU && rb >= 0 && rx >= 0)
            ww = foo5u(m, OP16_LBU, 0, rx, rb);

        else if (op == OP_SW && rs == R_SP && rx >= 0)
            ww = foo8u(m, OP16_SW_SP, 2, rx);
        else if (op == OP_SW && rs == R_SP && rt == R_LR)
            ww = foo8u(m, OP16_SW_SP_LR, 2, 0);
        else if (op == OP_SW && rb >= 0 && rx >= 0)
            ww = foo5u(m, OP16_SW, 2, rx, rb);
        else if (op == OP_SH && rb >= 0 && rx >= 0)
            ww = foo5u(m, OP16_SH, 1, rx, rb);
        else if (op == OP_SB && rb >= 0 && rx >= 0)
            ww = foo5u(m, OP16_SB, 0, rx, rb);
        else
        { syserr("outrelref(%.8x: %d, %d)", op|m, rs, rt);
          ww = OP16_BREAK;
        }
    }
    outinstr0(ww, reads_(r1, r2, 0, 0), writes_(w1, (type&OP_LOAD?w1:0), type));
    if (postmov)
        outinstr0(postmov, reads_(w1, 0, 0, 0), writes_(rt, 0, OP_NULL));
  }
}

static void imm_extend(RealRegister r1, RealRegister r2, int32 m)
{
    if (!(0<=m && m<=2)) syserr("gen.c(odd extend)");
    if (!(mips_opt & 2))
    {   /* The code here follows that in flowgraf.c:                    */
        int32 k = (m==2) ? 16 : 24;
        outinstrDT(OP_SLL | SHAMT(k), r1, r2, 0);
        outinstrDT(OP_SRA | SHAMT(k), r1, r1, 0);
    }
    else
    {
        outinstrI((m==2) ? OP_MIPS16+OP16_SEH : OP_MIPS16+OP16_SEB, r1, r2);
    }
}

static void mips16_save(bool save, unsigned32 mask, int32 size)
{   int32 slo = size >> 3 & 15;
    int32 shi = size >> 7 & 15;
    if (size != ((shi<<4 | slo) << 3))
        syserr("gen.c(mips16 SAVE/RESTORE (bad size %ld)", size);
    if (!(mask >> 16) && size == 128) mask |= shi << 20;
    if (mask >> 16) mask |= (unsigned32)OP16_EXT << 16;
    flush_peepholer();
    /* The reads/writes are a lie here! (rescued by flush_peepholer())  */
    outinstr0((save?OP16_SAVE:OP16_RESTORE) | mask | slo,
             reads_(R_SP, 0, 0, 0), writes_(R_SP, 0, OP_NULL));
    flush_peepholer();
    change_sp(save ? size : -size);
}

/* save_regs() normally returns 0, but if apush is set to a non-zero    */
/* number of arg regs and these can be stored in aoffset by the         */
/* push which also saves R_V1 etc then do so and return 1.              */
/* If apush is set to non-zero and this is impossible also return 0.    */
/* See calls.                                                           */
static bool save_regs(unsigned32 mask, int32 offset,
                      unsigned32 apush, int32 aoffset, int32 hint_push)
{   int32 i;
    int32 off = offset + sp_adjust+SP_OFFSET;
    if (mask == 0) return 0;
    if (mips_opt & 2 && !(mips_opt & 8))
    {   static char as_code[5] = { 0, 4, 8, 12, 14 };
        int32 xs = -2, ss = 0, as = 0;
        int32 mask2 = mask, mask3 = mask, mask4 = mask, smask = 0;
        int32 aoff = aoffset + sp_adjust+SP_OFFSET;
        if (apush && aoff != 0) return 0;
        for (i =  R_A1; i<=R_A1+3; i++)
            if (mask4 & regbit(i)) as++, mask4 ^= regbit(i); else break;
        if (off == 0 && !(mask & ~(regbit(R_A1+4)-regbit(R_A1))) && mask4 == 0)
            { if (apush) return 0;
              mips16_save(1, ((int32)as_code[as]) << 16,
                          hint_push ? (as+1 >> 1) * 8 : 0);
              return 0; }
        if (mask2 & regbit(R_LR)) smask |= SR_RA, mask2 ^= regbit(R_LR);
        for (i = R_V1; i<R_V1+NVARREGS && i<R_V1+9; i++)
            /* see v2p() here for v2p(R_V1+8) = $30. */
            if (mask2 & regbit(i)) xs++, mask2 ^= regbit(i); else break;
        for (i =  R_A1+3; i>=R_A1; i--)
            if (mask3 & regbit(i)) ss++, mask3 ^= regbit(i); else break;
        if (xs >= -1) smask |= SR_S0;
        if (xs >= 0) smask |= SR_S1;
        if (xs > 0) smask |= xs<<24;
        if (mask2 == 0 && off + 4*bitcount(mask) == 0 && apush <= 4)
            { /* here's the place we can deal with apush...             */
              mips16_save(1, smask | (int32)(as_code[apush]) << 16,
                          -(off-SP_OFFSET & -8));
              return 1; }
        else if (mask3 == 0 && off + 4*bitcount(mask) == 0)
            { if (apush) return 0;
              mips16_save(1, (ss==4 ? 11:ss)<<16, -(off-SP_OFFSET & -8));
              return 0; }
        /* if saving 3 regs A1,A2,A3 may as well save A4 (saves space). */
        else if (ss == 0 && xs == -2 && mask3 == regbit(R_A1+3)-regbit(R_A1) &&
                 off + 4*bitcount(mask|regbit(R_A1+3)) == 0)
            { if (apush) return 0;
              mips16_save(1, 11<<16, -(off-SP_OFFSET & -8));
              return 0; }
        else if (ss == 0 && xs == -2 &&
                 (mask3 & ~regbit(R_A1+1)) == regbit(R_A1) &&
                 off + 4*bitcount(mask|regbit(R_A1+1)) == 0)
            { if (apush) return 0;
              outinstr0(foo8s(-8, OP16_ADJSP, 3, 0),
                       reads_(R_SP, 0, 0, 0), writes_(R_SP, 0, OP_NULL));
              change_sp(8);
              mips16_save(1, (mask3 & regbit(R_A1+1)?8L:4L)<<16, 0);
              return 0; }
        else {
            if (apush) return 0;
#ifdef never
            cc_msg("BEWARE: cannot use SAVE(mask=%lx, off=%ld), mask2=%lx, mask3=%lx, smask=%lx\n",
                    mask, off, mask2, mask3, smask);
#endif
        }
    }
    if (apush) return 0;
    if (off < SP_COVER) {
      ensure_sp(); off = offset + sp_adjust+SP_OFFSET; }
    for (i = 0; i < 32; i++)  /* NMAGICREGS > 32 beware */
    { if (mask & regbit(i))
        outrelref(OP_SW, i, R_SP, off, OP_STORE),
        off+=4;
    }
    /* The following is a rough-and-ready estimate .... */
    if (mips_opt & 2) wasted_in_save += 2*(bitcount(mask));
    return 0;
}

static void restore_regs(unsigned32 mask, int32 offset)
{   int32 i;
    int32 off = offset + sp_adjust+SP_OFFSET;
    if (mask == 0) return;
    if (mips_opt & 2 && !(mips_opt & 8))
    {   int32 xs = -2, mask2 = mask, smask = 0;
        if (mask2 & regbit(R_LR)) smask |= SR_RA, mask2 ^= regbit(R_LR);
        for (i = R_V1; i<R_V1+NVARREGS && i<R_V1+9; i++)
            /* see v2p() here for v2p(R_V1+8) = $30. */
            if (mask2 & regbit(i)) xs++, mask2 ^= regbit(i); else break;
        if (xs >= -1) smask |= SR_S0;
        if (xs >= 0) smask |= SR_S1;
        if (xs > 0) smask |= xs<<24;
        if (mask2 == 0 && (off + 4*bitcount(mask) & 7) == 0)
            { if (off + 4*bitcount(mask) > 255*8) {
                ensure_sp(); off = offset + sp_adjust+SP_OFFSET; }
              mips16_save(0, smask, off + 4*bitcount(mask));
              return; }
        else {
#ifdef never
            cc_msg("BEWARE: cannot use RESTORE(mask=%lx, off=%ld), mask2=%lx, smask=%lx\n",
                    mask, off, mask2, smask);
#endif
        }
    }
    /* saving of double/fp regs buggy here */
    /* This confusion is to reverse the loop
       which has the advantage of optimising
       delayed loads                         */
    for (i = 0; i < 32; i++) if (mask & regbit(i)) off+=4;
    for (i = 31; i>=0; i--)  /* NMAGICREGS > 32 beware */
    {
       if (mask & regbit(i))
        off -= 4,
        outrelref(OP_LW, i, R_SP, off, OP_LOAD);
    }
    /* The following is a rough-and-ready estimate .... */
    if (mips_opt & 2) wasted_in_restore += 2*(bitcount(mask));
}

static void save_fregs(unsigned32 fmask, int32 offset)
{   int32 i;
    int32 off = offset + sp_adjust+SP_OFFSET;
    if (off < SP_COVER) {
      ensure_sp(); off = offset + sp_adjust+SP_OFFSET; }
    for (i = 0; i < 16; i++)  /* NMAGICREGS > 32 beware */
    { int32 r = i+32;
      if (fmask & regbit(i))
        outinstrF(OP_SWC1 | F_FT(MSR(r)) | F_RS(R_SP) | off&0xffff,
         reads_(R_SP, MSR(r), 0, 0), writes_(0, 0, OP_STORE)),
        outinstrF(OP_SWC1 | F_FT(LSR(r)) | F_RS(R_SP) | ((off+4)&0xffff),
         reads_(R_SP, LSR(r), 0, 0), writes_(0, 0, OP_STORE)),
        off += 8;
    }
}

static void restore_fregs(unsigned32 fmask, int32 offset)
{   int32 i;
    offset += sp_adjust+SP_OFFSET;
    for (i = 0; i < 16; i++) if (fmask & regbit(i)) offset+=8;
    for (i = 15; i>=0; i--)  /* NMAGICREGS > 32 beware */
    { int32 r = i+32;
      if (fmask & regbit(i))
        offset-=8,
        outinstrF(OP_LWC1 | F_FT(LSR(r)) | F_RS(R_SP) | offset+4&0xffff,
         reads_(R_SP, 0, 0, 0), writes_(LSR(r), LSR(r), OP_LOAD)),
        outinstrF(OP_LWC1 | F_FT(MSR(r)) | F_RS(R_SP) | offset&0xffff,
         reads_(R_SP, 0, 0, 0), writes_(MSR(r), MSR(r), OP_LOAD));
    }
}

#define M_FVARREGS (regbit(R_FV1+NFLTVARREGS-32) - regbit(R_FV1-32))

/* Note that on the MIPS 'ni' below is not the number of int reg args   */
/* but rather (one beyond) the max int arg used.  The real number       */
/* of int regs used is ni-2*nf due to the odd calling standard.         */
static void routine_entry(int32 mw)
{   int32 m = k_argwords_(mw);
/*    int32 ni = k_intregs_(mw); */
    int32 nipush = m>NARGREGS ? NARGREGS : m;
    int32 nf = k_fltregs_(mw);
#ifndef TARGET_FP_ARGS_IN_FP_REGS
#error k_intregs_() not set properly (see flowgraf.c/case J_ENTER)
#endif
    fp_minus_sp = 0;
    sp_adjust = 0;
    returnlab = nextlabel();
    firstargoff = 0;
    {   int32 mask = ((regmaskvec.map[0]) & M_VARREGS) |
                     (procflags & NONLEAF ?
                        regbit(R_LR) :
                        regmaskvec.map[0] & (int)regbit(R_LR));
        int32 fmask = regmaskvec.map[1] & M_FVARREGS;
        int32 maskarg = procflags & PROC_ARGPUSH ?
                    regbit(R_A1+nipush)-regbit(R_A1+2*nf) : 0;
        int32 fmaskarg = procflags & PROC_ARGPUSH ?
                    regbit(R_FA1-32+nf)-regbit(R_FA1-32) : 0;
        /* In the next line note that ARGPUSH implies NONLEAF.          */
        int32 argspace = (procflags & NONLEAF || mask != 0) ?
                             (ABIWASTE ? 16 :
                                4*bitcount(maskarg) + 8*bitcount(fmaskarg)) :
                             0;
        argpush_effective = ABIWASTE ? (procflags & NONLEAF || mask != 0) :
                                       (procflags & PROC_ARGPUSH);
        max_arg_number = m;
        firstsaveoff = 4*(bitcount(mask) & 1);          /* align saves */
        firstargoff = 4*bitcount(mask) + firstsaveoff;
	firstsaveoff += 8*bitcount(fmask), firstargoff += 8*bitcount(fmask);
        argregs_pushed = padsize(argspace, alignof_double);
        change_sp(-(firstargoff+argregs_pushed));
/*
 * The SP adjustment above is guaranteed to be by an even number of
 * words, so SP is doubleword aligned.  I will keep that so for the
 * rest of this code...
 */
/* The next two lines save registers of a va_arg fn (or any other in    */
/* which the address of an arg is taken) contiguously with any other    */
/* args which are pushed by the caller.  There is a problem here:       */
/* a given a fn f(double x, ...) then the 2nd arg will be expected in   */
/* two int regs and so f(1.2,3.4) will cause va_arg to look in the      */
/* wrong place for ...va_start(x); va_arg(double)...  The solution      */
/* is probably to save the 2nd arg reg in this case in a slightly       */
/* unusual position and have va_arg take more care with access.         */
#define gen_warn_va_double \
   "calls to va_arg(double) may be mis-compiled in this function"
        if (nf==1 && procflags & PROC_ARGPUSH)
            cc_warn(gen_warn_va_double);
        if (nf == 0 && fmaskarg == 0 && fmask == 0 &&
            (procflags & PROC_ARGPUSH) && nipush != 0 &&
            save_regs(mask, firstsaveoff, nipush, firstargoff, 0));
        else {
          save_regs(maskarg, firstargoff+8*nf, 0, 0, 0);
          save_fregs(fmaskarg, firstargoff);
          save_regs(mask, firstsaveoff, 0, 0, 0);
          save_fregs(fmask, 0);
        }
    }
}

/*
 * Restore all registers for routine exit.
 */

enum RE_Reason { RE_return, RE_tailcall, RE_endproc };

static void routine_exit(enum RE_Reason why)
{   /* remove locals, and args if pushed on stack (i.e. if ARGPUSH) */
    int32 mask = ((regmaskvec.map)[0] & M_VARREGS) |
                 (procflags & NONLEAF ? regbit(R_LR) :
                          regmaskvec.map[0] & regbit(R_LR));
    int32 fmask = regmaskvec.map[1] & M_FVARREGS;

    /* Tidying this code all together shows it to be confused...        */
    if (why != RE_tailcall)
    {   /* if get get an unconditional return expand it inline */
        /* and save its address if it was the first            */
/*
 * On the MIPS it can sometimes hurt to set a label here on a speculative
 * basis - doing so will flush the peepholer and force the return NOT to
 * get converted to use a jmp.n
 * If routine_exit() is going to generate real instructions do it -
 * otherwise do not.  Note that this interacts with some rather dodgy code
 * at J_ENDPROC in mips/gen.c where returnlab gets created if needbe at the
 * end of a procedure.
 */
        int32 xmask = ((regmaskvec.map)[0] & M_VARREGS) |
                       (procflags & NONLEAF ? regbit(R_LR) : 0);
        /* >= 0x7ffffffe means 'unset', possibly referenced.        */
        if (why != RE_endproc && (mips_opt & 1) && sp_adjust == 0
                              && !(returnlab->lndraft >= 0x7ffffffe))
        {   outinstrB(OP_BEQ, 0, 0, returnlab);
            return;
        }
        /* >= 0x7ffffffe means 'unset', possibly referenced.        */
        if (returnlab->lndraft >= 0x7ffffffe && xmask != 0 && sp_adjust == 0)
            Xsetlabel(returnlab,0);
        dbg_return(codebase+codep);
        /* >= 0x7ffffffe means 'unset', possibly referenced.        */
        if (sp_adjust==0 && returnlab->lndraft >= 0x7ffffffe)
        {   /* cnt == 0 iff we are likely to have a bare JR delay slot...   */
            int32 cnt = 4*bitcount(mask) + 8*bitcount(fmask) +
                  (fp_minus_sp+firstargoff+sp_adjust+argregs_pushed ? 1 : 0);
            if (cnt != 0) Xsetlabel(returnlab, 0);
        }
    }
    restore_fregs(fmask, fp_minus_sp+0);
    restore_regs(mask, fp_minus_sp+firstsaveoff);
    add_integer(R_SP, R_SP, fp_minus_sp+firstargoff+sp_adjust+argregs_pushed);
    adjusted_sp();      /* not strictly needed as a label will follow.  */
    if (why != RE_tailcall) outinstrJR(OP_JR, 0, R_LR);
}

/* vvvvvvvvvvvvvvvvvvvvvv    PEEPHOLER    vvvvvvvvvvvvvvvvvvvv */

typedef struct mcpeepdata mcpeepdata;
static int32 pcdraft;   /* , lastlit; */
static struct mcpeepdata *bb_list;
static struct mcpeepdata *new_mcpeep(void);
static void emit_one_peeped(struct mcpeepdata *);
static void emit_all_peeped(void);
static void setlab1(LabelNumber *l);

/* ^^^^^^^^^^^^^^^^^^^^^^    PEEPHOLER    ^^^^^^^^^^^^^^^^^^^^ */

/* vvvvvvvvvvv MIPS16 shared literal pool stuff:  vvvvvvvvvvvv */

typedef struct SLP {
    int32 npools;       /* nth literal pool, starting from 1            */
    Symstr *name;       /* name of literal pool (zero if empty)         */
    int32 codebase;     /* codebase of first user                       */
    struct { Symstr *sym; int32 off; } entries[1024];
} SLP;

static SLP *shared_litpool;
static int32 shared_litpool_size;

static int32 find_shared_lit(Symstr *sym, int32 off)
{   int32 i, n;
    if (shared_litpool == 0)
    {   shared_litpool = (SLP *)GlobAlloc(SU_Other, sizeof(SLP));
        shared_litpool->npools = 0;
        shared_litpool_size = 0;
    }
    n = shared_litpool_size;
    if (n == 0)
    {   char v[128];
        sprintf(v, "__litpool_%d", ++shared_litpool->npools);
        shared_litpool->name = sym_insert_id(v);
        shared_litpool->codebase = codebase;
    }
    for (i = 0; i<n; i++)
      if (shared_litpool->entries[i].sym == sym &&
          shared_litpool->entries[i].off == off)
        return i*4;
    shared_litpool_size++;
    if (n == 1024) syserr("gen.c(shared literal pool overflow)");
    shared_litpool->entries[n].sym = sym;
    shared_litpool->entries[n].off = off;
    return n*4;
}

typedef struct P1LP {
    struct { Symstr *sym; int32 off; int nrefs; } entries[1024];
} P1LP;

static P1LP *phase1_litpool;
static int32 phase1_litpool_size;

static void note_phase1_lit(Symstr *sym, int32 off, int32 weight)
{   int32 i, n;
    if (phase1_litpool == 0)
    {   phase1_litpool = (P1LP *)GlobAlloc(SU_Other, sizeof(P1LP));
        phase1_litpool_size = 0;
    }
    n = phase1_litpool_size;
    for (i = 0; i<n; i++)
      if (phase1_litpool->entries[i].sym == sym &&
          phase1_litpool->entries[i].off == off)
      { phase1_litpool->entries[i].nrefs += weight;
        return;
      }
    phase1_litpool_size++;
    if (n == 1024) syserr("gen.c(phase1 literal pool overflow)");
    phase1_litpool->entries[n].sym = sym;
    phase1_litpool->entries[n].off = off;
    phase1_litpool->entries[n].nrefs = weight;
}

static void note_shared_lits_as_phase1()
{
    int32 i, n = shared_litpool_size;
    for (i = 0; i<n; i++)
      note_phase1_lit(shared_litpool->entries[i].sym,
                      shared_litpool->entries[i].off, 2);
}

/* improve_phase1_lit maps a single-use literal with a non-neat offset  */
/* (here neat means 256-multiple) into a neat offset for a ADDIU.       */
/* We try harder to share (e.g. strings) if we can't even find a neat   */
/* literal.                                                             */
/* Postcondition: the result 'noff' must be so that off-noff is in the  */
/* range -128..+127.                                                    */
static int32 improve_phase1_lit(Symstr *sym, int32 off)
{   int32 i, j, n;
    /* cc_msg("improve $r+%d", sym, off); */
    n = phase1_litpool_size;
    for (i = 0; i<n; i++)
      if (phase1_litpool->entries[i].sym == sym &&
          phase1_litpool->entries[i].off == off) break;
    if (i == n) syserr("gen.c(improve phase1 literal)");
    if (phase1_litpool->entries[i].nrefs != 1)
      return off;
    for (j = 0; j<n; j++)
    { int32 noff = (off+128) & ~255;
      if (phase1_litpool->entries[j].sym == sym &&
          phase1_litpool->entries[j].off == noff)
      { phase1_litpool->entries[j].nrefs++;
        /* cc_msg("=>1=> %d\n", noff); */
        if (off-noff+128 >> 8) syserr("iphl1");
        return noff;
      }
    }
    for (j = 0; j<n; j++)
    { int32 noff = phase1_litpool->entries[j].off;
      if (phase1_litpool->entries[j].sym == sym &&
          phase1_litpool->entries[j].nrefs != 1 &&
          (off-noff+128 >> 8) == 0)
      { /* cc_msg("=>2=> %d\n", noff); */
        if (off-noff+128 >> 8) syserr("iphl2");
        return noff;
      }
    }
    /* Doing this backward helps with string literals which are         */
    /* allocated forwards in the pool.                                  */
    for (j = n-1; j>=0; j--) if (j != i)
    { int32 noff = phase1_litpool->entries[j].off;
      if (phase1_litpool->entries[j].sym == sym &&
          (off-noff+128 >> 8) == 0)
      { phase1_litpool->entries[j].nrefs++;
        /* cc_msg("=>3=> %d\n", noff); */
        if (off-noff+128 >> 8) syserr("iphl3");
        return noff;
      }
    }
    /* cc_msg("=>4=> %d\n", off); */
    return off;
}

/* ^^^^^^^^^^^ MIPS16 shared literal pool stuff:  ^^^^^^^^^^^^ */

static bool lose_next_bxx = NO;

static void show_inst_direct(J_OPCODE op, RealRegister r1, RealRegister r2, int32 m,
                             int32 peep)
/* The types of the arguments here are rather unsatisfactory - in        */
/* particular the last one (m) is really a big union.                    */
{
    int32 illbits;
    int32 opm = op & ~(Q_MASK | J_SIGNED | J_UNSIGNED | J_ALIGNMENT);
    RealRegister r1r = (int32)r1, r2r = (int32)r2, mr = (int32)m;
/* Is this the way to do it?  Concern over code quality - the compiler
 * clearly needs to know how to put union values of size 4 in registers.
 *  union { Symstr *sym; int32 umint; } um;
 *  um.umint = m;
 */
    /* The odd hacks in the next line deal with preliminary floating point */
    if (uses_r1(op)) { if (r1r >= 48ul) syserr(syserr_r1r, (long)r1r); }
    if (uses_r2(op)) { if (r2r >= 48ul) syserr(syserr_r2r, (long)r2r); }
    if (uses_r3(op)) { if (mr >= 48ul) syserr(syserr_mr, (long)mr); }

    if (cmp_pending) {
      if (!(opm == J_B || opm == J_SCCK)) cmp_pending = 0;
    }
    else if ((opm == J_B || opm == J_SCCK) && (op & Q_MASK) != Q_AL) {
      syserr("gen.c(branch %.8lx without compare)", (long)op);
    }

    /* if (pcdraft > lastlit+0x0400) outltorg(), lastlit = pcdraft; */

/* to enable future JOPCODE peephole optimisation expand_jop_macros()
   tries quite hard not to call show_inst() with instructions with
   no effect.
*/
    /* illbits (and peep) checks that unexpected (erroneous!) bits are not set */
    illbits = op & (Q_MASK | J_SIGNED | J_UNSIGNED);/* Note no alignment */
    switch (opm)
    {
#ifdef J_WORD
case J_WORD:
        outinstr0(m, reads_(0, 0, 0, 0), writes_(0, 0, OP_WORD));
        break;
#endif
case J_CMPK:
        cmp_defer(1, r2r, -1, m, op & Q_MASK);
        illbits &= ~Q_MASK;
        break;
case J_MOVR:
        if (r1r==mr) syserr(syserr_remove_noops);
        move_register(r1r, mr);
        break;
case J_NEGR:
        negate_register(r1r, mr);
        break;
case J_NOTR:
        not_register(r1r, mr);
        break;
case J_MOVK:
        load_integer(r1r, m);
        break;
case J_SUBK:
        m = -m; /* drop through */
case J_ADDK:
 /* SP should not appear as r1 field -- SETSP is used.  However, note that  */
 /* local varable addresses do have r2=SP.  Note that then local_address()  */
 /* has taken care of sp_adjust, and so we should NOT do so here.           */
        if (r2r == R_SP && r1r == R_SP) syserr("gen.c(ADDK(SP))");
        if (r2r == R_SP && r1r == R_SP) {change_sp(m);}
        else add_integer(r1r, r2r, m);
        break;
case J_ANDK:
        and_integer(r1r, r2r, m);
        break;
case J_ORRK:
        or_integer(r1r, r2r, m);
        break;
case J_EORK:
        xor_integer(r1r, r2r, m);
        break;

case J_MULK:
        multiply_integer(r1r, r2r, m);
        break;
case J_DIVK:
        divide_integer(r1r, r2r, m, (op & J_UNSIGNED)==0, 1);
        illbits &= ~(J_SIGNED|J_UNSIGNED);
        break;
case J_REMK:
        divide_integer(r1r, r2r, m, (op & J_UNSIGNED)==0, 0);
        illbits &= ~(J_SIGNED|J_UNSIGNED);
        break;

case J_SHRK:
        if (m<=0 || m>31) syserr(syserr_silly_shift, (long)m);
        outinstrDT((op & J_SIGNED ? OP_SRA : OP_SRL) | SHAMT(m), r1r, r2r, 0);
        illbits &= ~(J_SIGNED|J_UNSIGNED);
        break;
case J_SHLK:
        if (m<=0 || m>31) syserr(syserr_silly_shift, (long)m);
        outinstrDT(OP_SLL | SHAMT(m), r1r, r2r, 0);
        illbits &= ~(J_SIGNED|J_UNSIGNED);
        break;
case J_EXTEND:
        imm_extend(r1r, r2r, m);
        break;

case J_SHLR:
        /* do a OP_TBND here for >31 shifts? */
        outinstrDST(OP_SLLV, r1r, mr, r2r);
        illbits &= ~(J_SIGNED|J_UNSIGNED);
        break;
case J_SHRR:
        /* do a OP_TBND here for >31 shifts? */
        outinstrDST((op & J_SIGNED ? OP_SRAV : OP_SRLV), r1r, mr, r2r);
        illbits &= ~(J_SIGNED|J_UNSIGNED);
        break;
case J_CMPR:
        cmp_defer(2, r2r, mr, 0, op & Q_MASK);
        illbits &= ~Q_MASK;
        break;

case J_ANDR:
        outinstrDST(OP_AND, r1r, r2r, mr); break;
case J_ORRR:
        outinstrDST(OP_OR, r1r, r2r, mr); break;
case J_EORR:
        outinstrDST(OP_XOR, r1r, r2r, mr); break;
case J_ADDR:
        outinstrDST(OP_ADDU, r1r, r2r, mr); break;
case J_SUBR:
        outinstrDST(OP_SUBU, r1r, r2r, mr); break;
case J_MULR:
        op_muldiv(OP_MULT, 1, r1r, r2r, mr); break;
case J_DIVR:
        op_muldiv((op&J_UNSIGNED ? OP_DIVU : OP_DIV), 1, r1r, r2r, mr);
        illbits &= ~(J_SIGNED|J_UNSIGNED);
        break;
case J_REMR:
        op_muldiv((op&J_UNSIGNED ? OP_DIVU : OP_DIV), 0, r1r, r2r, mr);
        illbits &= ~(J_SIGNED|J_UNSIGNED);
        break;

/* case J_RSBR: this cannot happen unless TARGET_HAS_SCALED_OPS */

case J_OPSYSK:
	{
	  LabelNumber *ss = nextlabel();
	  load_integer(R_SYSCALLno, m);	/* Trap number in register $v0 */
	  outinstrF(OP_SYSCALL, reads_(R_A1, R_A1+1, R_A1+2, R_A1+3),
		                writes_(R_SYSCALLres, R_A1+3, OP_NULL));
	  compare_integer(R_A1+3, 0, op, -1);
	  conditional_branch_to_n(Q_EQ, ss);
          /* The next 2 lines could be merged, but not on the MIPS16.   */
	  external_ref(OP_ADDIU, R_TM, 0, targeterrno, OP_NULL);
          outrelref(OP_SW, R_SYSCALLres, R_TM, 0, OP_STORE);
	  load_integer(R_SYSCALLres, -1);
	  flush_peepholer();
	  setlab1(ss);
          move_register(R_A1result, R_SYSCALLres);
	}
	break;
case J_CALLK:
        ensure_sp();
        outinstr3(OP_JAL, (Symstr *)m);
        break;
case J_TAILCALLK:
        routine_exit(RE_tailcall);
        if (mips_opt & 1) {
          int32 d = obj_symref((Symstr *)m, xr_code, 0);
          /* On MIPS16 we do not have OP_J, but we do have OP_B to      */
          /* +/-65536 bytes.  Use 65500 to allow for peephole slop.     */
          if ((d == -1 && currentfunction.symstr != (Symstr *)m)
              || (codebase+pcdraft+4*shared_litpool_size-d) > 65500) {
	    external_ref(OP_ADDIU, R_R7eqMR3, 0, (Symstr *)m, OP_NULL);
            outinstrJR(OP_JR, 0, R_R7eqMR3);
            break;
          }
        }
        outinstr3(OP_J, (Symstr *)m);
        break;
case J_CALLR:
/* regalloc.c has ensured mr/LR clash as required by MIPS.              */
        ensure_sp();
        outinstrJR(OP_JALR, R_LR, mr);
        break;
/* this code is tentative: mr cannot be anything restored by routine_exit() */
case J_TAILCALLR:
        move_register(R_TM, mr);
        routine_exit(RE_tailcall);
        /* We had better devise some scheme to ensure that mr is not */
        /* one of the restored registers!                            */
        outinstrJR(OP_JR, 0, R_TM);
        break;
case J_COUNT:
/* (int)r1 is ? (I would like the character on the line) ????              */
/* (char *)r2 is the name of the file, and (int)m is the line number       */
/* within that file. I will assume here a sensible limit on the length     */
/* of files and hence pack these two pieces of information into a single   */
/* 32-bit word. The structure used is count_position, and up to 16 files   */
/* can be referenced. If there is any danger of running out of same I will */
/* flush out the table used to decode files names and start again.         */
        {   count_position k = {0};
            /* beware that the next line may flush literals etc. */
            k.s.posn = 0;   /* Not available here */
            k.s.line = (unsigned int)m;
            k.s.file = lit_of_count_name((char *)r2);
            move_register(R_TM, R_LR);           /* Save: JAL clobbers */
            flush_peepholer();                   /* BSR placement critical */
            ensure_sp();
            outinstr3(OP_JAL, count1routine);
/* @@@ we need a noop here, or one of the above instructions for the MIPS  */
            countdata(0);                        /* what it increments */
            countdata(k.i);
            move_register(R_LR, R_TM);
        }
        break;
#ifdef TARGET_HAS_DEBUGGER
case J_INFOLINE:
        flush_peepholer();
        dbg_addcodep((void *)r1,codebase+codep);  /* hack -- see m88kdbg.c */
        break;
case J_INFOBODY:
        flush_peepholer();
        dbg_bodyproc(/*codebase+codep*/);
        break;
#endif
case J_ADCON:
        if (debugging(DEBUG_DATA))
            cc_msg("ADCON $r+%d\n", (Symstr *)m, (int32)r2);
        external_ref(OP_ADDIU, r1r, (int32)r2, (Symstr *)m, OP_NULL);
        break;
case J_LDRVK:
        external_ref(OP_LW, r1r, (int32)r2, (Symstr *)m, OP_LOAD);
        illbits &= ~(J_SIGNED+J_UNSIGNED);
        break;
case J_LDRBVK:
        external_ref(op&J_SIGNED ? OP_LB : OP_LBU,
                     r1r, (int32)r2, (Symstr *)m, OP_LOAD);
        illbits &= ~(J_SIGNED+J_UNSIGNED);
        break;
case J_LDRWVK:
        external_ref(op&J_SIGNED ? OP_LH : OP_LHU,
                     r1r, (int32)r2, (Symstr *)m, OP_LOAD);
        illbits &= ~(J_SIGNED+J_UNSIGNED);
        break;
case J_LDRFVK:
        external_ref(OP_LWC1, r1r, (int32)r2, (Symstr *)m, OP_LOAD);
        break;
case J_LDRDVK:
        external_ref(OP_LWC1, MSR(r1r), (int32)r2, (Symstr *)m, OP_LOAD);
        external_ref(OP_LWC1, LSR(r1r), (int32)r2 + 4, (Symstr *)m, OP_LOAD);
        break;
case J_STRVK:
        external_ref(OP_SW, r1r, (int32)r2, (Symstr *)m, OP_STORE);
        break;
case J_STRBVK:
        external_ref(OP_SB, r1r, (int32)r2, (Symstr *)m, OP_STORE);
        break;
case J_STRWVK:
        external_ref(OP_SH, r1r, (int32)r2, (Symstr *)m, OP_STORE);
        break;
case J_STRFVK:
        external_ref(OP_SWC1, r1r, (int32)r2, (Symstr *)m, OP_STORE);
        break;
case J_STRDVK:
        external_ref(OP_SWC1, MSR(r1r), (int32)r2, (Symstr *)m, OP_STORE);
        external_ref(OP_SWC1, LSR(r1r), (int32)r2 + 4, (Symstr *)m, OP_STORE);
        break;
case J_STRING:
        syserr("gen.c(J_STRING)");
        outinstr4(OP_ADDIU, r1r, 4*litpoolp,
                       litlab, writes_(r1r, 0, OP_NULL));
        codeseg_stringsegs((StringSegList *)m, 1);
        break;
case J_B:
#ifdef never
        if (!((LabelNumber *)m == RETLAB && (op & Q_MASK) == Q_AL) ||
                !(mips_opt & 1))
            ensure_sp();
#endif
        { int32 q = cmp_undefer_br(op & Q_MASK);
          ensure_sp();
          if (compare_type==1)
            conditional_branch_to_n(q, (LabelNumber *)m);
          else
            conditional_branch_to(q, (LabelNumber *)m);
        }
        illbits &= ~Q_MASK;
        compare_type = 0; cmp_pending = 0;
        break;
case J_SCCK:
        { int32 q = cmp_undefer_scc(op & Q_MASK, m, r1r);
          if (compare_type==1)
            conditional_scc_to_n(q, r1r, m);
          else
            conditional_scc_to(q, r1r, m);
        }
        illbits &= ~Q_MASK;
        compare_type = 0; cmp_pending = 0;
        break;
case J_BXX:             /* used with case tables */
/*
 * the first entry in a case table is the default branch... discard it!
 * this works by assuming that the bxx instructions are issued just after
 * a casebranch, and that after any casebranch there will always be a
 * bxx table.
 */
        if (lose_next_bxx) lose_next_bxx = NO;
        else {
            LabelNumber *dest = (LabelNumber *)m;
            /* the next line could be improved for certain case tables */
            if (dest == RETLAB) dest = returnlab;
            outswent(dest);
        }
        compare_type = 0; cmp_pending = 0;
        break;
case J_CASEBRANCH:
/*
 *     sltiu ip, r1, #m-2
 *     beq   ip, default
 *     sll   r1,r1,3        (swapped with above beq by peepholer)
 *     la    ip,ll          (i.e.  lui ip,hi[ll]; addiu ip,ip,lo[ll];)
 *     addu  ip,r1,ip
*** can we do beter when m<64k, by or-upper instruction, add-lower?
 *     jr    ip
 *     noop
 * ll: ; br default        unnecessary entry in branch table - discarded
 *     br lowest_case         This is rather gross, but without major
 *     noop                   schuduling I cannot do much
 *     br next case
 *     noop
 *     ...
 *
 */
/* n.b. that J_CASEBRANCH can use R_TM as a work register                 */
/* ALso that TARGET_CORRUPTS_SWITCH_REGISTER is exploited...              */
        {   LabelNumber **labtab = (LabelNumber **)r2; /* @@@ nasty hack  */
            LabelNumber *tablab = nextlabel();
            ensure_sp();
            compare_integer(r1r, m-1, Q_HS, -1);      /* m-1 for default  */
            /* default case is given first in the table */
/* MIPS case table entries have a branch+noop (hence shift of 3).         */
/* Moreover the SLL 3 fills the BEQ delay slot after the SLTUI.           */
/* On the MIPS16 we can adjust table sizes later via OUT_SWSHIFT...       */
            outinstrDT(OP_SLL | SHAMT(mips_opt & 2 ? 1L:3L), r1r, r1r, 2);
            conditional_branch_to_n(Q_HS, labtab[0]);
            compare_type = 0; cmp_pending = 0;
            outinstr4(OP_ADDIU, R_TM, 0, tablab,
                        writes_(R_TM, 0, OP_NULL));
            outinstrDST(OP_ADDU, r1r, r1r, R_TM);
#ifdef never
            if (mips_opt_zh(3))
            {   outrelref(OP_LBU, r1r, r1r, 0, OP_LOAD);
                outinstrDST(OP_ADDU, r1r, r1r, R_TM);
            }
#endif
            outinstrJR(OP_JR, 0, r1r);
            Xsetlabel(tablab, 1);
            lose_next_bxx = YES;
        }
        break;
case J_LABEL:
        if (((LabelNumber *)m)->block == DUFF_ADDR)
            fprintf(stderr,"Unused label L%ld\n",
                           lab_name_((LabelNumber *)m));
        Xsetlabel((LabelNumber *)m, 0);
        break;
case J_STACK:
        fp_minus_sp = m;
        adjusted_sp();
        align_sp();
        return;
case J_SETSP:
        {   int32 newstack = m;
            if (fp_minus_sp != (int32)r2)
                syserr("SETSP confused %ld!=%ld %ld",
                       (long)fp_minus_sp, (long)r2, (long)newstack);
            change_sp(fp_minus_sp - newstack);
            fp_minus_sp = newstack;
            align_sp();
        }
        /* MIPS16NCC v1.08 remove negative offsets beyond SP_COVER.     */
        if ((mips_opt & 1) && sp_adjust+SP_OFFSET < SP_COVER) ensure_sp();
        break;

/* load/store                                                              */
case J_LDRBK:
        illbits &= ~(J_SIGNED|J_UNSIGNED);
        outrelref((op & J_SIGNED ? OP_LB:OP_LBU), r1r, r2r, m, OP_LOAD);
        break;
case J_STRBK:
        outrelref(OP_SB, r1r, r2r, m, OP_STORE);
        break;
case J_LDRWK:
        illbits &= ~(J_SIGNED|J_UNSIGNED);
        outrelref((op & J_SIGNED ? OP_LH:OP_LHU), r1r, r2r, m, OP_LOAD);
        break;
case J_STRWK:
        outrelref(OP_SH, r1r, r2r, m, OP_STORE);
        break;
case J_LDRK:
        outrelref(OP_LW, r1r, r2r, m, OP_LOAD);
        break;
case J_STRK:
        outrelref(OP_SW, r1r, r2r, m, OP_STORE);
        break;

/* front end removes for MIPS */
case J_LDRBR:
case J_STRBR:
case J_LDRWR:
case J_STRWR:
case J_LDRR:
case J_STRR:
        syserr("gen.c(rrload/rrstore)");
        break;

/* administration:                                                      */
case J_ENDPROC:
/*
 * At the end of a procedure I interact with the peepholer a bit so I can
 * make a reliable test to see if a return sequence is still needed.
 */
        if (returnlab->lndraft == 0x7ffffffe) {
            /* i.e. !setlab1(returnlab) && reflab(returnlab) */
            fp_minus_sp = 0; adjusted_sp();
            /* FIXME? was Xsetlabel(returnlab, 0); */
            setlab1(returnlab);
            routine_exit(RE_endproc);
        }
        flush_peepholer();
        if (debugging(DEBUG_LOCALCG))
           cc_msg("GEN: pcdraft = %ld\n", (long)pcdraft);
        if (shared_litpool_size > 0 &&
                codebase+pcdraft+4*shared_litpool_size - shared_litpool->codebase >= litpool_codelimit)
        {   Symstr *poolname = shared_litpool->name;
            int i;
            show_entry(poolname, xr_defloc+xr_code);
            for (i = 0; i<shared_litpool_size; i++)
            {   Symstr *sym = shared_litpool->entries[i].sym;
                int32 off = shared_litpool->entries[i].off;
                if (debugging(DEBUG_LOCALCG))
                    cc_msg("lit $r%d before codebase %.8x to pool for %.8x\n",
                       sym, off, codebase, shared_litpool->codebase);
                if (sym)
                    (void) lit_findword(off, LIT_ADCON, sym,
                            LITF_INCODE|LITF_FIRST|LITF_LAST);
                else
                    (void) lit_findwordaux(off, LIT_NUMBER, 0,
                            LITF_INCODE|LITF_FIRST|LITF_LAST);
            }
            shared_litpool_size = 0;
            dumplits2(0);
            show_code(poolname);
            litlab = nextlabel();
        }
        note_shared_lits_as_phase1();
        litlab->lndraft = (pcdraft + 3) & ~3;
        show_entry(currentfunction.symstr, currentfunction.xrflags);
        bb_list = (struct mcpeepdata *)dreverse((List *)bb_list);
        emit_all_peeped();
        /* align to 32 bit word on MIPS16 */
        if (codep & 2) codep += 2;      /* OP16_NOP already there.      */
        if (pcdraft >= litpool_codelimit || mips_opt_zh(2))
          dumplits2(0);
        asm_lablist = (LabList *)dreverse((List *)asm_lablist);
/*
 * to ensure that all literals for a proc get put out. nb on this machine
 * literals are always collected into a single pool which will be dumped
 * out between procedure bodies - i.e. when the machine dependent peepholer
 * is quiescent.
 */
        dump_count_names();
        break;
#ifdef CPLUSPLUS
case J_ORG:
	while (codep < m) outinstrNOOP();
        if (codep != m) syserr("J_ORG");
        break;
#endif
case J_ENTER:
        asm_lablist = 0;
        pcdraft = 0; bb_list = 0; /* lastlit = -0x40; */
        litpool_codelimit = 1024;    /* limit with short LD.W r,n(pc)   */
        phase1_litpool_size = 0;
        routine_entry(m);
        break;

/* there is fun here keeping the stack doubleword aligned.             */
case J_PUSHM:
        {   int32 nregs = 4*bitcount(m);
                {   change_sp(-nregs);
                    save_regs(m, 0, 0, 0, 1);
                    fp_minus_sp += nregs;
                    align_sp();
                }
        }
        break;

default:
        show_fp_inst_direct(op, m, peep, r1r, r2r, mr);
        return;
    }
    if (illbits | peep) syserr(syserr_illegal_jopmode, (long)op, (long)peep);
}

static void show_fp_inst_direct(J_OPCODE op, int32 m, int32 peep,
                RealRegister r1r, RealRegister r2r, RealRegister mr)
{   int32 illbits;
    illbits = op & (Q_MASK | J_SIGNED | J_UNSIGNED);
    switch (op & ~(Q_MASK | J_SIGNED | J_UNSIGNED | J_ALIGNMENT))
    {
/* now the floating point part of the instruction set */
case J_PUSHD:
      change_sp(-8);
      { int32 offset = sp_adjust + SP_OFFSET;
        outinstrF(OP_SWC1 | F_FT(MSR(r1r)) | F_RS(R_SP) | offset&0xffff,
                reads_(MSR(r1r), R_SP, 0, 0), writes_(0, 0, OP_NULL));
        outinstrF(OP_SWC1 | F_FT(LSR(r1r)) | F_RS(R_SP) | (offset+4)&0xffff,
                reads_(LSR(r1r), R_SP, 0, 0), writes_(0, 0, OP_NULL));
      }
      fp_minus_sp += 8;
      break;
case J_PUSHF:
      change_sp(-4);
      { int32 offset = sp_adjust + SP_OFFSET;
        outinstrF(OP_SWC1 | F_FT(r1r) | F_RS(R_SP) | offset&0xffff,
                reads_(r1r, R_SP, 0, 0), writes_(0, 0, OP_NULL));
      }
      fp_minus_sp += 4;
      align_sp();
      break;
case J_MOVFK:
      /* Would be nice to load single & double literals the same way        */
                    /* we should improve the lsword = 0 case.            */
      {
        int32 disp = genfplit((FloatCon *)m, 0);
        outinstr4(OP_ADDIU, R_TM, disp, litlab, writes_(R_TM, 0, OP_NULL));
        outinstrF(OP_LWC1 | F_FT(r1r) | F_RS(R_TM),
		       reads_(R_TM, 0, 0, 0), writes_(r1r, r1r, OP_LOAD));
        break;
      }
case J_MOVDK:
      /* Would be nice to load single & double literals the same way        */
                    /* we should improve the lsword = 0 case.            */
      {
        int32 disp = genfplit((FloatCon *)m, 1);
        outinstr4(OP_ADDIU, R_TM, disp, litlab, writes_(R_TM, 0, OP_NULL));
        outinstrF(OP_LWC1 | F_FT(MSR(r1r)) | F_RS(R_TM),
            reads_(R_TM, 0, 0, 0), writes_(MSR(r1r), MSR(r1r), OP_LOAD));
	outinstrF(OP_LWC1 | F_FT(LSR(r1r)) | F_RS(R_TM) | 4,
	    reads_(R_TM, 0, 0, 0), writes_(LSR(r1r), LSR(r1r), OP_LOAD));
        break;
      }
case J_ADCONF:
case J_ADCOND:
      { int32 disp = genfplit((FloatCon *)m, op==J_ADCOND);
        outinstr4(OP_ADDIU, r1r, disp, litlab, writes_(r1r, 0, OP_NULL));
        break;
      }
case J_MOVFR:
      outinstrF(OP_MOV_S | F_FD(r1r) | F_FS(mr),
                     reads_(mr, 0, 0, 0), writes_(r1r, 0, OP_NULL));
      break;
case J_MOVDR:
      outinstrF(OP_MOV_D | F_FD(r1r) | F_FS(mr),
                     reads_(MSR(mr), LSR(mr), 0, 0), writes_(r1r, 0, OP_NULL));
      break;
case J_NEGFR:
      outinstrF(OP_NEG_S | F_FD(r1r) | F_FS(mr),
                     reads_(mr, 0, 0, 0), writes_(r1r, 0, OP_NULL));
      break;
case J_NEGDR:
      outinstrF(OP_NEG_D | F_FD(r1r) | F_FS(mr),
                     reads_(MSR(mr), LSR(mr), 0, 0), writes_(r1r, 0, OP_NULL));
      break;
case J_FIXFR:    /* C 'fix' is truncate towards zero */
      outinstrF(OP_CFC1 | F_RT(R_TM) | F_FS(63),
		     reads_(63, 0, 0, 0), writes_(R_TM, R_TM, OP_LOAD));
      outinstrI(OP_ORI | 3, r1r, R_TM);
      outinstrI(OP_XORI | 2, r1r, r1r);
      outinstrF(OP_CTC1 | F_FS(63) | F_RT(r1r),
		     reads_(r1r, 0, 0, 0), writes_(63, 63, OP_LOAD));
      outinstrF(OP_CVT_W_S | F_FD(R_FIP) | F_FS(mr),
                     reads_(r1r, 63, 0, 0), writes_(R_FIP, 0, OP_NULL));
      outinstrF(OP_CTC1 | F_FS(63) | F_RT(R_TM),
		     reads_(R_TM, 0, 0, 0), writes_(63, 63, OP_LOAD));
      outinstrF(OP_MFC1 | F_FS(R_FIP) | F_RT(r1r),
                     reads_(R_FIP, 0, 0, 0), writes_(r1r, r1r, OP_LOAD));
      illbits ^= J_SIGNED;   /* only signed version acceptable */
      break;
case J_FIXDR:    /* C 'fix' is truncate towards zero */
      outinstrF(OP_CFC1 | F_RT(R_TM) | F_FS(63),
		     reads_(63, 0, 0, 0), writes_(R_TM, R_TM, OP_LOAD));
      outinstrI(OP_ORI | 3, r1r, R_TM);
      outinstrI(OP_XORI | 2, r1r, r1r);
      outinstrF(OP_CTC1 | F_FS(63) | F_RT(r1r),
		     reads_(r1r, 0, 0, 0), writes_(63, 63, OP_LOAD));
      outinstrF(OP_CVT_W_D | F_FD(R_FIP) | F_FS(mr),
                     reads_(MSR(r1r), LSR(r1r), 63, 0),
		     writes_(R_FIP, 0, OP_NULL));
      outinstrF(OP_CTC1 | F_FS(63) | F_RT(R_TM),
		     reads_(R_TM, 0, 0, 0), writes_(63, 63, OP_LOAD));
      outinstrF(OP_MFC1 | F_FS(R_FIP) | F_RT(r1r),
                     reads_(R_FIP, 0, 0, 0), writes_(r1r, r1r, OP_LOAD));
      illbits ^= J_SIGNED;   /* only signed version acceptable */
      break;
case J_FLTFR:
      outinstrF(OP_MTC1 | F_FS(r1r) | F_RT(mr),
                     reads_(mr, 0, 0, 0), writes_(r1r, r1r, OP_LOAD));
      outinstrF(OP_CVT_S_W | F_FD(r1r) | F_FS(r1r),
                     reads_(r1r, 0, 0, 0), writes_(r1r, 0, OP_NULL));
      illbits ^= J_SIGNED;   /* only signed version acceptable */
      break;
case J_FLTDR:
      outinstrF(OP_MTC1 | F_FS(r1r) | F_RT(mr),
                     reads_(mr, 0, 0, 0), writes_(r1r, r1r, OP_LOAD));
      outinstrF(OP_CVT_D_W | F_FD(r1r) | F_FS(r1r),
                     reads_(r1r, 0, 0, 0), writes_(r1r, 0, OP_NULL));
      illbits ^= J_SIGNED;   /* only signed version acceptable */
      break;
case J_MOVFDR:  /* Widen from single to double */
      outinstrF(OP_CVT_D_S | F_FD(r1r) | F_FS(mr),
                     reads_(mr, 0, 0, 0), writes_(r1r, 0, OP_NULL));
        break;
case J_MOVDFR:  /* Round from double to single precision */
      outinstrF(OP_CVT_S_D | F_FD(r1r) | F_FS(mr),
                     reads_(MSR(mr), LSR(mr), 0, 0), writes_(r1r, 0, OP_NULL));
        break;
case J_MOVIFR:
      outinstrF(OP_MTC1 | F_FS(r1r) | F_RT(mr),
                     reads_(mr, 0, 0, 0), writes_(0, r1r, OP_LOAD));
      break;
case J_MOVIDR:
      outinstrF(OP_MTC1 | F_FS(MSR(r1r)) | F_RT(r2r),
                     reads_(r2r, 0, 0, 0), writes_(0, MSR(r1r), OP_LOAD));
      outinstrF(OP_MTC1 | F_FS(LSR(r1r)) | F_RT(mr),
                     reads_(mr, 0, 0, 0), writes_(LSR(r1r), LSR(r1r), OP_LOAD));
      break;
case J_MOVFIR:
      outinstrF(OP_MFC1 | F_FS(mr) | F_RT(r1r),
                     reads_(mr, 0, 0, 0), writes_(r1r, r1r, OP_LOAD));
      break;
case J_MOVDIR:
      outinstrF(OP_MFC1 | F_FS(MSR(mr)) | F_RT(r1r),
                     reads_(MSR(mr), 0, 0, 0), writes_(r1r, r1r, OP_LOAD));
      outinstrF(OP_MFC1 | F_FS(LSR(mr)) | F_RT(r2r),
                     reads_(LSR(mr), 0, 0, 0), writes_(r2r, r2r, OP_LOAD));
      break;
case J_LDRFK:
      outrelref(OP_LWC1, r1r, r2r, m, OP_LOAD);
      break;
case J_LDRDK:
      outrelref(OP_LWC1, MSR(r1r), r2r, m, OP_LOAD);
      outrelref(OP_LWC1, LSR(r1r), r2r, m+4, OP_LOAD);
      break;
case J_STRFK:
      outrelref(OP_SWC1, r1r, r2r, m, OP_STORE);
      break;
case J_STRDK:
      outrelref(OP_SWC1, MSR(r1r), r2r, m, OP_STORE);
      outrelref(OP_SWC1, LSR(r1r), r2r, m+4, OP_STORE);
      break;
case J_LDRFR:
      if (mr==0)
        outinstrF(OP_LWC1 | F_FT(r1r) | F_RS(r2r),
                       reads_(r2r, 0, 0, 0), writes_(r1r, r1r, OP_LOAD));
      else if (r2r==0)
        outinstrF(OP_LWC1 | F_FT(r1r) | F_RS(mr),
                       reads_(mr, 0, 0, 0), writes_(r1r, r1r, OP_LOAD));
      else {
        outinstrDST(OP_ADDU, R_TM, r2r, mr);
        outinstrF(OP_LWC1 | F_FT(r1r) | F_RS(R_TM),
                       reads_(R_TM, 0, 0, 0), writes_(r1r, r1r, OP_LOAD));
      }
      break;
case J_LDRDR:
      if (mr==0) {
        outinstrF(OP_LWC1 | F_FT(MSR(r1r)) | F_RS(r2r),
                       reads_(r2r, 0, 0, 0), writes_(MSR(r1r), MSR(r1r), OP_LOAD));
        outinstrF(OP_LWC1 | F_FT(LSR(r1r)) | F_RS(r2r) | 4,
                       reads_(r2r, 0, 0, 0), writes_(LSR(r1r), LSR(r1r), OP_LOAD));
      }
      else if (r2r==0) {
        outinstrF(OP_LWC1 | F_FT(MSR(r1r)) | F_RS(mr),
                       reads_(mr, 0, 0, 0), writes_(MSR(r1r), MSR(r1r), OP_LOAD));
        outinstrF(OP_LWC1 | F_FT(LSR(r1r)) | F_RS(mr) | 4,
                       reads_(mr, 0, 0, 0), writes_(LSR(r1r), LSR(r1r), OP_LOAD));
      }
      else {
        outinstrDST(OP_ADDU, R_TM, r2r, mr);
        outinstrF(OP_LWC1 | F_FT(MSR(r1r)) | F_RS(R_TM),
                       reads_(R_TM, 0, 0, 0), writes_(MSR(r1r), MSR(r1r), OP_LOAD));
        outinstrF(OP_LWC1 | F_FT(LSR(r1r)) | F_RS(R_TM) | 4,
                       reads_(R_TM, 0, 0, 0), writes_(LSR(r1r), LSR(r1r), OP_LOAD));
      }
      break;
case J_STRFR:
        if (mr==0)
          outinstrF(OP_SWC1 | F_FT(r1r) | F_RS(r2r),
                         reads_(r2r, r1r, 0, 0), writes_(0, 0, OP_STORE));
        else if (r2r==0)
          outinstrF(OP_SWC1 | F_FT(r1r) | F_RS(mr),
                         reads_(mr, r1r, 0, 0), writes_(0, 0, OP_STORE));
        else {
          outinstrDST(OP_ADDU, R_TM, r2r, mr);
          outinstrF(OP_SWC1 | F_FT(r1r) | F_RS(R_TM),
                         reads_(R_TM, r1r, 0, 0), writes_(0, 0, OP_STORE));
        }
        break;
case J_STRDR:
        if (mr==0) {
          outinstrF(OP_SWC1 | F_FT(MSR(r1r)) | F_RS(r2r),
                         reads_(r2r, MSR(r1r), 0, 0), writes_(0, 0, OP_STORE));
          outinstrF(OP_SWC1 | F_FT(LSR(r1r)) | F_RS(r2r) | 4,
                         reads_(r2r, LSR(r1r), 0, 0), writes_(0, 0, OP_STORE));
        }
        else if (r2r==0) {
          outinstrF(OP_SWC1 | F_FT(MSR(r1r)) | F_RS(mr),
                         reads_(mr, MSR(r1r), 0, 0), writes_(0, 0, OP_STORE));
          outinstrF(OP_SWC1 | F_FT(LSR(r1r)) | F_RS(mr) | 4,
                         reads_(mr, LSR(r1r), 0, 0), writes_(0, 0, OP_STORE));
        }
        else {
          outinstrDST(OP_ADDU, R_TM, r2r, mr);
          outinstrF(OP_SWC1 | F_FT(MSR(r1r)) | F_RS(R_TM),
                         reads_(R_TM, MSR(r1r), 0, 0), writes_(0, 0, OP_STORE));
          outinstrF(OP_SWC1 | F_FT(LSR(r1r)) | F_RS(R_TM) | 4,
                         reads_(R_TM, LSR(r1r), 0, 0), writes_(0, 0, OP_STORE));
        }
        break;
case J_CMPFR:
        compare_fregister(r2r, mr, op&Q_MASK, 0);
/* note that fp and integer comparisons are coded using the same bits.   */
        illbits &= ~Q_MASK;
        break;
case J_CMPDR:
        compare_fregister(r2r, mr, op&Q_MASK, 1<<21);
/* note that fp and integer comparisons are coded using the same bits.   */
        illbits &= ~Q_MASK;
        break;
case J_ADDFR:
      outinstrF(OP_ADD_S | F_FD(r1r) | F_FS(r2r) | F_FT(mr),
                      reads_(r2r, mr, 0, 0), writes_(r1r, 0, OP_NULL));
      break;
case J_ADDDR:
      outinstrF(OP_ADD_D | F_FD(r1r) | F_FS(r2r) | F_FT(mr),
                      reads_(MSR(r2r), MSR(mr), LSR(r2r), LSR(mr)),
                      writes_(r1r, 0, OP_NULL));
      break;
case J_SUBFR:
      outinstrF(OP_SUB_S | F_FD(r1r) | F_FS(r2r) | F_FT(mr),
                      reads_(r2r, mr, 0, 0), writes_(r1r, 0, OP_NULL));
      break;
case J_SUBDR:
      outinstrF(OP_SUB_D | F_FD(r1r) | F_FS(r2r) | F_FT(mr),
                      reads_(MSR(r2r), MSR(mr), LSR(r2r), LSR(mr)),
                      writes_(r1r, 0, OP_NULL));
      break;
case J_MULFR:
      outinstrF(OP_MUL_S | F_FD(r1r) | F_FS(r2r) | F_FT(mr),
                      reads_(r2r, mr, 0, 0), writes_(r1r, 0, OP_NULL));
      break;
case J_MULDR:
      outinstrF(OP_MUL_D | F_FD(r1r) | F_FS(r2r) | F_FT(mr),
                      reads_(MSR(r2r), MSR(mr), LSR(r2r), LSR(mr)),
                      writes_(r1r, 0, OP_NULL));
      break;
case J_DIVFR:
      outinstrF(OP_DIV_S | F_FD(r1r) | F_FS(r2r) | F_FT(mr),
                      reads_(r2r, mr, 0, 0), writes_(r1r, 0, OP_NULL));
      break;
case J_DIVDR:
      outinstrF(OP_DIV_D | F_FD(r1r) | F_FS(r2r) | F_FT(mr),
                      reads_(MSR(r2r), MSR(mr), LSR(r2r), LSR(mr)),
                      writes_(r1r, 0, OP_NULL));
      break;

default:
        syserr(syserr_show_inst_dir, (long)op);
        illbits = 0;
        break;
    }
    if (illbits | peep) syserr(syserr_illegal_jopmode, (long)op, (long)peep);
}

/* Exported routines...                                               */

/* The peepholer: */
void show_instruction(J_OPCODE op,VRegInt vr1,VRegInt vr2,VRegInt vm)
{   /* it may be better to arrange for two switches - one on 'op' and    */
    /* one on pending.op                                                 */
    RealRegister r1 = vr1.r, r2 = vr2.r;
    int32 m = vm.i;
    int32 xtra = 0;                   /* essentially extra bits for 'op' */
    if (debugging(DEBUG_LOCALCG))
    {   cc_msg("GEN: ");
        jopprint_opname(op);
        cc_msg("%ld%s %ld%s %ld%s\n", (long)r1, op&J_DEAD_R1?"#":"",
                                      (long)r2, op&J_DEAD_R2?"#":"",
                                      (long)m,  op&J_DEAD_R3?"#":"");
    }
#ifdef J_BASEALIGN4
    op &= ~J_BASEALIGN4;
#endif
/* to enable future JOPCODE peephole optimisation expand_jop_macros()
   tries quite hard not to call show_inst() with instructions with
   no effect.
*/
    switch (pending.op & ~(J_SIGNED|J_UNSIGNED|J_DEADBITS|J_ALIGNMENT))
    {
case J_NOOP:
        goto skip;
case J_ADCON:
        if (mips_opt & 1) break;
        switch (op & ~(J_SIGNED|J_UNSIGNED|J_DEADBITS|J_ALIGNMENT))
        {
    case J_LDRK:  case J_STRK:
    case J_LDRBK: case J_STRBK:
    case J_LDRWK: case J_STRWK:
    case J_LDRFK: case J_STRFK:
    case J_LDRDK: case J_STRDK:
            if (!(op & J_DEAD_R2) ||
                pending.r1 != r2) break;
/*
 * This is a bit of abuse - in mip LDRVK etc are used for stack-relative
 * references - here I re-use the jopcodes (on a purely local basis) for
 * references relative to external symbols.  This helps me cope with the
 * MIPS idiom
 *      lui rx,r0,hi16
 *      lw  ry,rx,lo16(xxx)
 * If somebody wants to allocate a new set of jopcodes (LDRXK, say) where the
 * X stands for external) I would be quite happy.
 */
            pending.op = J_addvk(op & ~J_DEADBITS);
            pending.r1 = r1;
            pending.r2 += m;
            return;
    default:
            break;
        }
        break;
case J_SETSP:
        if ((op & ~J_DEADBITS) == J_SETSP && pending.m == (int32)r2)
        {   /* squash adjacent SETSP's */
            pending.m = m;
            return;
        }
        break;
case J_ADDK:
        if (!(mips_opt & 1)) break;
/* For MIPS16, target.h says that addressing is limited to 5 bits.      */
/* Hence bigger offsets have first an ADDK and then a LDRxK/STKxK.      */
/* This enables CSE to common up uses of ADDK (benchmarks better).      */
/* However if the ADDK result is only used once we should just use      */
/* an EXT opcode instead of the ADDK.                                   */
        switch (op & ~(J_SIGNED|J_UNSIGNED|J_DEADBITS|J_ALIGNMENT))
        {
    case J_STRK: case J_STRBK: case J_STRWK:
            if (pending.r1 == pending.r2) break;        /* be safe      */
    case J_LDRK: case J_LDRBK: case J_LDRWK:
            if (!(op & J_DEAD_R2) || pending.r1 != r2) break;
            if (m+pending.m != s16_(m+pending.m)) break;
            pending.op = op;
            pending.r1 = r1;
            /* pending.r2 = pending.r2; */
            pending.m = m + pending.m;
            return;
        }
        break;
case J_ANDK:
/* This is a bit of a hack to fixup (char)(x&257) which has been mapped */
/* to (x&1)&255 but we now need to lose the &255.  It's a hack because  */
/* cse can remove the &255, but only after the 257 has been turned      */
/* into 1, which happens in regalloc.c.  Sigh.                          */
        if ((op & ~J_DEADBITS) == J_ANDK
                         && (op & J_DEAD_R2) && pending.r1 == r2
                         && (pending.m & ~m) == 0)
        {   pending.r1 = r1;
            return;
        }
        break;
case J_LDRK:
/* Map LDRK; SHRK into LDBRK; SHRK etc.                                 */
/* Similarly we could map LDRK; ANDK into LDRB etc.                     */
        if ((op & ~(J_SIGNED|J_UNSIGNED|J_DEADBITS)) == J_SHRK
                         && (op & J_DEAD_R2) && pending.r1 == r2
                         && (pending.m & 3) == 0)
        {   if (m>=24)
            {   pending.op = pending.op & ~(J_LDRK|J_SIGNED|J_UNSIGNED)
                                 | (J_LDRBK | (op & (J_SIGNED|J_UNSIGNED)));
                m -= 24;
                if (target_lsbytefirst) pending.m += 3;
                if (m == 0) goto done;
            }
            else if (m>=16)
            {   pending.op = pending.op & ~(J_LDRK|J_SIGNED|J_UNSIGNED)
                                 | (J_LDRWK | (op & (J_SIGNED|J_UNSIGNED)));
                m -= 16;
                if (target_lsbytefirst) pending.m += 2;
                if (m == 0) goto done;
            }
        }
        break;
default:
        break;
    }
    show_inst_direct(pending.op & ~J_DEADBITS, pending.r1, pending.r2,
                     pending.m, pending.xtra);
skip:
    pending.op = op, pending.r1 = r1, pending.r2 = r2,
                     pending.m = m, pending.xtra = xtra;
done:
    switch (op & ~J_DEADBITS)
    {   case J_ENTER: case J_ENDPROC: case J_STACK: case J_LABEL:
        case J_INFOLINE: case J_INFOBODY:
            /* flush these cases immediately for local_address()  */
            show_inst_direct(pending.op & ~J_DEADBITS, pending.r1, pending.r2,
                             pending.m, pending.xtra);
            pending.op = J_NOOP; pending.xtra = 0;
            compare_type = 0; cmp_pending = 0;
            break;
    }
}

/* the next routine is required for the machine independent codebuf.c */
void branch_round_literals(LabelNumber *m)
{
    show_inst_direct(J_B, GAP, GAP, (int32)m, 0);
}

void mcdep_init()
{
    shared_litpool = 0; shared_litpool_size = 0;
    phase1_litpool = 0; phase1_litpool_size = 0;
    wasted_in_save = 0; wasted_in_restore = 0;
    pending.op = J_NOOP; pending.xtra = 0;
    compare_type = 0; cmp_pending = 0;
    lose_next_bxx = NO;
    /* The next line is a bit overenthusiastic, but it doesn't hurt    */
    /* on the MIPS R3000 where there are lots of regs, and it is       */
    /* controlled on the MIPS16.                                       */
    avoidallocating(R_LR); avoidallocating(R_IP);
    if (mips_opt & 1 && mips_opt_zh(9))
        avoidallocating(R_R6eqMR2);
    init_peepholer();
    obj_symref(targeterrno, 0, 0);
}

/* vvvvvvvvvvvvvvvvvvvvvv    PEEPHOLER    vvvvvvvvvvvvvvvvvvvv */

enum { OUT_NULL, OUT_COUNT, OUT_INSTR,
       OUT_CODELABHI, OUT_CODELABLO,
       OUT_EXTLABHI, OUT_EXTLABLO,
       OUT_EXTCALL,
       OUT_EXTLIT,                       /* MIPS16 literals  */
       OUT_SWSHIFT, OUT_SWENT, OUT_SWTAB,          /* for J_CASEBRANCH */
       OUT_LABREF,
       OUT_LABDEF,
       OUT_LTORG,
       /* hacks... */
       OUT_ALIGN
     };

struct mcpeepdata {
    struct mcpeepdata *next;
    int32 type;         /* e.g. OUT_INSTR */
    int32 w;            /* 32 bits of data */
    int32 reads;        /* registers accessed */
    int32 writes;       /* registers clobbered, plus usage class */
    Symstr *s;          /* external symbol (if needed) */
    unsigned32 off;     /* offset (if needed) */
    LabelNumber *lab;   /* if needed */
    int32 reftype;      /* label reference type */
    int32 outdraft;     /* maximum codep for placement */
};

/* old stuff... */
static mcpeepdata mc_peep;
static int32 mc_delay = 0; /* Forbidden registers for next instruction */

/* ^^^^^^^^^^^^^^^^^^^^^^    PEEPHOLER    ^^^^^^^^^^^^^^^^^^^^ */

static void emit_one_instr(int32 w)
{   mcpeepdata *mc_peep1 = new_mcpeep();
    mc_peep1->type = OUT_INSTR;
    mc_peep1->w = w;
    emit_one_peeped(mc_peep1);
}

static void countdata(int32 w)
{
    flush_peepholer();
    mc_peep.type = OUT_COUNT;
    mc_peep.w = w;
}

static bool touches(int32 a, int32 b)
{
    if (a == 0) return NO;
    else if (a == reg1_(b) || a == reg2_(b) ||
             a == reg3_(b) || a == reg4_(b)) return YES;
    else return NO;
}

static bool intersects(int32 a, int32 b)
/*
 * a is 4 (read) registers, b is 2 (write) registers.
 */
{
    if (touches(reg1_(b), a) ||
        touches(reg2_(b), a))
      return YES;
    else return NO;
}

static void delayed_load_store(int32 reads)
{
  if (touches(mc_delay,reads)) {
    if (mips_opt & 2)
        ;                       /* MIPS16 is properly interlocked.      */
    else
        emit_one_instr(OP_NOOP);
    mc_delay = 0;
  }
}

static bool swappable(int32 readsa, int32 writesa, int32 readsb, int32 writesb)
/*
 * At present two instructions come in the order (a;b) - see if they can be
 * flipped into order (b;a).
 */
{
    if (intersects(readsa, writesb) ||
        intersects(readsb, writesa) ||
/*
 * The (writesa & 0xffff0000) is a bit of a pun - it maps a pair of
 * written regs plus a type mask onto the format of 4 read registers.
 */
        intersects(writesa & 0xffff0000, writesb)) return NO;
    else return YES;
}

static void outinstr0(int32 w, int32 reads, int32 writes)
{
    /* ordinary instruction */
    flush_peepholer();
    mc_peep.type = OUT_INSTR;
    mc_peep.w = w;
    mc_peep.reads = reads;
    mc_peep.writes = writes;
}

static void outinstr0x(int32 w, int32 reads, int32 writes)
{
    /* OUT_SWSHIFT instruction, adjusted later */
    flush_peepholer();
    mc_peep.type = OUT_SWSHIFT;
    mc_peep.w = w;
    mc_peep.reads = reads;
    mc_peep.writes = writes;
}

static void outswtab(LabelNumber **tab, int32 n)
{
    mcpeepdata *mc_peep1 = new_mcpeep();
    mc_peep1->type = OUT_SWTAB;
    mc_peep1->w = n;
    mc_peep1->lab = (LabelNumber *)tab;
    emit_one_peeped(mc_peep1);
}

static void outswent(LabelNumber *dest)
{
    mcpeepdata *mc_peep1 = new_mcpeep();
    mc_peep1->type = OUT_SWENT;
    mc_peep1->w = (mips_opt & 2) ?  OP16_B : OP_BEQ;
    mc_peep1->reftype = (mips_opt & 2) ? LABREF_M16_S11 : LABREF_OFF16;
    mc_peep1->lab = dest;
    emit_one_peeped(mc_peep1);
}

static void outltorg()
{
    mcpeepdata *mc_peep1 = new_mcpeep();
    mc_peep1->type = OUT_LTORG;
    emit_one_peeped(mc_peep1);
}

static void outinstr1(int32 ww, int32 reads, int32 writes)
{
    /* OP_JUMP instruction */
    bool noop_needed = YES;
    if (mc_peep.type != OUT_NULL &&
        swappable(mc_peep.reads, mc_peep.writes, reads, writes) &&
/* On MIPS16 we can't put a PREfix instruction in the jmp delay slot.   */
        ((mips_opt & 2) ? ((mc_peep.w>>16) == 0) :
         (mips_opt & 1) ? 1 :
/* On MIPS R3000 never place loads in delay slots as access at the      */
/* destination could kill.                                              */
                          !(mc_peep.writes & OP_LOAD)))
      noop_needed = NO;
    else flush_peepholer();
    delayed_load_store(reads);
    if (mips_opt & 2) {
      if (noop_needed) { ww ^= (OP16_JR^OP16_JRC), noop_needed = 0; }
    }
    emit_one_instr(ww);
    mc_delay = reg2_(writes);
    flush_peepholer();
    if (noop_needed) {
      outinstrNOOP();
      mc_delay = 0;
      flush_peepholer();
    }
}

/* outinstr2() does a branch to a (set or unset) label.                 */
static void outinstr2(int32 w, int32 reads, int32 writes,
       LabelNumber *lab, int32 t)
{
    bool noop_needed = YES;
    if (mips_opt & 2) {
      flush_peepholer();
      noop_needed = NO;
    }
    else if (mc_peep.type != OUT_NULL &&
/* Be cowardly and never place loads in delay slots as access at the    */
/* destination could kill.                                              */
          !(mc_peep.writes & OP_LOAD) &&
        swappable(mc_peep.reads, mc_peep.writes, reads, writes))
      noop_needed = NO;
    else
      flush_peepholer();
    delayed_load_store(reads);

    {   mcpeepdata *mc_peep1 = new_mcpeep();
        mc_peep1->type = OUT_LABREF;
        mc_peep1->w = w;
        mc_peep1->lab = lab;
        mc_peep1->reftype = t;
        mc_peep1->off = 0;
        emit_one_peeped(mc_peep1);
    }

    mc_delay = reg2_(writes);
    flush_peepholer();
    if (noop_needed) {
      outinstrNOOP();
      mc_delay = 0;
      flush_peepholer();
    }
}

/* outinstr3() does a JAL, J or JALX to an external symbol.             */
static void outinstr3(int32 w, Symstr *name)
{
    int32 d;
    int32 reads = reads_(0, 0, 0, 0);
    int32 writes = writes_((w == OP_JAL ? (int32)R_LR : 0), 0, OP_BR26);
    bool noop_needed = YES;
    if (mc_peep.type != OUT_NULL &&
        swappable(mc_peep.reads, mc_peep.writes, reads, writes) &&
/* On MIPS16 we can't put a PREfix instruction in the jmp delay slot.   */
        ((mips_opt & 2) ? ((mc_peep.w>>16) == 0) :
         (mips_opt & 1) ? 1 :
/* On MIPS R3000 never place loads in delay slots as access at the      */
/* destination could kill.                                              */
                          !(mc_peep.writes & OP_LOAD)))
      noop_needed = NO;
    else flush_peepholer();
    delayed_load_store(reads);
    d = obj_symref(name, xr_code, 0);
/* since J and JAL are absolute, we have to ask the linker to do the    */
/* the relocation instead of exploiting backward (d != -1) branches.    */

    if (mips_opt & 2) {
        if (w==OP_J)
        {   /* Caller (J_TAILCALL) has ensured in range of OP16_B.      */
            w = OP16_B;
            noop_needed = NO;
        }
        else
            w = (w==OP_JAL ? OP16_JAL<<16 : OP16_JALX<<16);
    }

    {   mcpeepdata *mc_peep1 = new_mcpeep();
        mc_peep1->type = OUT_EXTCALL;
        mc_peep1->w = w;
        mc_peep1->s = name;
        emit_one_peeped(mc_peep1);
    }

    mc_delay = reg2_(writes);
    flush_peepholer();
    if (noop_needed) {
      outinstrNOOP();
      mc_delay = 0;
      flush_peepholer();
    }
}

static void outinstr4(int32 op, RealRegister rt, unsigned32 off,
                      LabelNumber *lab, int32 writes)
{
  int32 ww;
  int32 reftype;
  int32 postmov = 0;
  RealRegister rd = rt;
  if (mips_opt & 1)
  {
    flush_peepholer();
    if (!(mips_opt & 2))
    { ww = (op==OP_LW ? OP_pseudoLW_PC:OP_pseudoADDIU_PC) | F_RT(rt) | (off>>2);
      reftype = LABREF_OFF16;
    }
    else
    { int32 rx = mr16[v2p(rt)];
      if (rx < 0)
      {   rt = R_R7eqMR3; rx = v2p(rt);
          postmov = OP16_MOV32R | v2p(rd) & 24 | (v2p(rd) & 7) << 5 | rx;
      }
      ww = foo8u(off, (op==OP_LW ? OP16_LW_PC : OP16_ADDIU_PC), 2, rx);
      reftype = (ww >> 16) ? LABREF_M16_S16X : LABREF_M16_U8;
    }
    mc_peep.type = OUT_LABREF;
    mc_peep.reftype = reftype;
    mc_peep.w = ww;
    mc_peep.s = 0;      /* not nec */
    mc_peep.off = 0;
    mc_peep.lab = lab;
    mc_peep.reads = reads_(0, 0, 0, 0);
    mc_peep.writes = writes;
    flush_peepholer();  /* not needed? -- worsens ADDIU_PC in delay slot? */
  }
  else
  {
    flush_peepholer();
    mc_peep.type = OUT_CODELABHI;
    mc_peep.w = OP_LUI | F_RT(rt);
    mc_peep.s = bindsym_(codesegment);
    mc_peep.off = off;
    mc_peep.lab = lab;
    mc_peep.reads = reads_(0, 0, 0, 0);
    mc_peep.writes = writes_(rt, 0, OP_NULL);

    flush_peepholer();
    mc_peep.type = OUT_CODELABLO;
    mc_peep.w = op | F_RT(rt) | F_RS(rt);
    mc_peep.s = bindsym_(codesegment);
    mc_peep.off = off;
    mc_peep.lab = lab;
    mc_peep.reads = reads_(rt, 0, 0, 0);
    mc_peep.writes = writes_(rt, 0, OP_NULL) | writes;
    flush_peepholer();
  }
  if (postmov)
    outinstr0(postmov, reads_(rt, 0, 0, 0), writes_(rd, 0, OP_NULL));
}

/* outinstr4a() is only used if (mips_opt & 1), i.e. mips16.            */
static void outinstr4a(int32 op, RealRegister rt, Symstr *name, int32 off,
                       int32 writes)
{
  int32 ww;
  int32 postmov = 0;
  RealRegister rd = rt;
  {
    flush_peepholer();
    if (!(mips_opt & 2))
    { ww = (op==OP_LW ? OP_pseudoLW_PC:OP_pseudoADDIU_PC) | F_RT(rt);
    }
    else
    { int32 rx = mr16[v2p(rt)];
      if (rx < 0)
      {   rt = R_R7eqMR3; rx = v2p(rt);
          postmov = OP16_MOV32R | v2p(rd) & 24 | (v2p(rd) & 7) << 5 | rx;
      }
      ww = foo8u(0, (op==OP_LW ? OP16_LW_PC : OP16_ADDIU_PC), 2, rx);
    }
    mc_peep.type = OUT_EXTLIT;
    mc_peep.w = ww;
    mc_peep.s = name;
    mc_peep.off = off;
    note_phase1_lit(name, off, 1);
    mc_peep.lab = 0;    /* not nec */
    mc_peep.reads = reads_(0, 0, 0, 0);
    mc_peep.writes = writes;
    flush_peepholer();  /* not needed? -- worsens ADDIU_PC in delay slot? */
  }
  if (postmov)
    outinstr0(postmov, reads_(rt, 0, 0, 0), writes_(rd, 0, OP_NULL));
}

static void external_ref(int32 op, RealRegister r1r, unsigned32 off,
                  Symstr *name, int32 opclass)
{
  if (mips_opt & 1)
  {   int32 disp;
      if (op != OP_ADDIU || opclass != OP_NULL)
          syserr("mips(external_ref)");
      outinstr4a(OP_LW, r1r, name, off, writes_(r1r, r1r, OP_LOAD));
  }
  else
  {
    int32 r1 = 0, r2 = 0, w1 = 0;
    flush_peepholer();
    mc_peep.type = OUT_EXTLABHI;
    mc_peep.w = OP_LUI | F_RT(R_TM);
    mc_peep.s = name;
    mc_peep.off = off;
    mc_peep.reads = reads_(0, 0, 0, 0);
    mc_peep.writes = writes_(R_TM, 0, OP_NULL);

    switch (opclass & (OP_STORE|OP_2WORD))
    {
case 0:
        w1 = r1r; break;
case OP_2WORD:
        w1 = r1r; break;             /* /* should be w1 = r1r+1? */
case OP_STORE:
        r1 = r1r; break;
case OP_STORE|OP_2WORD:
        r1 = r1r; r2 = r1r+1; break;
    }
    flush_peepholer();
    mc_peep.type = OUT_EXTLABLO;
    mc_peep.w = op | F_RT(r1r) | F_RS(R_TM);
    mc_peep.s = name;
    mc_peep.off = off;
    mc_peep.reads = reads_(R_TM, r1, r2, 0);
    mc_peep.writes = writes_(w1, (opclass&OP_LOAD?w1:0), opclass);
    flush_peepholer();
  }
}

/* Conditional branch based on register compare */
static void conditional_branch_to(int32 condition, LabelNumber *dest)
{
    if (dest == RETLAB)
    {   dest = returnlab;
        if (condition == Q_AL)
        {   routine_exit(RE_return);
            /* outltorg(); */
            return;
        }
    }

    switch (condition) {
      default:
        syserr("cond_br2 %lx", (long)condition);
        return;
      case Q_AL:
        outinstrB(OP_BEQ, 0, 0, dest);
        /* outltorg(); */
        break;

      case Q_NE: case Q_UNE:
        if (!(mips_opt & 1)) goto fltcase2;
      case Q_HI:
      case Q_GT:
      case Q_LO:
      case Q_LT:
        if (compare_type==-1) goto fltcase2;
        outinstrB(OP_BNE, 0, R_T24, dest);
        break;

      case Q_EQ: case Q_UEQ:
        if (!(mips_opt & 1)) goto fltcase2;
      case Q_GE:
      case Q_LE:
      case Q_HS:
      case Q_LS:
        if (compare_type==-1) goto fltcase2;
        outinstrB(OP_BEQ, 0, R_T24, dest);
        break;

      fltcase2:
        outinstr2(compare_op, compare_r, compare_w, dest, LABREF_OFF16);
        break;
    }
}

static int32 br_of_Q(int32 condition)
{   switch (condition) {
 default:   syserr("br_of_Q %lx", (long)condition); return 0;
 case Q_EQ: case Q_UEQ: return OP_BEQ;
 case Q_NE: case Q_UNE: return OP_BNE;
 case Q_LT: case Q_LO: return OP_BLTZ;
 case Q_LE: case Q_LS: return OP_BLEZ;
 case Q_GT: case Q_HI: return OP_BGTZ;
 case Q_GE: case Q_HS: return OP_BGEZ;
        }
}

/* Conditional branch based on constant compare */
static void conditional_branch_to_n(int32 condition, LabelNumber *dest)
{
    if (condition == Q_NOT) return;

    if (dest == RETLAB)
    {   dest = returnlab;
        if (condition == Q_AL)
        {   routine_exit(RE_return);
            /* outltorg(); */
            return;
        }
    }

    switch (condition) {
        case Q_LS: case Q_HI:      /* now dealt with elsewhere */
        case Q_LE: case Q_GT:
        default:
          syserr("cond_br_n3 %lx", (long)condition);
          return;
        case Q_AL:
          outinstrB(OP_BEQ, 0, 0, dest);
          /* outltorg(); */
          break;
        case Q_EQ: case Q_UEQ:
          if (compare_no == 0) goto cmpz; /* always if (mips_opt & 1) */
          outinstrB(OP_BEQ, compare_reg, R_TM, dest);
          break;
        case Q_NE: case Q_UNE:
          if (compare_no == 0) goto cmpz; /* always if (mips_opt & 1) */
          outinstrB(OP_BNE, compare_reg, R_TM, dest);
          break;
        case Q_LO:
        case Q_LT:
          if (compare_no == 0 && !(mips_opt & 1)) goto cmpz;
          outinstrB(OP_BNE, R_T24, 0, dest);
          break;
        case Q_GE:
        case Q_HS:
          if (compare_no == 0 && !(mips_opt & 1)) goto cmpz;
          outinstrB(OP_BEQ, R_T24, 0, dest);
          break;
        cmpz:
          outinstrB(br_of_Q(condition), compare_reg, 0, dest);
    }
}

static void map0Xto0n(RealRegister r, int32 n, RealRegister rs)
{   LabelNumber *skip = nextlabel();
    move_register(r, rs);
    outinstrB(OP_BEQ, rs, 0, skip);
    load_integer_over_zeroreg(r, n);
    flush_peepholer();
    setlab1(skip);
}

static void map01to0n(RealRegister r, int32 n)
{         int32 logn;
          if ((logn = power_of_two(n)) != -1)
              outinstrDT(OP_SLL | SHAMT(logn), r, R_T24, 0);
          else if ((logn = power_of_two(-n)) != -1)
          {   negate_register(r, R_T24);
              outinstrDT(OP_SLL | SHAMT(logn), r, r, 0);
          }
          else if ((logn = power_of_two(n+1)) != -1)
          {   negate_register(r, R_T24);
              if (n == 0xff || n == 0xffff)
                  and_integer(r, r, n);
              else
                  outinstrDT(OP_SRL | SHAMT(32-logn), r, r, 0);
          }
          else
              map0Xto0n(r, n, R_T24);
}

static void mapX0to0n(RealRegister r, int32 n, RealRegister rs)
{
  if (n == 1)
  { outinstrI(OP_SLTIU | 1, R_T24, rs);
    move_register(r, R_T24);
  }
  else
  { LabelNumber *skip = nextlabel();
    if (r == rs) { move_register(R_T24, rs); rs = R_T24; }
    load_integer(r, 0);
    outinstrB(OP_BNE, rs, 0, skip);
    load_integer_over_zeroreg(r, n);
    flush_peepholer();
    setlab1(skip);
  }
}

static void map10to0n(RealRegister r, int32 n)
{         int32 logn;
          if ((logn = power_of_two(n)) != -1)
          {   negate_register(r, R_T24);
              add_integer(r, r, 1);
              outinstrDT(OP_SLL | SHAMT(logn), r, r, 0);
          }
          else if ((logn = power_of_two(-n)) != -1)
          {   add_integer(r, R_T24, -1);
              outinstrDT(OP_SLL | SHAMT(logn), r, r, 0);
          }
          else if ((logn = power_of_two(n+1)) != -1)
          {   add_integer(r, R_T24, -1);
              if (n == 0xff || n == 0xffff)
                  and_integer(r, r, n);
              else
                  outinstrDT(OP_SRL | SHAMT(32-logn), r, r, 0);
          }
          else
              mapX0to0n(r, n, R_T24);
}

/* Set register to 0 or n based on register compare */
static void conditional_scc_to(int32 condition, RealRegister r, int32 n)
{
    switch (condition) {
      default:
        syserr("cond_scc2 %lx", (long)condition);
        return;
      case Q_AL:
        load_integer(r, n);
        break;

      case Q_NE: case Q_UNE:
        if (!(mips_opt & 1) || compare_type==-1) goto fltcase2;
        map0Xto0n(r, n, R_T24);
        break;
      case Q_HI:
      case Q_GT:
      case Q_LO:
      case Q_LT:
        if (compare_type==-1) goto fltcase2;
        map01to0n(r, n);
        break;

      case Q_EQ: case Q_UEQ:
        if (!(mips_opt & 1) || compare_type==-1) goto fltcase2;
        mapX0to0n(r, n, R_T24);
        break;
      case Q_GE:
      case Q_LE:
      case Q_HS:
      case Q_LS:
        if (compare_type==-1) goto fltcase2;
        map10to0n(r, n);
        break;

      fltcase2:
        syserr("conditional_scc_to(unfinished)");
        break;
    }
}

/* Set register to 0 or n based on constant compare */
static void conditional_scc_to_n(int32 condition, RealRegister r, int32 n)
{
    switch (condition) {
        case Q_LS: case Q_HI:      /* now dealt with elsewhere */
        case Q_LE: case Q_GT:
        default:
          syserr("cond_scc_n3 %lx", (long)condition);
          return;
        case Q_NOT:
          load_integer(r, 0);
          break;
        case Q_AL:
          load_integer(r, n);
          break;
        case Q_EQ: case Q_UEQ:
          if (compare_no != 0) goto cmpz2;
          mapX0to0n(r, n, compare_reg);
          break;
        case Q_NE: case Q_UNE:
          if (compare_no != 0) goto cmpz2;
          map0Xto0n(r, n, compare_reg);
          break;
        case Q_LO:
        case Q_LT:
          if (compare_no == 0 && !(mips_opt & 1)) goto cmpz;
          map01to0n(r, n);
          break;
        case Q_GE:
        case Q_HS:
          if (compare_no == 0 && !(mips_opt & 1)) goto cmpz;
          map10to0n(r, n);
          break;
        cmpz:
          syserr("conditional_scc_to_n(unfinished)");
          /*  LabelNumber *skip = nextlabel();
          **  if (compare_reg != R_T24)
          **    syserr("conditional_scc_to_n(unfinished)");
          **  load_integer(r, 0);
          **  outinstrB(br_of_Q(Q_NEGATE(condition)), R_T24, 0, skip);
          **  load_integer_over_zeroreg(r, n);
          **  flush_peepholer();
          **  setlab1(skip);
          */
          break;
        cmpz2:
          syserr("conditional_scc_to_n(unfinished)");
          break;
    }
}

static void Xsetlabel(LabelNumber *l, bool aligned)
{
    ensure_sp();
    flush_peepholer();
    if (aligned)
    {   mcpeepdata *mc_peep1 = new_mcpeep();
        mc_peep1->type = OUT_ALIGN;
        emit_one_peeped(mc_peep1);
    }
    setlab1(l);
}

/* although the idea of setlabel is machine independent, it stays here
   because it back-patches code.  In the long term setlabel should be
   in codebuf.c and call a machine dependent backpatch routine.
*/
void setlabel(LabelNumber *l)
{
    List *p = l->u.frefs;
    if (asmstream) asm_lablist = mkLabList(asm_lablist, l);
    while (p!=NULL)
    {   int32 v = car_(p);
        int32 q = (v & 0x00ffffff);   /* BYTE address */
        int32 w;
        unsigned32 d;
        if (mips_opt & 2)
        { w = code_hword_(q);
          if (!(v & LABREF_short)) w = w<<16 | code_hword_(q+2);
        }
        else
          w = code_inst_(q);
        switch (v & 0xff000000)
        {
    case LABREF_OFF16:     /* e.g. forw. MIPS32 ref. */
            d = (codep-(q+4) >> 2) + (w & 0xffff);
            if (d > 0x7fff) syserr(syserr_displacement, (long)d);
            w = (w & 0xffff0000) | d;
            break;
    case LABREF_M16_S8:    /* MIPS16 BEQ form. */
            d = (codep-(q+2) >> 1) + (w & 0xff);
            if (d > 0x7f) syserr(syserr_displacement, (long)d);
            w = (w & 0xff00) | d & 0xff;
            break;
    case LABREF_M16_S11:   /* MIPS16 B form. */
            d = (codep-(q+2) >> 1) + (w & 0x07ff);
            if (d > 0x03ff) syserr(syserr_displacement, (long)d);
            w = (w & 0xf800) | d & 0x07ff;
            break;
    case LABREF_M16_S16:   /* MIPS16 EXT/BEQ form. */
            d = (codep-(q+4)>>1) + (w>>5 & 0xf800 | w>>16 & 0x07e0 | w & 0x1f);
            if (d > 0x7fff) syserr(syserr_displacement, (long)d);
            w = (w & 0xf800ffe0) | (d&0xf800)<<5  | (d&0x07e0)<<16  | d & 0x1f;
            break;
    case LABREF_M16_U8:    /* MIPS16 LA/LW opcode forw. ref.     */
            d = (codep-(q&~3) >> 2) + (w & 0xff);
            if (d > 0xff) syserr(syserr_displacement, (long)d);
            w = (w & 0xff00) | d & 0xff;
            break;
    case LABREF_M16_S16X:  /* MIPS16 EXT/LW(PC) opcode forw. ref. */
            d = (codep-(q&~3)) + (w>>5 & 0xf800 | w>>16 & 0x07e0 | w & 0x1f);
            if (d > 0x7fff) syserr(syserr_displacement, (long)d);
            w = (w & 0xf800ffe0) | (d&0xf800)<<5  | (d&0x07e0)<<16  | d & 0x1f;
            break;
    case LABREF_LIT32:     /* e.g. forw. literal ref.                  */
/*
 * This uses long forward ref entries - the 'List' was actually a
 * List3 with an extra field (csr) containing a pointer to a CodeXref
 * which will later be used to relocate this word (typically with
 * the base of the code segment).  Adjust this offset suitably.
 */
            {   CodeXref *z = (CodeXref *)(((List3 *)p)->csr);
                z->codexrlitoff += codebase+codep;
                p = (List *)discard3(p);
                continue;
            }
    default:
            syserr(syserr_labref, (long)v);
        }
        if (mips_opt & 2)
          if (!(v & LABREF_short))
            code_hword_(q) = (unsigned)(w>>16), code_hword_(q+2) = (unsigned)w;
          else
            code_hword_(q) = (unsigned)w;
        else
#ifdef REL193_HACK
          if (host_lsbytefirst)
            code_hword_(q) = w, code_hword_(q+2) = w>>16;
          else
            code_hword_(q) = w>>16, code_hword_(q+2) = w;
#else
            code_inst_(q) = w;
#endif
        p = (List *)discard2(p);
    }
    lab_setloc_(l, codep | 0x80000000); /* cheapo union checker for ->frefs */
}

static void flush_peepholer()
{
    if (mc_peep.type != OUT_NULL)
    {
        mcpeepdata *mc_peep1 = new_mcpeep();
        delayed_load_store(mc_peep.reads);
        *mc_peep1 = mc_peep;
        emit_one_peeped(mc_peep1);
        mc_delay = reg2_(mc_peep.writes);
        mc_peep.type = OUT_NULL;
    }
}

static void init_peepholer()
{
    mc_peep.type = OUT_NULL;
}

void localcg_flush_literals(void)
{   if (shared_litpool_size > 0)
    {   Symstr *poolname = shared_litpool->name;
        int i;
        show_entry(poolname, xr_defloc+xr_code);
        for (i = 0; i<shared_litpool_size; i++)
        {   Symstr *sym = shared_litpool->entries[i].sym;
            int32 off = shared_litpool->entries[i].off;
            if (debugging(DEBUG_LOCALCG))
                cc_msg("lit $r%d before codebase %.8x to pool for %.8x\n",
                   sym, off, codebase, shared_litpool->codebase);
            if (sym)
                (void) lit_findword(off, LIT_ADCON, sym,
                        LITF_INCODE|LITF_FIRST|LITF_LAST);
            else
                (void) lit_findwordaux(off, LIT_NUMBER, 0,
                        LITF_INCODE|LITF_FIRST|LITF_LAST);
        }
        shared_litpool_size = 0;
        litlab = nextlabel();
        dumplits2(0);
        show_code(poolname);
    }
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

static void setlab1(LabelNumber *l)
{
    mcpeepdata *mc_peep1 = new_mcpeep();
    flush_peepholer();
    mc_peep1->type = OUT_LABDEF;
    mc_peep1->lab = l;
    emit_one_peeped(mc_peep1);
}

static void setlab2(LabelNumber *l)
{
    setlabel(l);
}

static void outcode16(int32 w)
{
    if ((codep & 2) == 0) {
      outcodeword(OP16_NOP << 16 | OP16_NOP, LIT_OPCODE);
      codep -= 4;
    }
    code_hword_(codep) = (unsigned16)w;
    codep += 2;
}

static void reflab2(mcpeepdata *p)
{
/* This is where I need to discriminate between the two sorts of things */
/* that I put in the ->frefs field of a label entry.                    */
    if (lab_isset_(p->lab))
    { /* u.defn shares with u.frefs */
      int32 dest = s24_(p->lab->u.defn);
      int32 w = p->w;
      if (mips_opt & 2)
      { int32 d = dest - codep >> 1;                    /* halfwords    */
        int32 t = p->reftype;
        d -= 1;   /* Labels are relative to next instruction */
        if (d >= (unsigned32)0xffffff80 && t == LABREF_M16_S8)
          w |= d & 0xff;
        else if (d >= (unsigned32)0xfffffc00 && t == LABREF_M16_S11)
          w |= d & 0x07ff;
        else if (t == LABREF_M16_U8)    /* backward LA/LW(PC)           */
        { /* there can be no previous defn of (forw) LABREF_M16_U8.     */
          /* but X_backaddrlits come here via gen.c 'fake'.             */
          int32 dx = dest - (codep&~3);                 /* bytes        */
          w = w & 0xff00 | extform(dx + ((w & 0xff) << 2));
          t = LABREF_M16_S16X;
        }
        else if (t == LABREF_M16_S16X)
          syserr("gen.c(odd M16_S16X ref)");
        else
        { d -= 1; /* Labels are relative to next instruction */
          if (d < (unsigned32)0xffff8000) syserr(syserr_back_coderef, (long)d);
          t = LABREF_M16_S16;
          w |= extform(d);
        }
        p->reftype = t;
      }
      else
      { int32 d = dest - codep >> 2;                    /* fullwords    */
        d -= 1;   /* Labels are relative to next instruction */
        if (d < (unsigned32)0xffff8000) syserr(syserr_back_coderef, (long)d);
        w |= (d & 0xffff);
      }
      p->w = w;
    }
    else
    {   /* forward ref */
        addfref_(p->lab, codep | p->reftype);
    }
}

static void emit_one_peeped(mcpeepdata *p)
{
    /* first chain together */
    p->next = bb_list;
    bb_list = p;
    /* now (over)estimate size and update pcdraft */
    /* the lsb of pcdraft means 'uncertain size; e.g. after cond branch */
    p->outdraft = pcdraft & ~1;
    switch (p->type) {
case OUT_LABDEF:
      pcdraft = pcdraft & ~1;             /* now know 16-bit aligned.   */
      p->lab->lndraft = pcdraft;
      return;
case OUT_LABREF:
      if (p->lab == 0) { pcdraft += 4; break;}      /* FIXME: check */
      p->lab->lndraft &= ~1;              /* mark referenced.           */
      if (!(mips_opt & 2))
          pcdraft += 4;
/* FIXME: next line needs to test p->reftype.                           */
      else if (p->lab->lndraft <= pcdraft && pcdraft-256 <= p->lab->lndraft)
          pcdraft += 2;     /* backward jump, known not to be too far.  */
      else
      {   pcdraft += 4;     /* 4 for possible MIPS16 OP16_EXT version.  */
          pcdraft |= 1;     /* but also mark size as uncertain...       */
      }
      break;
case OUT_ALIGN:
      if (pcdraft & 2) pcdraft += 2;
      pcdraft |= 1;         /* but also mark size as uncertain...       */
      break;
case OUT_SWSHIFT:
      if (mips_opt & 2) pcdraft += 2; else pcdraft += 4;
      break;
case OUT_SWENT:
      p->lab->lndraft &= ~1;              /* mark referenced.           */
      pcdraft += 4;
      if (mips_opt & 2) pcdraft |= 1;     /* mark size as uncertain...  */
      else pcdraft += 4;                  /* need NOP on MIPS1          */
      break;
case OUT_INSTR:
      if (mips_opt & 2)
      {   if (p->w >> 16) pcdraft += 2;
          pcdraft += 2;
          break;
      }
      /* drop through */
case OUT_CODELABHI:     /* not on MIPS16 */
case OUT_CODELABLO:     /* not on MIPS16 */
case OUT_EXTLABHI:      /* not on MIPS16 */
case OUT_EXTLABLO:      /* not on MIPS16 */
case OUT_EXTLIT:        /* only on MIPS16 */
case OUT_EXTCALL:       /* MIPS16 and MIPS32 -- JAL etc. */
      pcdraft += 4;
      break;
case OUT_LTORG:
      litlab->lndraft = (pcdraft + 3) & ~3;
      cc_msg("estimating litlab L%d at 0x%.6lx\n", lab_xname_(litlab), pcdraft);
      break;
case OUT_COUNT:
      pcdraft += 4;
      break;
default:
      syserr("emit_one_peeped unknown %d", p->type);
    }
}

static void emit_two_peeped(mcpeepdata *p)
{   switch (p->type)
    {
default:
        syserr("emit_two_peeped unknown %d", p->type);
        break;
case OUT_LABDEF:
/*        outalign(p->next); */
        setlab2(p->lab);
        break;
case OUT_ALIGN:
        if (codep & 2) outcode16(OP16_NOP); 
        break;
case OUT_SWSHIFT:       /* MIPS16 (mips_opt & 2) only */
        if (mips_opt & 2)
        {   /* currently a SLL r,#1; but map to SLL r,#2 if big...      */
            outcode16(p->w + (pcdraft>2048 ? 1<<2 : 0));
        }
        else
            outcodeword(p->w, LIT_OPCODE);
        break;
case OUT_SWENT:
        if (mips_opt & 2 && pcdraft>2048) {
            p->w |= extform(0), p->reftype = LABREF_M16_S16;
        }
        /* drop through */
case OUT_LABREF:
        reflab2(p);
        if (p->reftype == LABREF_M16_S16 && (p->w >> 16) == 0)
            syserr("non-EXT DISP16-bit branch");
        /* drop through */
case OUT_INSTR:
        if (mips_opt & 2)
        {   if (p->w >> 16) outcode16(p->w >> 16);
            outcode16(p->w);
        }
        else
            outcodeword(p->w, LIT_OPCODE);
        if (p->type == OUT_SWENT && !(mips_opt & 2))
            outcodeword(OP_NOOP, LIT_OPCODE);
        break;
case OUT_EXTCALL:
        if (mips_opt & 2 && p->w == OP16_B)
        {   /* MIPS16 tailcall instruction */
            int32 d = obj_symref(p->s, xr_code, 0);
            if (d != -1 && codebase+codep+2-d <= 2048)
                                                /* fits in OP16_B (S11)  */
                outcode16(OP16_B | 2047 & -(codebase+codep+2-d>>1));
            else if (d != -1 && codebase+codep+4-d <= 65536)
            {                           /* fits in OP16_EXT:OP16_B (S16) */
                int32 off = 65535 & -(codebase+codep+4-d >> 1);
                int32 w = OP16_B | extform(off);
                outcode16(w >> 16);
                outcode16(w);
            }
            else
            {   syserr("gen.c(tailcall confused 65536)");
            }
            break;
        }
        codexrefs = (CodeXref *)global_list3(SU_Xref, codexrefs,
                                 X_JmpAddr | (codebase+codep), p->s);
        if (mips_opt & 2)
        {   outcode16(p->w >> 16);
            outcode16(p->w);
        }
        else
            outcodewordaux(p->w, LIT_JMPADDRx, p->s);
        break;
case OUT_CODELABHI:     /* not on MIPS16 */
/*          fprintf(stderr,"Adding reference at %lx to %s+%d (L%ld)\n",
            codebase+codep, symname_(p->s), p->off, lab_xname_(p->lab)); */
        codexrefs = (CodeXref *)global_list4(SU_Xref, codexrefs,
                                 X_DataAddr | (codebase+codep), p->s, p->off);
        addlongfref_(p->lab, codep | LABREF_LIT32, codexrefs);
        outcodewordaux(p->w, LIT_RELADDR, p->s);
        break;
case OUT_CODELABLO:     /* not on MIPS16 */
        codexrefs = (CodeXref *)global_list4(SU_Xref, codexrefs,
                                 X_DataAddr1 | (codebase+codep), p->s, p->off);
        addlongfref_(p->lab, codep | LABREF_LIT32, codexrefs);
        outcodewordaux(p->w, LIT_RELADDR, p->s);
        break;
case OUT_EXTLABHI:     /* not on MIPS16 */
        codexrefs = (CodeXref *)global_list4(SU_Xref, codexrefs,
                                 X_DataAddr | (codebase+codep), p->s, p->off);
        /* The next line compensates for sign-extension of EXTLABLO.    */
        outcodewordaux(p->w | (p->off+0x8000)>>16 & 0xffff, LIT_RELADDR, p->s);
        break;
case OUT_EXTLABLO:     /* not on MIPS16 */
        codexrefs = (CodeXref *)global_list4(SU_Xref, codexrefs,
                                 X_DataAddr1 | (codebase+codep), p->s, p->off);
        outcodewordaux(p->w | p->off & 0xffff, LIT_RELADDR, p->s);
        break;
case OUT_EXTLIT:       /* only on MIPS16 */
        /* Otherwise all literals coming here can be addressed via      */
        /* non-extended instructions...                                 */
        {   int noff = improve_phase1_lit(p->s, p->off);
            int32 disp = find_shared_lit(p->s, noff);
            codexrefs = (CodeXref *)global_list4(SU_Xref, codexrefs,
                                 X_DataVal | (codebase+codep),
                                 shared_litpool->name, disp);
            outcode16(p->w);
            p->type = OUT_LABREF;       /* rather a hack ...    */
            p->off = p->off - noff;     /* see mapLITtoLABREF() */
        }
        break;
case OUT_LTORG:
        cc_msg("seting litlab L%d at 0x%.6lx\n", lab_xname_(litlab), codep);
        dumplits2(0);
        break;
case OUT_COUNT:
        outcodeword(p->w, LIT_NUMBER);
        break;
    }
}

static int nastybranch(mcpeepdata *p)
{
    if (!(mips_opt & 2)) return 0;
    if (lab_isset_(p->lab)) return 0;   /* reflab2() fixes these up.    */
    switch (p->reftype)
    {
    /* The next lines really are (e.g.) 256.  We have max 254 offset    */
    /* counting from codep+2 -- hence 256, similarly 2048.              */
    /* Note that counting from p->outdraft instead of codep gives       */
    /* a tighter bound.                                                 */
case LABREF_M16_S8:
        return (p->outdraft+256 >= p->lab->lndraft) ? 0 : 1;
case LABREF_M16_S11:
        return (p->outdraft+2048 >= p->lab->lndraft) ? 0 : 1;
case LABREF_M16_U8:
        /* Here we have up to 255 words from (codep&~3)...              */
        return ((p->outdraft&~3)+1020 >= p->lab->lndraft+4*(p->w&255)) ? 0 : 2;
default:
        return 0;
    }
}

static void mapLITtoLABREF(mcpeepdata *p)
{   Symstr *name = p->s;
    int32 off = p->off, addend = 0;
    int32 disp;
    LabelNumber *lab = litlab;
    if (name)
    {   if ((disp = lit_findword(off, LIT_ADCON, name,
                             LITF_INCODE|LITF_FIRST|LITF_LAST|LITF_PEEK)) >= 0)
            /* nothing */;
/* allow a few bytes for delay-slot slop  (7ff0 instead of 8000) ...    */
        else if ((disp = lit_findadcon(name, off, codebase+codep-0x7ff0)) >= 0)
        {   /* fake a label for this... */
            /* Note there is one label for each offset, so disp=0.      */
            LabelNumber *fake = nextlabel();
            lab_setloc_(fake, disp-codebase);
            lab = fake;
            disp = 0;
        }
        else
        {   int32 noff = improve_phase1_lit(name, off);
            disp = lit_findword(noff, LIT_ADCON, name,
                                LITF_INCODE|LITF_FIRST|LITF_LAST);
            addend = off - noff;
        }
    }
    else
        disp = lit_findwordaux(off, LIT_NUMBER, 0,
                                LITF_INCODE|LITF_FIRST|LITF_LAST);
    p->type = OUT_LABREF;
    p->lab = lab;
    p->w = foo8u(disp, p->w, 2, 0);
    p->reftype = (p->w >> 16) ? LABREF_M16_S16X : LABREF_M16_U8;
    p->off = addend;
}

static void emit_all_peeped()
{   mcpeepdata *p;
    for (p = bb_list; p != NULL; p = p->next)
    {   if (p->type == OUT_EXTLIT && (pcdraft >= litpool_codelimit || mips_opt_zh(2)))
            mapLITtoLABREF(p);
        if (p->type == OUT_LABREF) switch (nastybranch(p))
        {
    case 1:     /* MIPS16 B/BEQ out of range */
          p->reftype = LABREF_M16_S16;
          p->w |= extform(0);
          break;
    case 2:     /* MIPS16 LA/LW out of range */
          p->reftype = LABREF_M16_S16X;
          p->w = p->w & 0xff00 | extform((p->w & 0x00ff) << 2);
          break;
    case 0:
          break;
    default:
          syserr("Unexpected nastybranch value");
        }
        emit_two_peeped(p);
        if (p->type == OUT_LABREF && p->off != 0)
        {   int32 rx = p->w >> 8 & 7;
            if ((p->off + 128 >> 8) != 0)
                syserr("gen.c(EXTLIT off 0x%.4x)", p->off);
            outcode16(OP16_ADDIU2 | rx<<8 | (p->off) & 255);  /* foo8s */
        }
    }
    bb_list = 0;
}

/* end of gen.c */
