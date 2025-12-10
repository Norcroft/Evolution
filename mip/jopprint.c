/*
 * file jopprint.c - things maybe used while debugging compiler
 * Copyright (C) Codemist Ltd., April 1986.
 * Copyright (C) Acorn Computers Ltd., 1988.
 */

/*
 * RCS $Revision: 1.23 $ Codemist 23
 * Checkin $Date: 1995/09/22 12:53:06 $
 * Revising $Author: amycroft $
 */

#ifdef __STDC__
#  include <string.h>
#  include <stdarg.h>
#  include <stdlib.h>
#else
#  include <strings.h>
#  include <varargs.h>
#endif
#include "globals.h"
#include "cgdefs.h"
#include "store.h"
#define DEFINE_JOPTABLE 1
#include "jopcode.h"
#include "aeops.h"
#include "aetree.h"    /* for pr_stringsegs */
#include "flowgraf.h"   /* for is_exit_label */

#if defined ENABLE_CG || defined ENABLE_REGS || defined ENABLE_CSE

static char *condition_name(int32 w)
/* only for print_opname(). Argument already masked with Q_MASK */
{
    char *op;
    switch(w)
    {
case Q_EQ:  op = "(if EQ)"; break;
case Q_NE:  op = "(if NE)"; break;
case Q_HS:  op = "(if HS)"; break;
case Q_LO:  op = "(if LO)"; break;
case Q_MI:  op = "(if MI)"; break;
case Q_PL:  op = "(if PL)"; break;
/* case Q_VS:  op = "(if VS)"; break; */
/* case Q_VC:  op = "(if VC)"; break; */
case Q_HI:  op = "(if HI)"; break;
case Q_LS:  op = "(if LS)"; break;
case Q_GE:  op = "(if GE)"; break;
case Q_LT:  op = "(if LT)"; break;
case Q_GT:  op = "(if GT)"; break;
case Q_LE:  op = "(if LE)"; break;
case Q_AL|J_UNSIGNED:                /* bits moved, rationalise */
case Q_AL:  op = "       "; break;
case Q_NOT: op = "(never)"; break;
case Q_UEQ: op = "(ifUEQ)"; break;
case Q_UNE: op = "(ifUNE)"; break;
case Q_XXX: op = "(ifXXX)"; break;
default:  { static char opbuf[12];
            op = opbuf;
            sprintf(opbuf, "(Q_%.2x???)", (unsigned)(w >> 24));
            break;
          }
    }
    return op;
}

extern void jopprint_opname(J_OPCODE o)
{
    char v[20];
    strcpy(v, joptable[o & J_TABLE_BITS].name);
    if (o & J_SIGNED) strcat(v, "s");
    if (o & J_UNSIGNED) strcat(v, "u");
    if (o & J_ALIGNMENT)
        strcat(v, ((o & J_ALIGNMENT) >> J_ALIGNPOS)*3 + "a1\0a2\0a4\0a8");
    if (o & J_BASEALIGN4) strcat(v, "w");
#if defined TARGET_HAS_SCALED_ADDRESSING || defined TARGET_HAS_SCALED_OPS || \
    defined TARGET_HAS_SCALED_ADD
    if (o & J_NEGINDEX) strcat(v, "m");
    if (o & J_SHIFTMASK)
    {   int32 m = (o & J_SHIFTMASK) >> J_SHIFTPOS;
        if ((m & SHIFT_RIGHT) == 0) strcat(v, "<<");
        else if (m & SHIFT_ARITH) strcat(v, ">>");
        else strcat(v, ">>L");
        sprintf(v+strlen(v), "%ld", (long)(m & SHIFT_MASK));
    }
    cc_msg("%-12s", v);
#else
    cc_msg("%-8s", v);
#endif
}

typedef struct BindListIndex BindListIndex;
struct BindListIndex {
    /* int32 */ IPtr h0;        /* same h0 field as a Binder (for puns) */
    BindListIndex *next;
    Binder *binder;
    BindList *bindlist;
};

static unsigned32 bindlist_n;
static BindListIndex *bindlist_index;

static void pr_bindlist(BindList *p)
{
    int ch = '{';
    for (; p!=NULL; p = p->bindlistcdr) {
        Binder *b = p->bindlistcar;
        if (bindlist_n != 0) {
            if (h0_(b) == s_binder) {
                if (ch == '{') {
                    BindListIndex *bi = (BindListIndex *)SynAlloc(sizeof(BindListIndex));
                    h0_(bi) = ++bindlist_n;
                    bi->binder = b;
                    bi->next = bindlist_index;
                    bi->bindlist = p;
                    bindlist_index = bi;
                    p->bindlistcar = (Binder *)bi;
                    cc_msg("%c#%ld= $b", ch, (long)bindlist_n ^ 0x80000000, b);
                } else
                    cc_msg("%c$b", ch, b);
                ch = ',';
            } else {
                unsigned32 bindlist_nn = (unsigned32)h0_(b);
                cc_msg("%c#%ld#}", ch, (long)bindlist_nn ^ 0x80000000);
                return;
            }
        } else {
            cc_msg("%c$b", ch, b);
            ch = ',';
        }
    }
    cc_msg("%s", (ch=='{') ? "{}" : "}");
}

static void pr_bindlist_head(BindList *p, int32 n)
{
    int ch = '{';
    for (; --n >= 0; p = p->bindlistcdr, ch = ',')
        cc_msg("%c$b", ch, p->bindlistcar);
    cc_msg("%c*}", ch);
}

static void pr_argdesc(int32 d)
{
#ifdef TARGET_FP_ARGS_IN_FP_REGS        /* TARGET_FP_ARGS_CALLSTD1      */
    cc_msg("%ld(%ld,%ld", k_argwords_(d), k_intregs_(d), k_fltregs_(d));
#elif defined(TARGET_FP_ARGS_CALLSTD2)
    cc_msg("%ld(0x%lx", k_argwords_(d), k_argfpmask_(d));
#else
    cc_msg("%ld(", k_argwords_(d));
#endif
    /*if (k_resultregs_(d) > 1) */cc_msg("=>%ld", k_resultregs_(d));
    cc_msg(")");
    if (d & K_SPECIAL_ARG) cc_msg("[+]");/* + implicit arg   */
    if (d & K_PURE) cc_msg("P");         /* pure             */
    if (d & K_INLINE) cc_msg("I");       /* will be inlined  */
    if (d & K_VACALL) cc_msg("...");     /* call vararg fn   */
}

void print_jopcode_1(J_OPCODE op, VRegInt r1, VRegInt r2, VRegInt m)
{
        cc_msg("%8s", condition_name(op & Q_MASK));
        jopprint_opname(op);

        if (gap_r1(op)) cc_msg("-, ");
        else if (uses_r1(op) || pseudo_reads_r1(op))
        {   if (r1.r == GAP) cc_msg("-, ");
            else cc_msg("%ld, ", (long)r1.r);
        }
        else cc_msg("%ld, ", (long)r1.i);

        if (gap_r2(op) ||
            (pseudo_reads_r2(op) && r2.r == GAP))
            cc_msg("-, ");
        else if (op==J_INFOLINE || op==J_COUNT)
            cc_msg("'%s', ", r2.str);
        else if (op==J_CALLK || op==J_CALLR || op==J_OPSYSK ||
                 op==J_TAILCALLK || op==J_TAILCALLR)
        {   pr_argdesc(r2.i);
            cc_msg(", ");
        } else
            cc_msg("%ld, ",
               (long)((uses_r2(op) || pseudo_reads_r2(op)) ? r2.r : r2.i));

        if (uses_stack(op) ||
            op == J_CALLK || op == J_TAILCALLK ||
            op==J_ADCON || op == J_INIT || op == J_INITF || op == J_INITD)
        {   Binder *bb = m.b;
            if (bb == NULL || h0_(bb) == s_identifier)
                /* To allow print_jopcode to be called from local cgs */
                cc_msg("$r", (Symstr *)bb);
            else
            {   cc_msg("$b", bb);
                if (bindstg_(bb) & bitofstg_(s_auto))
                {   VRegnum r3 = bindxx_(bb);
                    if (r3 != GAP) cc_msg(" [r%ld]", (long)r3);
                }
            }
        }
        else if (op==J_STRING)
            pr_stringsegs(m.s);
        else if (op==J_SETSPENV)
        {   if (bindlist_n == 0 && m.bl != NULL && r2.bl != NULL)
            {   int32 lm = length((List *)m.bl);
                int32 lr2 = length((List *)r2.bl);
                if (lm > lr2) {
                    pr_bindlist_head(m.bl, lm - lr2);
                    cc_msg(" from ");
                    pr_bindlist(r2.bl);
                } else {
                    pr_bindlist(m.bl);
                    cc_msg(" from ");
                    pr_bindlist_head(r2.bl, lr2 - lm);
                }
            } else {
                pr_bindlist(m.bl);
                cc_msg(" from ");
                pr_bindlist(r2.bl);
            }
        }
        else if (op==J_SETSPGOTO)
        {   cc_msg("L%ld from ", (long)lab_xname_(m.l));
            pr_bindlist(r2.bl);
        }
        else if (uses_r3(op))
        {   if (m.r == GAP) cc_msg("<**missing register**>");
            else cc_msg("%ld", (long)m.r);
        }
        else if ((op & ~Q_MASK)==J_B || op == J_BXX || op == J_LABEL)
            cc_msg("L%ld", (long)lab_xname_(m.l));
        else switch (op)
        {   case J_MOVDK: case J_CMPDK:
            case J_ADDDK: case J_SUBDK:
            case J_MULDK: case J_DIVDK:
            case J_MOVFK: case J_CMPFK:
            case J_ADDFK: case J_SUBFK:
            case J_MULFK: case J_DIVFK:
            case J_ADCONF:case J_ADCOND:
                cc_msg("%s", m.f->floatstr);
                break;
            case J_ENDPROC:
                cc_msg("-");
                break;
            case J_ENTER:
            case J_SAVE:
                pr_argdesc(m.i);
                break;
            default:
                cc_msg("%ld", (long)m.i);
                if (m.i > 1000 || m.i < -1000) cc_msg("  [%#lx]", (long)m.i);
                break;
        }
}

void print_jopcode(J_OPCODE op, VRegInt r1, VRegInt r2, VRegInt m)
{   cc_msg("        ");
    print_jopcode_1(op, r1, r2, m);
    cc_msg("\n");
    if (op == J_CASEBRANCH || op == J_THUNKTABLE)
    {   LabelNumber **v = r2.lnn;
        int32 i, ncase = m.i;
        r1.r = r2.r = GAP;
        for (i=0; i<ncase; i++)
        {   m.l = v[i];
            print_jopcode(J_BXX, r1, r2, m);
        }
    }
}

void flowgraf_printblock(BlockHead *p, bool deadbits)
{
    VRegInt gap, m;
    Icode    *c = blkcode_(p), *limit;
    gap.r = GAP;
    cc_msg("L%li: \n", (long)lab_name_(blklab_(p)));
    if (c == (Icode *)DUFF_ADDR && blklength_(p) > 0) {
        cc_msg("block eliminated by crossjumping\n\n");
        return;
    }
    for (limit = c + blklength_(p); c < limit; ++c) {
        if (deadbits)
          cc_msg("%c%c%c", (c->op & J_DEAD_R1 ? '1': '-'),
                           (c->op & J_DEAD_R2 ? '2': '-'),
                           (c->op & J_DEAD_R3 ? '3': '-'));
        print_jopcode(c->op & ~J_DEADBITS, c->r1, c->r2, c->m);
    }
    if (!(blkflags_(p) & BLKSWITCH)) {
        if (blkflags_(p) & BLK2EXIT) {
            if (deadbits) cc_msg("   ");
            m.l = blknext1_(p);
            print_jopcode(J_B + (blkflags_(p) & Q_MASK), gap, gap, m);
        }
        if (!(blkflags_(p) & BLK0EXIT)) {
            if (deadbits) cc_msg("   ");
            m.l = blknext_(p);
            print_jopcode(J_B, gap, gap, m);
        }
    }
}

static void flowgraf_print_start(void) {
    bindlist_n = 0x80000000;
    bindlist_index = NULL;
}

static void flowgraf_print_end(void) {
    BindListIndex *p = bindlist_index;
    for (; p != NULL; p = p->next)
        p->bindlist->bindlistcar = p->binder;

    bindlist_index = NULL;
    bindlist_n = 0;
}

void flowgraf_print(const char *mess, bool deadbits)
{   BlockHead *p;
    cc_msg("\n\n%s\n\n", mess);
    flowgraf_print_start();
    for (p = top_block; p != NULL; p = blkdown_(p))
        flowgraf_printblock(p, deadbits);
    flowgraf_print_end();
}

#else
void print_jopcode_1(J_OPCODE op, VRegInt r1, VRegInt r2, VRegInt m)
{
    IGNORE(op); IGNORE(r1); IGNORE(r2); IGNORE(m);
}
void print_jopcode(J_OPCODE op, VRegInt r1, VRegInt r2, VRegInt m)
{
    IGNORE(op); IGNORE(r1); IGNORE(r2); IGNORE(m);
}
void flowgraf_print(const char *mess)
{
    IGNORE(mess);
}
#endif

#ifdef ENABLE_REGS
void print_xjopcode(J_OPCODE op, VRegInt r1, VRegInt r2, VRegInt m,
                    char *fmt, ...)
{   va_list ap;
    va_start(ap, fmt);
    print_jopcode_1(op, r1, r2, m);
    cc_msg(" ");
    _vfprintf(stderr, fmt, ap);
    cc_msg("\n");
    /* since this is only used by regalloc we do not need to print out   */
    /* branch tables of CASEBRANCH as it never calls it with such things */
}
#else
void print_xjopcode(J_OPCODE op, VRegInt r1, VRegInt r2, VRegInt m,
                    char *fmt, ...)
{
    IGNORE(op); IGNORE(r1); IGNORE(r2); IGNORE(m); IGNORE(fmt);
}
#endif

/* end of jopprint.c */
