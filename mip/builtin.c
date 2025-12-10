/*
 * builtin.c: constants/global symbols for C compiler.
 * Copyright (C) Codemist Ltd., 1987-1992.
 * Copyright (C) Acorn Computers Ltd., 1988-1990.
 * Copyright (C) Advanced RISC Machines Limited, 1990-1992.
 */

/*
 * RCS $Revision: 1.44 $ Codemist 23
 * Checkin $Date: 1995/11/24 11:14:47 $
 * Revising $Author: hmeeking $
 */

/* AM memo: names in here are really getting out of control.            */
/* Rework soon, but remember that all names should be distinct in the   */
/* the first 8 chars for linkers like the os370 ones....                */

/* AM memo: more thought is required in this file to account for        */
/* natural (and unnatural) machine parameterisations.  In particular    */
/* getting the bsd vax/bsd sun/sysV names right is a pain.              */

#ifndef _BUILTIN_H
#include <time.h>
#include <string.h>
#include "globals.h"
#include "defs.h"
#include "builtin.h"
#include "bind.h"
#include "store.h"
#include "aeops.h"
#include "aetree.h"

#define builtin_init_cpp() 0
#endif

/* The following line indicates more thought is required re naming. */
#ifdef TARGET_LINKER_OMITS_DOLLAR
#  define SYSPFX "__"
#else
#  define SYSPFX "x$"
#endif

FloatCon  *fc_two_31;   /* floating point constant 2^31 */

FPConst fc_zero;         /* floating point constants 0.0  */
#ifdef PASCAL /*ECN*/
FPConst fc_half;         /*                          0.5  */
FPConst fc_big;          /*               FLT or DBL MAX  */
#endif
FPConst fc_one;          /*                          1.0  */
FPConst fc_two;          /*                          2.0  */
FPConst fc_minusone;     /*                          -1.0  */

TypeExpr *te_boolean;
Expr *lit_false;
Expr *lit_true;

TypeExpr *te_char;    /* = (global)primtype_(bitoftype_(s_char)) */
TypeExpr *te_int;     /* = (global)primtype_(bitoftype_(s_int)) */
TypeExpr *te_ushort, *te_uint, *te_lint, *te_ulint;  /* and friends */
TypeExpr *te_double;  /* = (global)primtype_(bitoftype_(s_double)) */
TypeExpr *te_float;   /* its short friend */
TypeExpr *te_ldble;   /* and its long one */
TypeExpr *te_void;    /* = (global)primtype_(bitoftype_(s_void)) */
TypeExpr *te_charptr; /* = (global)ptrtotype_(te_char))) */
TypeExpr *te_intptr;  /* = (global)ptrtotype_(te_int))) */
TypeExpr *te_voidptr; /* = (global)ptrtotype_(te_void))) */

/* since no-one looks inside datasegment and code segment perhaps they
   should be Symstr's */
Binder *datasegment, *codesegment, *constdatasegment, *ddtorsegment;
#ifdef TARGET_HAS_BSS
Binder *bsssegment;
#endif
Symstr *mainsym, *setjmpsym, *assertsym, *first_arg_sym, *last_arg_sym;
Symstr *libentrypoint, *stackoverflow, *stack1overflow,
       *countroutine, *count1routine;

#ifdef TARGET_IS_ACW
Symstr *c_handler, *stackcheck, *heapend;
#endif
#ifdef TARGET_IS_KCM
Symstr *FPArg1, *FPArg2, *FPArg1x, *FPArg2x, *cnvtdw_routine, *cnvtwd_routine,
  *cnvtsd_routine, *cnvtds_routine, *addd_routine, *subd_routine,
  *negd_routine, *muld_routine, *divd_routine, *cmpd_routine, *divu_routine,
  *remu_routine;
#endif
#ifdef TARGET_IS_SPARC
Symstr *multiply;
Symstr *divide;
Symstr *udivide;
#endif
#ifdef TARGET_IS_ALPHA
Symstr *divide;
Symstr *udivide;
Symstr *sremainder;
Symstr *uremainder;
#endif
Symstr *traproutine;
Symstr *targeterrno;

#ifdef TARGET_IS_ALPHA
/* CSE on various machines with a need for 8-byte aligned stacks may
 * need this...  The "pure" option is disables here just for the Alpha
 * to help short-term debugging.
 */
#define PUREBIT   0
#else
#define PUREBIT   bitoffnaux_(s_pure)
#endif

op_simulation sim;

static Symstr *mallocsym, *callocsym, *reallocsym;

bool returnsheapptr(Symstr *fn) {
    return (fn == mallocsym || fn == callocsym || fn == reallocsym ||
            strncmp("__nw__", fn->symname, 6) == 0);
}

static Binder *library_function_binder(Symstr* sym, TypeExpr* fntype)
{   return global_mk_binder(0,
                            sym,
                            bitofstg_(s_extern) | b_undef | b_fnconst,
                            fntype);
}

static Expr *library_function_1(Symstr* sym, TypeExpr* fntype)
{   return (Expr*) global_list4(SU_Other, s_addrof,
                        global_list4(SU_Type, t_content, fntype, 0, 0),
                        (FileLine *)0,
                        library_function_binder(sym, fntype));
}

static Expr *library_function(char *name, int minf, int maxf, int32 flags)
{   Symstr *sv = sym_insert_id(name);
    TypeExprFnAux s;
    TypeExpr *fntype = g_mkTypeExprfn(t_fnap, te_int, 0, 0,
                           packTypeExprFnAux(s, minf, maxf, 0, 0, flags));
    return library_function_1(sv, fntype);
}

TypeExpr *te_fntype(TypeExpr *res, TypeExpr *a1, TypeExpr *a2,
                                   TypeExpr *a3, TypeExpr *a4,
                                   TypeExpr *a5)
{   TypeExprFnAux s;
    int n = 0;
    FormTypeList *f = 0;
    if (a5) f = mkFormTypeList(f, 0, a5, 0), n++;
    if (a4) f = mkFormTypeList(f, 0, a4, 0), n++;
    if (a3) f = mkFormTypeList(f, 0, a3, 0), n++;
    if (a2) f = mkFormTypeList(f, 0, a2, 0), n++;
    if (a1) f = mkFormTypeList(f, 0, a1, 0), n++;
    return mkTypeExprfn(t_fnap, res, 0, f, packTypeExprFnAux(s, n,n,0,0,0));
}

TypeExpr *g_te_fntype(TypeExpr *res, TypeExpr *a1, TypeExpr *a2,
                                            TypeExpr *a3, TypeExpr *a4,
                                            TypeExpr *a5)
{   TypeExprFnAux s;
    int n = 0;
    FormTypeList *f = 0;
    if (a5) f = g_mkFormTypeList(f, 0, a5, 0), n++;
    if (a4) f = g_mkFormTypeList(f, 0, a4, 0), n++;
    if (a3) f = g_mkFormTypeList(f, 0, a3, 0), n++;
    if (a2) f = g_mkFormTypeList(f, 0, a2, 0), n++;
    if (a1) f = g_mkFormTypeList(f, 0, a1, 0), n++;
    return g_mkTypeExprfn(t_fnap, res, 0, f, packTypeExprFnAux(s, n,n,0,0,0));
}

#ifdef CPLUSPLUS
/* We could use this more for C things too, but beware top-level names  */
/* starting with a single '_' which could upset conforming C progs.     */
static Binder *toplevel_function(char *name, TypeExpr *t)
{   Symstr *sv = sym_insert_id(name);
    DeclRhsList *d = mkDeclRhsList(
        /* declname = */ sv,
        /* decltype = */ t,
        /* declstg =  */ bitofstg_(s_extern) | b_fnconst | b_undef);
    (void)instate_declaration(d, TOPLEVEL);
    /* instate_declaration() returns the unwanted INSTANCE Binder.      */
    return bind_global_(sv);
}
#endif

#ifdef SOFTWARE_FLOATING_POINT
static Expr *floating_function(int nargs, TypeExpr *result,
                               TypeExpr *a1, TypeExpr *a2, char *name)
{
    Symstr *w = sym_insert_id(name),
           *a_name = sym_insert_id("a"),
           *b_name = sym_insert_id("b");
    Binder *b;
    FormTypeList *a = g_mkFormTypeList(0, a_name, a1, 0);
    TypeExprFnAux s;
    int32 fl =
#ifdef SOFTWARE_FLOATING_POINT_RETURNS_DOUBLES_IN_REGISTERS
               (result == te_double) ? bitoffnaux_(s_structreg) :
#else
               (result == te_double) ? 0 :
#endif
                                       bitoffnaux_(s_pure);
    if (nargs != 1) a->ftcdr = g_mkFormTypeList(0, b_name, a2, 0);
    b = global_mk_binder(0,
                         w,
                         bitofstg_(s_extern) | b_undef | b_fnconst,
                         g_mkTypeExprfn(t_fnap, result, 0, a,
                            packTypeExprFnAux(s, nargs, nargs, 0, 0, fl)));
    return (Expr *)b;
}
#endif

#ifdef UNIQUE_DATASEG_NAMES
/* The following routine hacks round a bug in Acorn's linker (June 87) */
/* w.r.t. local symbols in different files being confused.             */
/* Something like it is probably needed for 370 CSECT names.           */
/* Acorn linker bug corrected July 87, so this code disabled.          */
/* ... but the code is still useful for Helios!                        */

static int main_compilation_count = 0;

static char *probably_unique_name(int ch)
{
    static char name[32];
#ifdef TARGET_LINKER_OMITS_DOLLAR
    sprintf(name, "__%c%lx", ch, (long)(20L*time(NULL)+main_compilation_count));
#else
    sprintf(name, "x$%c%lx", ch, (long)(20L*time(NULL)+main_compilation_count));
#endif
    return name;
}
#endif

static void initfpconst(FPConst *fc, const char val[])
{
    fc->s = real_of_string(val, bitoftype_(s_double)|bitoftype_(s_short));
    fc->d = real_of_string(val, bitoftype_(s_double));
}

static Expr *globalize_bool(bool t)
{   return (Expr *)global_list5(SU_Const,
        s_integer, te_boolean,
        (FileLine *)0, (Expr *)((t) ? 1L : 0L), 0);
}

void builtin_init(void)
{
    initfpconst(&fc_zero, "0.0");
#ifdef PASCAL /*ECN*/
    initfpconst(&fc_half, "0.5");
    fc_big.s = real_of_string("3.40282347e+38", bitoftype_(s_double) |
                                                bitoftype_(s_short));
    fc_big.d = real_of_string("1.79769313486231571e+308",
                                                bitoftype_(s_double));
#endif
    initfpconst(&fc_one, "1.0");
    initfpconst(&fc_two, "2.0");
    initfpconst(&fc_minusone, "-1.0");

    fc_two_31 = real_of_string("2147483648.0", bitoftype_(s_double));
#define initprimtype_(t) (TypeExpr*)global_list4(SU_Other, s_typespec, (t),0,0);
    te_char = initprimtype_(bitoftype_(s_char));
    te_int = initprimtype_(bitoftype_(s_int));
    te_ushort = initprimtype_(bitoftype_(s_int)|bitoftype_(s_short)|bitoftype_(s_unsigned));
    te_uint = initprimtype_(bitoftype_(s_int)|bitoftype_(s_unsigned));
    te_lint = initprimtype_(bitoftype_(s_int)|bitoftype_(s_long));
    te_ulint = initprimtype_(bitoftype_(s_int)|bitoftype_(s_long)|
                             bitoftype_(s_unsigned));
    te_double = initprimtype_(bitoftype_(s_double));
    te_float = initprimtype_(bitoftype_(s_double)|bitoftype_(s_short));
    te_ldble = initprimtype_(bitoftype_(s_double)|bitoftype_(s_long));
    te_void = initprimtype_(bitoftype_(s_void));
#define g_ptrtotype_(t) (TypeExpr*)global_list4(SU_Other, t_content, (t), 0, 0)
    te_charptr = g_ptrtotype_(te_char);
    te_intptr = g_ptrtotype_(te_int);
    te_voidptr = g_ptrtotype_(te_void);

#if defined(TARGET_IS_UNIX) && !defined(TARGET_IS_SPARC) && !defined(TARGET_IS_ALPHA)
    sim.mulfn = library_function("x$mul", 2, 2, PUREBIT);
    sim.divfn = library_function("x$div", 2, 2, PUREBIT);
    sim.udivfn = library_function("x$udiv", 2, 2, PUREBIT);
    sim.divtestfn = library_function("x$divtest", 1, 1, PUREBIT);
    sim.remfn = library_function("x$mod", 2, 2, PUREBIT);
    sim.uremfn = library_function("x$umod", 2, 2, PUREBIT);
    sim.fdivfn = library_function("x$fdiv", 2, 2, PUREBIT);
    sim.ddivfn = library_function("x$ddiv", 2, 2, PUREBIT);
#else
#ifdef TARGET_LINKER_OMITS_DOLLAR
    sim.mulfn = library_function("__multiply", 2, 2, PUREBIT);
    sim.divfn = library_function("__divide", 2, 2, PUREBIT);
    sim.udivfn = library_function("__udivide", 2, 2, PUREBIT);
    sim.divtestfn = library_function("__divtest", 1, 1, PUREBIT);
    sim.remfn = library_function("__remainder", 2, 2, PUREBIT);
    sim.uremfn = library_function("__uremainder", 2, 2, PUREBIT);
    sim.fdivfn = library_function("__fdivide", 2, 2, PUREBIT);
    sim.ddivfn = library_function("__ddivide", 2, 2, PUREBIT);
#else
/* the 'obsolete's below refer to the ARM only.                         */
    sim.mulfn = library_function("x$multiply", 2, 2, PUREBIT);  /* obsolete */
#if defined(TARGET_IS_ARM_OR_THUMB) && !defined(OBSOLETE_ARM_NAMES)
    sim.divfn = library_function(TARGET_PREFIX("__rt_sdiv"), 2, 2, PUREBIT);
    sim.udivfn = library_function(TARGET_PREFIX("__rt_udiv"), 2, 2, PUREBIT);
    sim.divtestfn = library_function(TARGET_PREFIX("__rt_divtest"), 1, 1, PUREBIT);
#else
    sim.divfn = library_function("x$divide", 2, 2, PUREBIT);
    sim.udivfn = library_function("x$udivide", 2, 2, PUREBIT);
    sim.divtestfn = library_function("x$divtest", 1, 1, PUREBIT);
#endif
    sim.remfn = library_function("x$remainder", 2, 2, PUREBIT);     /* obsolete */
    sim.uremfn = library_function("x$uremainder", 2, 2, PUREBIT);   /* obsolete */
    sim.fdivfn = library_function("x$fdivide", 2, 2, PUREBIT);
    sim.ddivfn = library_function("x$ddivide", 2, 2, PUREBIT);
#endif
#endif
#ifdef TARGET_HAS_DIV_10_FUNCTION
#if defined(TARGET_IS_ARM_OR_THUMB) && !defined(OBSOLETE_ARM_NAMES)
    sim.div10fn = library_function(TARGET_PREFIX("__rt_sdiv10"), 1, 1, PUREBIT);
    sim.udiv10fn = library_function(TARGET_PREFIX("__rt_udiv10"), 1, 1, PUREBIT);
#else
    sim.div10fn = library_function("_kernel_sdiv10", 1, 1, PUREBIT);
    sim.udiv10fn = library_function("_kernel_udiv10", 1, 1, PUREBIT);
#endif
    sim.rem10fn = library_function("_kernel_srem10", 1, 1, PUREBIT);  /* obsolete */
    sim.urem10fn = library_function("_kernel_urem10", 1, 1, PUREBIT); /* obsolete */
#endif
    sim.xprintf = library_function("_printf", 1, 1999, 0L);
    sim.xfprintf = library_function("_fprintf", 2, 1999, 0L);
    sim.xsprintf = library_function("_sprintf", 2, 1999, 0L);
    sim.yprintf = sym_insert_id("printf");
    sim.yfprintf = sym_insert_id("fprintf");
    sim.ysprintf = sym_insert_id("sprintf");

#ifdef STRING_COMPRESSION
    sim.xprintf_z = library_function("_printf$Z", 1, 1999, 0L);
    sim.xfprintf_z = library_function("_fprintf$Z", 2, 1999, 0L);
    sim.xsprintf_z = library_function("_sprintf$Z", 2, 1999, 0L);
    sim.yprintf_z = library_function("printf$Z", 1, 1999, 0L);
    sim.yfprintf_z = library_function("fprintf$Z", 2, 1999, 0L);
    sim.ysprintf_z = library_function("sprintf$Z", 2, 1999, 0L);
#endif
#ifdef SOFTWARE_FLOATING_POINT
    sim.dadd = floating_function(2,te_double,te_double,te_double,TARGET_PREFIX("_dadd"));
    sim.dsubtract = floating_function(2,te_double,te_double,te_double,TARGET_PREFIX("_dsub"));
    sim.dmultiply = floating_function(2,te_double,te_double,te_double,TARGET_PREFIX("_dmul"));
    sim.ddivide = floating_function(2,te_double,te_double,te_double,TARGET_PREFIX("_ddiv"));
    sim.dnegate = floating_function(1,te_double,te_double,NULL,TARGET_PREFIX("_dneg"));
    sim.dgreater = floating_function(2,te_int,te_double,te_double,TARGET_PREFIX("_dgr"));
    sim.dgeq = floating_function(2,te_int,te_double,te_double,TARGET_PREFIX("_dgeq"));
    sim.dless = floating_function(2,te_int,te_double,te_double,TARGET_PREFIX("_dls"));
    sim.dleq = floating_function(2,te_int,te_double,te_double,TARGET_PREFIX("_dleq"));
    sim.dequal = floating_function(2,te_int,te_double,te_double,TARGET_PREFIX("_deq"));
    sim.dneq = floating_function(2,te_int,te_double,te_double,TARGET_PREFIX("_dneq"));
    sim.dfloat = floating_function(1,te_double,te_int,NULL,TARGET_PREFIX("_dflt"));
    sim.dfloatu = floating_function(1,te_double,te_uint,NULL,TARGET_PREFIX("_dfltu"));
    sim.dfix = floating_function(1,te_int,te_double,NULL,TARGET_PREFIX("_dfix"));
    sim.dfixu = floating_function(1,te_uint,te_double,NULL,TARGET_PREFIX("_dfixu"));

    sim.fadd = floating_function(2,te_float,te_int,te_int,TARGET_PREFIX("_fadd"));
    sim.fsubtract = floating_function(2,te_float,te_int,te_int,TARGET_PREFIX("_fsub"));
    sim.fmultiply = floating_function(2,te_float,te_int,te_int,TARGET_PREFIX("_fmul"));
    sim.fdivide = floating_function(2,te_float,te_int,te_int,TARGET_PREFIX("_fdiv"));
    sim.fnegate = floating_function(1,te_float,te_int,NULL,TARGET_PREFIX("_fneg"));
    sim.fgreater = floating_function(2,te_int,te_int,te_int,TARGET_PREFIX("_fgr"));
    sim.fgeq = floating_function(2,te_int,te_int,te_int,TARGET_PREFIX("_fgeq"));
    sim.fless = floating_function(2,te_int,te_int,te_int,TARGET_PREFIX("_fls"));
    sim.fleq = floating_function(2,te_int,te_int,te_int,TARGET_PREFIX("_fleq"));
    sim.fequal = floating_function(2,te_int,te_int,te_int,TARGET_PREFIX("_feq"));
    sim.fneq = floating_function(2,te_int,te_int,te_int,TARGET_PREFIX("_fneq"));
    sim.ffloat = floating_function(1,te_float,te_int,NULL,TARGET_PREFIX("_fflt"));
    sim.ffloatu = floating_function(1,te_float,te_uint,NULL,TARGET_PREFIX("_ffltu"));
    sim.ffix = floating_function(1,te_int,te_int,NULL,TARGET_PREFIX("_ffix"));
    sim.ffixu = floating_function(1,te_uint,te_int,NULL,TARGET_PREFIX("_ffixu"));

    sim.fnarrow = floating_function(1,te_float,te_double,NULL,TARGET_PREFIX("_d2f"));
    sim.dwiden = floating_function(1,te_double,te_float,NULL,TARGET_PREFIX("_f2d"));

#ifdef TARGET_SOFTFP_SUPPORT_INCLUDES_REVERSE_OPS
    sim.drsb = floating_function(2,te_double,te_double,te_double,TARGET_PREFIX("_drsb"));
    sim.drdiv = floating_function(2,te_double,te_double,te_double,TARGET_PREFIX("_drdiv"));
    sim.frsb = floating_function(2,te_float,te_int,te_int,TARGET_PREFIX("_frsb"));
    sim.frdiv = floating_function(2,te_float,te_int,te_int,TARGET_PREFIX("_frdiv"));
#endif

#endif
#if defined(TARGET_IS_ARM) && !defined(OBSOLETE_ARM_NAMES)
    sim.readcheck1 = library_function("__rt_rd1chk", 1, 1, PUREBIT);
    sim.readcheck2 = library_function("__rt_rd2chk", 1, 1, PUREBIT);
    sim.readcheck4 = library_function("__rt_rd4chk", 1, 1, PUREBIT);
    sim.writecheck1 = library_function("__rt_wr1chk", 1, 1, PUREBIT);
    sim.writecheck2 = library_function("__rt_wr2chk", 1, 1, PUREBIT);
    sim.writecheck4 = library_function("__rt_wr4chk", 1, 1, PUREBIT);
#else
    sim.readcheck1 = library_function("_rd1chk", 1, 1, PUREBIT);
    sim.readcheck2 = library_function("_rd2chk", 1, 1, PUREBIT);
    sim.readcheck4 = library_function("_rd4chk", 1, 1, PUREBIT);
    sim.writecheck1 = library_function("_wr1chk", 1, 1, PUREBIT);
    sim.writecheck2 = library_function("_wr2chk", 1, 1, PUREBIT);
    sim.writecheck4 = library_function("_wr4chk", 1, 1, PUREBIT);
#endif
    sim.proc_entry = library_function("_proc_entry", 1, 1999, 0L);
    sim.proc_exit  = library_function("_proc_exit",  1, 1999, 0L);

/* _memcpyfn and _memsetfn are internals for (aligned) struct copy/clr  */
    sim.memcpyfn = library_function("_memcpy", 3, 3, 0L);
    sim.memsetfn = library_function("_memset", 3, 3, 0L);
    sim.realmemcpyfn = library_function("memcpy", 3, 3, 0L);
    sim.realmemsetfn = library_function("memset", 3, 3, 0L);

    sim.strcpysym = sym_insert_id("strcpy");
    mallocsym = sym_insert_id("malloc");
    callocsym = sym_insert_id("calloc");
    reallocsym = sym_insert_id("realloc");
/* _word(nnn) is a specially-treated 'function' to put nnn in-line in the */
/* generated code.  People may have views on a better name for it, esp.   */
/* in view of machines with byte and halfword instructions!               */
/* Introduced by ACN to help him with an 88000 library.                   */
    sim.inserted_word = library_function("_word", 1, 1, 0L);
    add_toplevel_binder((Binder *)arg1_(sim.inserted_word));    /* @@@?   */

#ifdef TARGET_IS_ACW
    c_handler = sym_insert_id(SYSPFX"c_handler");
    stackcheck = sym_insert_id(SYSPFX"stackcheck");
    heapend = sym_insert_id("CurrentHeapEnd");
#endif
#ifdef TARGET_LINKER_OMITS_DOLLAR
    stackoverflow = sym_insert_id("__stack_overflow");
    stack1overflow = sym_insert_id("__stack_overflow_1");
#else
#if defined(TARGET_IS_ARM_OR_THUMB) && !defined(OBSOLETE_ARM_NAMES)
    stackoverflow  = sym_insert_id(TARGET_PREFIX("__rt_stkovf_split_small"));
    stack1overflow = sym_insert_id(TARGET_PREFIX("__rt_stkovf_split_big"));
#else
    stackoverflow = sym_insert_id("x$stack_overflow");
    stack1overflow = sym_insert_id("x$stack_overflow_1");
#endif
#endif
    datasegment = global_mk_binder(0,
#ifdef UNIQUE_DATASEG_NAMES
                sym_insert_id(probably_unique_name('d')),
#else
#ifdef TARGET_LINKER_OMITS_DOLLAR
                sym_insert_id("__dataseg"),
#else
                sym_insert_id("x$dataseg"),
#endif
#endif
                bitofstg_(s_static),
                te_int);
    codesegment = global_mk_binder(0,
#ifdef UNIQUE_DATASEG_NAMES
                sym_insert_id(probably_unique_name('c')),
#else
#ifdef TARGET_LINKER_OMITS_DOLLAR
                sym_insert_id("__codeseg"),
#else
                sym_insert_id("x$codeseg"),
#endif
#endif
                bitofstg_(s_static),
                te_int);
#ifdef TARGET_HAS_BSS
    bsssegment = global_mk_binder(0,
#ifdef UNIQUE_DATASEG_NAMES
                sym_insert_id(probably_unique_name('z')),
#else
#ifdef TARGET_LINKER_OMITS_DOLLAR
                sym_insert_id("__bssseg"),
#else
                sym_insert_id("x$bssseg"),
#endif
#endif
                bitofstg_(s_static),
                te_int);
#endif
    /* C++ only really */
    ddtorsegment = global_mk_binder(0,
#ifdef UNIQUE_DATASEG_NAMES
                sym_insert_id(probably_unique_name('v')),
#else
#ifdef TARGET_LINKER_OMITS_DOLLAR
                sym_insert_id("_ddtorvec"),
#else
                sym_insert_id("x$ddtorvec"),
#endif
#endif
                bitofstg_(s_static),
                te_int);
    constdatasegment = global_mk_binder(0,
#ifdef UNIQUE_DATASEG_NAMES
                sym_insert_id(probably_unique_name('q')),
#else
#ifdef TARGET_LINKER_OMITS_DOLLAR
                sym_insert_id("__constdata"),
#else
                sym_insert_id("x$constdata"),
#endif
#endif
                bitofstg_(s_static)|u_constdata,
                te_int);
    mainsym = sym_insert_id("main");
    setjmpsym = sym_insert_id("setjmp");
    assertsym = sym_insert_id("___assert");
/* AM: hmm, is the name '___assert right in that users might get to see */
/* it if (say) a semicolon is omitted (check macro which use) and       */
/* query the next line which would mean ___assert without () fn call    */
/* would not get reported, or be done confusingly.  Probably OK.        */
    implicit_decl(assertsym, 1);    /* forge an 'extern int ___assert()' */
    first_arg_sym = sym_insert_id("___first_arg");
    last_arg_sym = sym_insert_id("___last_arg");
    libentrypoint = sym_insert_id("__main");
#ifdef TARGET_LINKER_OMITS_DOLLAR
    countroutine = sym_insert_id("__mcount");/*for Unix, x$ goes*/
#else
    countroutine = sym_insert_id("x$mcount");/*for Unix, x$ goes*/
#endif
    count1routine = sym_insert_id("_count1");
#ifdef RANGECHECK_SUPPORTED
#ifdef PASCAL /*ECN*/
    sim.abcfault = sym_insert_id("_range");
    sim.valfault = sym_insert_id("_badvalue");
#else
    sim.abcfault = sym_insert_id("__range"); /* BSD F77 library name */
# ifdef TARGET_LINKER_OMITS_DOLLAR
    sim.valfault = sym_insert_id("__badvalue");
# else
    sim.valfault = sym_insert_id("x$badvalue");
#endif
#endif
#endif
#ifdef TARGET_IS_KCM
    FPArg1         = sym_insert_id("_ARG1");
    FPArg2         = sym_insert_id("_ARG2");
    FPArg1x        = sym_insert_id("_ARG1_x1");
    FPArg2x        = sym_insert_id("_ARG2_x1");
    cnvtdw_routine = sym_insert_id("_CNVTDW");
    cnvtwd_routine = sym_insert_id("_CNVTWD");
    cnvtds_routine = sym_insert_id("_CNVTDS");
    cnvtsd_routine = sym_insert_id("_CNVTSD");
    addd_routine   = sym_insert_id("_ADDD");
    subd_routine   = sym_insert_id("_SUBD");
    negd_routine   = sym_insert_id("_NEGD");
    muld_routine   = sym_insert_id("_MULD");
    divd_routine   = sym_insert_id("_DIVD");
    cmpd_routine   = sym_insert_id("_CMPD");
    divu_routine   = sym_insert_id("_DIVU");
    remu_routine   = sym_insert_id("_REMU");
#endif
#ifdef TARGET_IS_SPARC
    multiply       = sym_insert_id("__multiply");
    divide         = sym_insert_id("__divide");
    udivide        = sym_insert_id("__udivide");
#endif
#ifdef TARGET_IS_ALPHA
/*
 * The following seem to be built into the Alpha (OSF) libraries,
 * with arguments passed (unusually) in $24, $25 and a result
 * delivered in $27.  Except for $23 (R_LR) registers are undisturbed,
 * I believe.
 */
    divide         = sym_insert_id("__divl");
    udivide        = sym_insert_id("__divlu");
    sremainder     = sym_insert_id("__reml");
    uremainder     = sym_insert_id("__remlu");
#endif
    traproutine    = sym_insert_id("__syscall");
    targeterrno    = sym_insert_id("errno");

    if (LanguageIsCPlusPlus)
        builtin_init_cpp();
    else
    {   te_boolean = te_int;
        lit_false = globalize_bool(NO);
        lit_true = globalize_bool(YES);
    }
}

/* end of builtin.c */
