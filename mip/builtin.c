/*
 * builtin.c: constants/global symbols for C compiler.
 * Copyright (C) Codemist Ltd., 1987-1992.
 * Copyright (C) Acorn Computers Ltd., 1988-1990.
 * Copyright (C) Advanced RISC Machines Limited, 1990-1992.
 */

/*
 * RCS $Revision: 1.18 $ Codemist 23a
 * Checkin $Date: 93/09/29 16:07:17 $
 * Revising $Author: lsmith $
 */

/* AM memo: names in here are really getting out of control.            */
/* Rework soon, but remember that all names should be distinct in the   */
/* the first 8 chars for linkers like the os370 ones....                */

/* AM memo: more thought is required in this file to account for        */
/* natural (and unnatural) machine parameterisations.  In particular    */
/* getting the bsd vax/bsd sun/sysV names right is a pain.              */

#include <time.h>
#include <string.h>
#include "globals.h"
#include "defs.h"
#include "builtin.h"
#include "sem.h"        /* ptrtotype_() */
#include "bind.h"
#include "store.h"
#include "aeops.h"
#include "aetree.h"

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

TypeExpr *te_char;   /* = (global)primtype_(bitoftype_(s_char)) */
TypeExpr *te_int;    /* = (global)primtype_(bitoftype_(s_int)) */
TypeExpr *te_uint, *te_lint, *te_ulint;  /* and friends */
TypeExpr *te_double; /* = (global)primtype_(bitoftype_(s_double)) */
TypeExpr *te_float;  /* its short friend */
TypeExpr *te_ldble;  /* and its long one */
TypeExpr *te_void;   /* = (global)primtype_(bitoftype_(s_void)) */
#ifdef EXTENSION_FRAC
TypeExpr *te_frac;
TypeExpr *te_lfrac;
#endif

#define te_ptr (sizeof_int==sizeof_ptr ? te_int : te_lint)
#define te_i32 (sizeof_int<4 ? te_lint : te_int)
#define te_u32 (sizeof_int<4 ? te_ulint : te_uint)
#define te_iflt te_i32

/* since no-one looks inside datasegment and code segment perhaps they
   should be Symstr's */
Binder *codesegment;
#ifndef TARGET_ASM_NAMES_LITERALS
Binder *datasegment, *constdatasegment;
#ifdef TARGET_IS_XAP_OR_NEC
Binder *zvdatasegment, *zcdatasegment;
#ifdef TARGET_HAS_BSS
Binder *zbsssegment;
#endif
#endif
#ifdef TARGET_HAS_BSS
Binder *bsssegment;
#endif
#ifdef TARGET_HAS_NEC_SECTS
Binder *tidatasegment, *sidatasegment, *sedatasegment, *sebsssegment;
#endif
#ifdef TARGET_HAS_C4P_SECTS
Binder *idatasegment, *iconstsegment, *ibsssegment;
#endif
#endif /* TARGET_ASM_NAMES_LITERALS */
Symstr *mainsym, *setjmpsym, *assertsym, *first_arg_sym, *last_arg_sym;
Symstr *thissym, *ctorsym, *dtorsym, *vtabsym;
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
#endif
#ifdef TARGET_IS_XAP_OR_NEC
/* CSE has a bug on nec-850 which incorrectly optimises calls to PURE fns.
 * The best solution is to upgrade the base version of NCC??
 */
#define PUREBIT   0
#endif

#ifndef PUREBIT
#define PUREBIT   bitoffnaux_(s_pure)
#endif

op_simulation sim;

static Symstr *mallocsym, *callocsym, *reallocsym;

bool returnsheapptr(Symstr *fn) {
    return (fn == mallocsym || fn == callocsym || fn == reallocsym ||
            strncmp("__nw__", fn->symname, 6) == 0);
}

static Expr *library_function_x(char *name, TypeExpr *tres, int minf, int maxf,
                              int32 flags, int32 rdesc)
{
    Symstr *w = sym_insert_id(name);
    Binder *b;
    TypeExprFnAux s;
    TypeExpr *t = g_mkTypeExprfn(t_fnap, tres, 0, 0,
                      packTypeExprFnAux2(s, minf, maxf, 0, 0, flags, rdesc));
    b = global_mk_binder(0,
                         w,
                         bitofstg_(s_extern) | b_undef | b_fnconst,
                         t);
    return (Expr*) global_list4(SU_Other, s_addrof,
                        global_list4(SU_Type, t_content, t, 0, 0),
                        (FileLine *)0,
                        b);
}
#define library_function(n,a,b,c) library_function_x(n,te_i32,a,b,c,-1) 
#define library_function_t(n,t,a,b,c) library_function_x(n,t,a,b,c,-1) 
#define library_function_r(n,a,b,c,rd) library_function_x(n,te_i32,a,b,c,rd) 

#ifdef CPLUSPLUS
static TypeExpr *te_fntype(TypeExpr *res, TypeExpr *a1, TypeExpr *a2,
                                          TypeExpr *a3, TypeExpr *a4)
{   TypeExprFnAux s;
    int n = 0;
    FormTypeList *f = 0;
    if (a4) f = mkFormTypeList(f, 0, a4, 0), n++;
    if (a3) f = mkFormTypeList(f, 0, a3, 0), n++;
    if (a2) f = mkFormTypeList(f, 0, a2, 0), n++;
    if (a1) f = mkFormTypeList(f, 0, a1, 0), n++;
    return mkTypeExprfn(t_fnap, res, 0, f, packTypeExprFnAux(s, n,n,0,0,0));
}

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
/* Keep _fadd etc for ARM, else use __fadd etc.                         */
#ifdef TARGET_IS_ARM
    Symstr *w = sym_insert_id(name+1),
#else
    Symstr *w = sym_insert_id(name),
#endif
           *a_name = sym_insert_id("a"),
           *b_name = sym_insert_id("b");
    Binder *b;
    FormTypeList *a = g_mkFormTypeList(0, a_name, a1, 0);
    TypeExprFnAux s;
    if (nargs != 1) a->ftcdr = g_mkFormTypeList(0, b_name, a2, 0);
    b = global_mk_binder(0,
                         w,
                         bitofstg_(s_extern) | b_undef | b_fnconst,
                         g_mkTypeExprfn(t_fnap, result, 0, a,
                            packTypeExprFnAux(s, nargs, nargs, 0, 0, 0)));
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
    te_uint = initprimtype_(bitoftype_(s_int)|bitoftype_(s_unsigned));
    te_lint = initprimtype_(bitoftype_(s_int)|bitoftype_(s_long));
    te_ulint = initprimtype_(bitoftype_(s_int)|bitoftype_(s_long)|
                             bitoftype_(s_unsigned));
    te_double = initprimtype_(bitoftype_(s_double));
    te_float = initprimtype_(bitoftype_(s_double)|bitoftype_(s_short));
    te_ldble = initprimtype_(bitoftype_(s_double)|bitoftype_(s_long));
    te_void = initprimtype_(bitoftype_(s_void));
#ifdef EXTENSION_FRAC
    te_frac = initprimtype_(bitoftype_(s_int)|bitoftype_(s_frac));
    te_lfrac = initprimtype_(bitoftype_(s_int)|bitoftype_(s_frac)|
                             bitoftype_(s_long));
#endif

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
#ifdef TARGET_IS_XAP_OR_NEC
#ifdef TARGET_IS_NEC
#ifdef TARGET_IS_C4P /* @@@ nasty hack */
    sim.mulfn = library_function("__mul", 2, 2, PUREBIT);
#else
    /* The next line requests special reg use (see __mul.c).            */
    sim.mulfn = library_function_r("__mul", 2, 2, PUREBIT, 0x0002);
#endif
#else
    sim.mulfn = library_function("__mul", 2, 2, PUREBIT);
#endif
    sim.divfn = library_function("__div", 2, 2, PUREBIT);
    sim.udivfn = library_function("__udiv", 2, 2, PUREBIT);
    sim.divtestfn = library_function("__divtst", 1, 1, PUREBIT);
    sim.remfn = library_function("__rem", 2, 2, PUREBIT);
    sim.uremfn = library_function("__urem", 2, 2, PUREBIT);
#else
    sim.mulfn = library_function("__multiply", 2, 2, PUREBIT);
    sim.divfn = library_function("__divide", 2, 2, PUREBIT);
    sim.udivfn = library_function("__udivide", 2, 2, PUREBIT);
    sim.divtestfn = library_function("__divtest", 1, 1, PUREBIT);
    sim.remfn = library_function("__remainder", 2, 2, PUREBIT);
    sim.uremfn = library_function("__uremainder", 2, 2, PUREBIT);
#endif
    sim.fdivfn = library_function("__fdivide", 2, 2, PUREBIT);
    sim.ddivfn = library_function("__ddivide", 2, 2, PUREBIT);
#else
/* the 'obsolete's below refer to the ARM only.                         */
    sim.mulfn = library_function("x$multiply", 2, 2, PUREBIT);  /* obsolete */
#if defined(TARGET_IS_ARM) && !defined(OBSOLETE_ARM_NAMES)
    sim.divfn = library_function("__rt_sdiv", 2, 2, PUREBIT);
    sim.udivfn = library_function("__rt_udiv", 2, 2, PUREBIT);
    sim.divtestfn = library_function("__rt_divtest", 1, 1, PUREBIT);
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
#ifdef EXTENSION_FRAC
    sim.xmulfn = library_function("__xmul", 2, 2, PUREBIT);
    sim.xdivfn = library_function("__xdiv", 2, 2, PUREBIT);
#endif
#ifdef TARGET_HAS_DIV_10_FUNCTION
#if defined(TARGET_IS_ARM) && !defined(OBSOLETE_ARM_NAMES)
    sim.div10fn = library_function("__rt_sdiv10", 1, 1, PUREBIT);
    sim.udiv10fn = library_function("__rt_udiv10", 1, 1, PUREBIT);
#else
    sim.div10fn = library_function("_kernel_sdiv10", 1, 1, PUREBIT);
    sim.udiv10fn = library_function("_kernel_udiv10", 1, 1, PUREBIT);
#endif
    sim.rem10fn = library_function("_kernel_srem10", 1, 1, PUREBIT);  /* obsolete */
    sim.urem10fn = library_function("_kernel_urem10", 1, 1, PUREBIT); /* obsolete */
#endif
    sim.xprintf = library_function_t("_printf", te_int, 1, 1999, 0L);
    sim.xfprintf = library_function_t("_fprintf", te_int, 2, 1999, 0L);
    sim.xsprintf = library_function_t("_sprintf", te_int, 2, 1999, 0L);
    sim.yprintf = sym_insert_id("printf");
    sim.yfprintf = sym_insert_id("fprintf");
    sim.ysprintf = sym_insert_id("sprintf");
    sim.xscanf = library_function_t("_scanf", te_int, 1, 1999, 0L);
    sim.xfscanf = library_function_t("_fscanf", te_int, 2, 1999, 0L);
    sim.xsscanf = library_function_t("_sscanf", te_int, 2, 1999, 0L);
    sim.yscanf = sym_insert_id("scanf");
    sim.yfscanf = sym_insert_id("fscanf");
    sim.ysscanf = sym_insert_id("sscanf");
#ifdef SOFTWARE_FLOATING_POINT
    {   TypeExpr *f32, *f64;
/*
 * Here I will rely on the fact that casts between floats that happen
 * to be the same length will not generate any code. Therefore I can
 * attribute to these built-in functions "any" floating point type of the
 * width that I require.
 */
        if (target_singlefloat)
        {   f32 = te_double;
            f64 = te_ldble;
        }
        else
        {   f32 = te_float;
            f64 = te_double;
        }
        sim.dadd = floating_function(2,f64,f64,f64,"__dadd");
        sim.dsubtract = floating_function(2,f64,f64,f64,"__dsub");
        sim.dmultiply = floating_function(2,f64,f64,f64,"__dmul");
        sim.ddivide = floating_function(2,f64,f64,f64,"__ddiv");
        sim.dnegate = floating_function(1,f64,f64,NULL,"__dneg");
        sim.dgreater = floating_function(2,te_int,f64,f64,"__dgr");
        sim.dgeq = floating_function(2,te_int,f64,f64,"__dgeq");
        sim.dless = floating_function(2,te_int,f64,f64,"__dls");
        sim.dleq = floating_function(2,te_int,f64,f64,"__dleq");
        sim.dequal = floating_function(2,te_int,f64,f64,"__deq");
        sim.dneq = floating_function(2,te_int,f64,f64,"__dneq");
        sim.dfloat = floating_function(1,f64,te_i32,NULL,"__dflt");
        sim.dfloatu = floating_function(1,f64,te_u32,NULL,"__dfltu");
        sim.dfix = floating_function(1,te_i32,f64,NULL,"__dfix");
        sim.dfixu = floating_function(1,te_u32,f64,NULL,"__dfixu");

        sim.fadd = floating_function(2,f32,te_iflt,te_iflt,"__fadd");
        sim.fsubtract = floating_function(2,f32,te_iflt,te_iflt,"__fsub");
        sim.fmultiply = floating_function(2,f32,te_iflt,te_iflt,"__fmul");
        sim.fdivide = floating_function(2,f32,te_iflt,te_iflt,"__fdiv");
        sim.fnegate = floating_function(1,f32,te_iflt,NULL,"__fneg");
        sim.fgreater = floating_function(2,te_int,te_iflt,te_iflt,"__fgr");
        sim.fgeq = floating_function(2,te_int,te_iflt,te_iflt,"__fgeq");
        sim.fless = floating_function(2,te_int,te_iflt,te_iflt,"__fls");
        sim.fleq = floating_function(2,te_int,te_iflt,te_iflt,"__fleq");
        sim.fequal = floating_function(2,te_int,te_iflt,te_iflt,"__feq");
        sim.fneq = floating_function(2,te_int,te_iflt,te_iflt,"__fneq");
        sim.ffloat = floating_function(1,f32,te_i32,NULL,"__fflt");
        sim.ffloatu = floating_function(1,f32,te_u32,NULL,"__ffltu");
        sim.ffix = floating_function(1,te_i32,te_iflt,NULL,"__ffix");
        sim.ffixu = floating_function(1,te_u32,te_iflt,NULL,"__ffixu");

        sim.fnarrow = floating_function(1,f32,f64,NULL,"__d2f");
        sim.dwiden = floating_function(1,f64,f32,NULL,"__f2d");
#ifdef EXTENSION_FRAC
        sim.ffixr = floating_function(1,te_lfrac,te_iflt,NULL,"__ffixr");
        sim.dfixr = floating_function(1,te_lfrac,f64,NULL,"__dfixr");
        sim.dfloatr = floating_function(1,f64,te_lfrac,NULL,"__dfltr");
        sim.ffloatr = floating_function(1,f32,te_lfrac,NULL,"__ffltr");
#endif
    }
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
    sim.memcpyfn = library_function_t("_memcpy", te_ptr, 3, 3, 0L);
    sim.memsetfn = library_function_t("_memset", te_ptr, 3, 3, 0L);
    sim.realmemcpyfn = library_function_t("memcpy", te_ptr, 3, 3, 0L);
    sim.realmemsetfn = library_function_t("memset", te_ptr, 3, 3, 0L);

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
#if defined(TARGET_IS_ARM) && !defined(OBSOLETE_ARM_NAMES)
    stackoverflow  = sym_insert_id("__rt_stkovf_split_small");
    stack1overflow = sym_insert_id("__rt_stkovf_split_big");
#else
    stackoverflow = sym_insert_id("x$stack_overflow");
    stack1overflow = sym_insert_id("x$stack_overflow_1");
#endif
#endif
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
#ifndef TARGET_ASM_NAMES_LITERALS
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
                bitofstg_(s_static),       /* removed oddity: |u_constdata */
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
#ifdef TARGET_IS_XAP_OR_NEC
                bitofstg_(s_static)|u_bss,
#else
                bitofstg_(s_static),
#endif
                te_int);
#endif
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
#ifdef TARGET_IS_XAP_OR_NEC
    zvdatasegment = global_mk_binder(0,
                sym_insert_id("__zdataseg"),
                bitofstg_(s_static)|u_zeropage,
                te_int);
    zcdatasegment = global_mk_binder(0,
                sym_insert_id("__zconstdata"),
                bitofstg_(s_static)|u_constdata|u_zeropage,
                te_int);
#ifdef TARGET_HAS_BSS
    zbsssegment = global_mk_binder(0,
                sym_insert_id("__zbssseg"),
                bitofstg_(s_static)|u_zeropage|u_bss,
                te_int);
#endif
#endif
#ifdef TARGET_HAS_NEC_SECTS
    tidatasegment = global_mk_binder(0,
                sym_insert_id("__tidataseg"),
                bitofstg_(s_static)|u_zeropage|u_zeropage2,
                te_int);
    sidatasegment = global_mk_binder(0,
                sym_insert_id("__sidataseg"),
                bitofstg_(s_static)|u_zeropage2,
                te_int);
    sedatasegment = global_mk_binder(0,
                sym_insert_id("__sedataseg"),
                bitofstg_(s_static)|u_zeropage2,
                te_int);
    sebsssegment = global_mk_binder(0,
                sym_insert_id("__sebssseg"),
                bitofstg_(s_static)|u_zeropage2|u_bss,
                te_int);
#endif
#ifdef TARGET_HAS_C4P_SECTS
    idatasegment = global_mk_binder(0,
                sym_insert_id("__idataseg"),
                bitofstg_(s_static)|u_immpage2,
                te_int);
    iconstsegment = global_mk_binder(0,
                sym_insert_id("__iconstdata"),
                bitofstg_(s_static)|u_immpage2|u_constdata,
                te_int);
    ibsssegment = global_mk_binder(0,
                sym_insert_id("__ibssseg"),
                bitofstg_(s_static)|u_immpage2|u_bss,
                te_int);
#endif
#endif /* TARGET_ASM_NAMES_LITERALS */
#ifdef CPLUSPLUS
    thissym = sym_insert_id("___this");         /* CPLUSPLUS            */
/* The next 2 lines have these exact names to match [ES] and overload.c */
    ctorsym = sym_insert_id("__ct");            /* CPLUSPLUS            */
    dtorsym = sym_insert_id("__dt");            /* CPLUSPLUS            */
    vtabsym = sym_insert_id("__VTABLE");        /* CPLUSPLUS            */
/* Maybe we need __vtp (pointer member) and __vt (static table?)        */
#define te_size_t te_uint
/* Arguable we should just parse appropriate strings instead of this.   */
  { TypeExpr *Pv = ptrtotype_(te_void);
    TypeExpr *FPv_v = te_fntype(te_void,Pv,0,0,0);
    sim.xnew = bindsym_(toplevel_function("__nw",
        te_fntype(Pv, te_size_t, 0, 0, 0)));
    sim.xdel = bindsym_(toplevel_function("__dl",
        te_fntype(te_void, Pv, 0, 0, 0)));
    sim.xnewvec = bindsym_(toplevel_function("__nw_v",
        te_fntype(Pv, Pv, te_size_t, te_size_t, ptrtotype_(FPv_v))));
    sim.xdelvec = bindsym_(toplevel_function("__dl_v",
        te_fntype(te_void, Pv, te_size_t, ptrtotype_(FPv_v), 0)));
  }
#endif
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
}

/* end of builtin.c */
