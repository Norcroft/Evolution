
/* C compiler file mips/target.h :  Copyright (C) Codemist Ltd., 1990-2000. */
/* version 9 */

#ifndef _mipstarget_LOADED
#define _mipstarget_LOADED 1

extern int32 mips_opt;
#define mips_opt_zh(n) (mips_opt & (1L << (n)))
#define mips_opt_zhc(n) (mips_opt & (1L << 10+(n)))

#define TARGET_IS_MIPS 1
#ifdef __dec
#  define TARGET_IS_ULTRIX
#  define TARGET_MACHINE "MIPS R2000"
#  define TARGET_PREDEFINES { "__mips__", "__CLK_TCK=1000", "__JMP_BUF_SIZE=22" }
#else
#ifndef TARGET_MACHINE
#  define TARGET_MACHINE "MIPS R3000"
#endif
#ifdef TARGET_IS_SGI
#define TARGET_PREDEFINES { "__mips__", "__CLK_TCK=1000000", "__JMP_BUF_SIZE=27" }
#else
#define TARGET_PREDEFINES { "__mips__", "__CLK_TCK=1000", "__JMP_BUF_SIZE=27" }
#endif
#endif

#undef TARGET_PREDEFINES
/* For native MIPS library */
#define TARGET_PREDEFINES { "__mips__", \
    "_LANGUAGE_C", "_MIPS_SZINT=32", "_MIPS_SZLONG=32", "_MIPS_SZPTR=32" }

#ifndef TARGET_IS_LITTLE_ENDIAN
#  ifndef TARGET_IS_LITTLE_ENDIAN
#    define TARGET_ENDIANNESS_CONFIGURABLE 1
#  endif
#endif

#ifdef TARGET_HAS_COFF
#  undef TARGET_HAS_DEBUGGER
#  ifndef target_coff_magic 
#    define target_coff_magic (target_lsbytefirst ? 0x0162 : 0x0160)
                              /* inspected if TARGET_HAS_COFF */
#  endif
#endif

#define SOFTWARE_FLOATING_POINT 1
#define software_floating_point_enabled (mips_opt & 1)
#define SOFTWARE_FLOATING_POINT_RETURNS_DOUBLES_IN_REGISTERS 1
#define TARGET_HAS_SCCK   (mips_opt & 1)

/* #define TARGET_ASM_NAMES_LITERALS 1 */
/* #define TARGET_PASS_FLOAT_AS_FLOAT 1 */

#define CONST_DATA_IN_CODE 1
#define TARGET_HAS_BSS 1
#define BSS_THRESHOLD (mips_opt & 1 ? 100 : 0)

#define TARGET_HAS_HALFWORD_INSTRUCTIONS 1   /* for MIPS16              */
#define TARGET_HAS_IEEE 1
#define TARGET_HAS_MULTIPLY 1
#define TARGET_HAS_DIVIDE   1
#define TARGET_HAS_TAILCALL 1
#define TARGET_LINKER_OMITS_DOLLAR 1
/* #define TARGET_LACKS_MULDIV_LITERALS 1    / * there's always a temp  */
#define TARGET_HAS_SIGN_EXTEND 1
#define TARGET_LACKS_UNSIGNED_FIX 1
#define TARGET_LACKS_3WAY_COMPARE 1
#define TARGET_HAS_SWITCH_BRANCHTABLE 1
#define TARGET_CORRUPTS_SWITCH_REGISTER 1
#define TARGET_HAS_CONST_R_ZERO 1
/* #define TARGET_HAS_2ADDRESS_CODE 1 */
#ifdef TARGET_IS_ULTRIX
# define TARGET_HAS_OTHER_IEEE_ORDER 1
#endif
#define TARGET_FP_ARGS_IN_FP_REGS 1
#define TARGET_LACKS_RR_MEMREF 1
#define TARGET_LDRK_MIN (mips_opt & 1 ? 0L : -0x8000L)
#define TARGET_LDRK_MAX (mips_opt & 1 ? 4*31L+3L : 0x7fffL)
#define TARGET_LDRWK_MIN (mips_opt & 1 ? 0L : -0x8000L)
#define TARGET_LDRWK_MAX (mips_opt & 1 ? 2*31L+1L : 0x7fffL)
#define TARGET_LDRBK_MIN (mips_opt & 1 ? 0L : -0x8000L)
#define TARGET_LDRBK_MAX (mips_opt & 1 ? 1*31L+0L : 0x7fffL)

#define TARGET_PREFER_CMPEQZ 1

#define NO_INSTORE_FILES 1

/* *** These numbers are virtual registers *** */
#define R_ZERO  (mips_opt & 1 ? -1 : 0) /* Always zero */
#define R_IP   1 /* universal temporary on MIPS1 */
#define R_IP2  6 /* some MIPS16 ops need to use $2 (nb log.reg.6 here)  */
#define R_LR  31 /* Link register */
#define R_A1  2L
#define R_T1  6L         /* 6-7 on MIPS16, 6-17 on MIPS-1               */
#define R_A1result (mips_opt_zhc(1) ? R_A1:6L)  /* -zhc1 uses A1 for return */
#define R_V1  18L
#define NARGREGS 4L      /* 2-5 */
#define NVARREGS  (mips_opt & 1 ? 2L : 8L)      /* 18-19 or 18-26 */
#define NTEMPREGS (mips_opt & 1 ? 2L : 12L)     /* 6-7 on MIPS16, else 6-17 */
#define NINTREGS 32L

#define NFLTREGS 32L
#define R_FA1result 32L
#define R_FT1 32L
#define NFLTTEMPREGS 7L                 /* f0-f10, f16 */
#define R_FIP 39L                       /* f18 */
#define R_FA1 40L
#define NFLTARGREGS 2L
#define R_FV1  42L
#define NFLTVARREGS 6L

#define MAXGLOBINTREG 8L               /* OK? */
#define MAXGLOBFLTREG 4L               /* OK? */

#define ALLOCATION_ORDER { 2, 3, 4, 5,      /* args */   \
      6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,/*tmp*/ \
      18, 19, 20, 21, 22, 23, 24, 25,            /*var*/ \
      40, 41,                               /* args */   \
      32, 33, 34, 35, 36, 37, 38,  /* temps, not 39 */   \
      42, 43, 44, 45, 46, 47,               /* vars */   \
      255}
/* The next line is for MIPS16. */
#define ALLOCATION_ORDER2 { 2, 3, 4, 5,     /* args */   \
      6, 7,                       /* result reg/tmp */   \
      18, 19,                          /* var 16,17 */   \
      8, 9, 10, 11, 12, 13, 14, 15,            /*tmp*/   \
      20, 21, 22, 23, 24, 25,                  /*var*/   \
      40, 41,                               /* args */   \
      32, 33, 34, 35, 36, 37, 38,  /* temps, not 39 */   \
      42, 43, 44, 45, 46, 47,               /* vars */   \
      255}

#define R_HILO  64 /* Fake for mult/div */
#define R_FCC   65 /* fake for fpu cond. code */

#define sizeof_int      4
#define alignof_int     4
#define alignof_double  8
/* #define alignof_struct  1 */
#define alignof_struct  4       /* for MIPS Copenhagen */
/* #define NARROW_BITFIELDS 1   / * allow char/short/enum too */

#endif

/* end of mips/target.h */
