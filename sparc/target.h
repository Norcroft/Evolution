
/* C compiler file sparc/target.h :  Copyright (C) Codemist Ltd., 1991. */
/* version 4a */

#ifndef _sparctarget_LOADED
#define _sparctarget_LOADED 1

#ifdef TARGET_IS_DRS6000
#  define TARGET_HAS_ELF
#else
#endif
#define TARGET_IS_SPARC 1
#define TARGET_MACHINE "SPARC"
#ifdef TARGET_IS_DRS6000
#define TARGET_PREDEFINES {"__sparc", "__CLK_TCK=100", "__JMP_BUF_SIZE=22"}
#else
#define TARGET_PREDEFINES {"__sparc", "__CLK_TCK=1000", "__JMP_BUF_SIZE=22"}
#endif
#define TARGET_HAS_BSS 1

#if !(defined TARGET_HAS_AOUT || defined TARGET_HAS_COFF || \
      defined TARGET_HAS_ELF)
#  define TARGET_HAS_AOUT 1
#endif

#define TARGET_IS_BIG_ENDIAN       1

#undef TARGET_HAS_DEBUGGER

#  define target_coff_magic 0577     /* inspected if TARGET_HAS_COFF */
#define TARGET_HAS_IEEE 1
/*#define TARGET_HAS_MULTIPLY 1 */   /* but see also config_init in mcdep.c */
/*#define TARGET_HAS_DIVIDE   1 */
/*#define TARGET_HAS_TAILCALL 1 */
#define TARGET_LINKER_OMITS_DOLLAR 1
#define TARGET_LACKS_DIVIDE_LITERALS 1
#define TARGET_LACKS_UNSIGNED_FIX 1
#define TARGET_LACKS_3WAY_COMPARE 1
#define TARGET_LACKS_ROR 1
/*#define TARGET_HAS_SWITCH_BRANCHTABLE 1*/
#define TARGET_CORRUPTS_SWITCH_REGISTER 1
#define TARGET_HAS_BLOCKMOVE 1
/* #define TARGET_HAS_2ADDRESS_CODE 1 */
#define TARGET_LDRK_MAX (0xfffL)
#define TARGET_LDRK_MIN (-TARGET_LDRK_MAX-1)
#define TARGET_COUNT_IS_PROC 1
#define TARGET_HAS_PROFILE 1

#define NO_INSTORE_FILES


/* The real order is 0-7   Globals (zero + temp regs)
 *                   8-15  Outputs (Next set of arguments R_A1-R_A6,R_SP,temp)
 *                   16-23 Locals (VARREGS)
 *                   24-31 Inputs (Current arguments R_P1-R_P6, R_FP, R_LR)
 *
 * Floating point are 0, 2, 4, 6, .. 14 cos of doubles.  Call then n/2
 */


#define TARGET_HAS_CONST_R_ZERO 1       /* i.e. R_ZERO defined */
#define R_ZERO		0L	/* Always zero */
#define R_TM		1L	/* universal temporary  */
#define R_IP		7L	/* The odd temporary */
#define R_A1		8L	/* outgoing args after (caller's) roll */
/*#define R_A1result	R_A1	/ * incoming result after (caller's) roll */
#define R_P1		24L	/* incoming params after roll */
#define R_P1result	R_P1	/* outgoing result before rollback */
#define R_T1		2L
#define R_V1		16L
#define R_SP		14L
#define R_FP		30L
#define R_LR		31L
#define R_LR1		15L	/* R_LR before roll ! */
#define R_F0		32L
#define R_FV1		(R_F0+8L)
#define R_FTMP          62L     /* universal float temporary */
#define NARGREGS	6L
#define NVARREGS	8L
#define NTEMPREGS	5L
#define NINTREGS	32L     /* same as smallest fp reg, usually R_F0 */
#define NFLTARGREGS	15
#define NFLTVARREGS	0
#define MAXGLOBINTREG	8L
#define MAXGLOBFLTREG	4L

#define ALLOCATION_ORDER { 24, 25, 26, 27, 28, 29,          /* params in */   \
			   16, 17, 18, 19, 20, 21, 22, 23,  /* var       */   \
			    2,  3,  4,  5,  6,  7,          /* tmp       */   \
			    8,  9, 10, 11, 12, 13,          /* args out  */   \
                           32, 33, 34, 35, 36, 37, 38, 39,  /* flt arg   */   \
                           40, 41, 42, 43, 44, 45, 46,      /* flt var   */   \
			   255}


#define sizeof_int      4
#define alignof_int     4
#define alignof_double  8
#define alignof_max	8
#ifndef alignof_struct  /* allow for options.h to change */
#define alignof_struct  1
#endif

#endif

/* end of sparc/target.h */
