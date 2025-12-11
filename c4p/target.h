
/* C compiler file c4p/target.h :  Copyright (C) Codemist Ltd., 1999.   */
/* version 2 */

#ifndef _c4ptarget_LOADED
#define _c4ptarget_LOADED 1

#define TARGET_IS_NEC 1            /* @@@ remove this soon! */
#define TARGET_IS_C4P 1
#define TARGET_IS_XAP_OR_NEC 1
#define TARGET_MACHINE "C4P"
#define TARGET_PREDEFINES {"__c4p", "__CLK_TCK=1000000", "__JMP_BUF_SIZE=2"}

#define TARGET_IS_LITTLE_ENDIAN    1
#define TARGET_HAS_HARVARD_SEGS    1
#define TARGET_HAS_HALFWORD_INSTRUCTIONS 1
#define TARGET_NUM_DATASECTS       50
#define TARGET_HAS_C4P_SECTS       1
#define target_elf_prefix          "_"

/* #define TARGET_CALL_PUSHPOP_LIBRARY 1 */

#define TARGET_HAS_DEBUGGER 1

#define SOFTWARE_FLOATING_POINT 1
#define TARGET_FP_LITS_FROM_MEMORY 1
#define TARGET_HAS_IEEE 1
/* #define TARGET_HAS_MULTIPLY 1 (need subroutine for 32 bit version) */
/* #define TARGET_HAS_DIVIDE   1 (need subroutine for signed version) */

extern int nec850e;  /* configure wrt multiplication/division support */

#define TARGET_HAS_TAILCALL 1
/* #define TARGET_HAS_TAILCALLR 1             @@@ needs gen.c fix @@@ */
#define TARGET_LINKER_OMITS_DOLLAR 1
#define TARGET_LACKS_DIVIDE_LITERALS 1
/* #define TARGET_LACKS_UNSIGNED_FIX 1 */
#define TARGET_LACKS_3WAY_COMPARE 1
#define TARGET_LACKS_ROR 1
#define TARGET_PREFER_SIGNED_SHIFTS 1
#define TARGET_HAS_SIGN_EXTEND 1
#define TARGET_LACKS_RR_MEMREF 1
/*#define TARGET_HAS_SWITCH_BRANCHTABLE 1*/
#define TARGET_CORRUPTS_SWITCH_REGISTER 1
/*  #define TARGET_HAS_SCCK 1 */
/*  #define TARGET_HAS_BLOCKMOVE 1 */
#define TARGET_HAS_2ADDRESS_CODE 1
/* There aren't enough registers to exploit the following.              */
/* #define TARGET_LDRK_MAX (0x1ffL)             */
/* #define TARGET_LDRK_MIN (-TARGET_LDRK_MAX-1) */
#define TARGET_COUNT_IS_PROC 1
#define TARGET_HAS_PROFILE 1

#define NO_INSTORE_FILES 1

#define R_LR		0L	/* beware -- see avoidallocating */
#define R_IP		0L      /* beware -- see avoidallocating */
#define R_A1		1L
#define R_V1		4L
#define R_SP		7L
#define NARGREGS	3L
#define NVARREGS	3L
#define NINTREGS	8     /* same as smallest fp reg, usually R_F0 */
#define R_F0		8L
#define R_FV1		8L
#define NFLTARGREGS	0
#define NFLTVARREGS	0
#define MAXGLOBINTREG	0
#define MAXGLOBFLTREG	0

/* #define ALLOCATION_ORDER { 1,2,3,4,5,6,255 } */

#define sizeof_char     1
#define sizeof_int      4
#define sizeof_long     4
#define sizeof_ptr      4

/* defaults for alignof_xxx is sizeof_xxx. */
#define alignof_char    1
#define alignof_int     4
#define alignof_long    4
#define alignof_ptr     4
#define alignof_float   4
#define alignof_double  4
#define alignof_ldble   4

#define alignof_max	4
#define alignof_toplevel 1
#define alignof_stack   4

#define CONST_DATA_IN_CODE 1
#ifndef alignof_struct  /* allow for options.h to change */
#define alignof_struct  4
#endif

#endif

/* end of c4p/target.h */
