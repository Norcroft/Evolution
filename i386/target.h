/* C compiler file i386/target.h */

#ifndef _i386target_LOADED
#define _i386target_LOADED 1

#define TARGET_ADDRESSES_UNSIGNED 1
/* #define TARGET_FLAGS_VA_CALLS 1 */  /* may need if call conv fails us */
#define TARGET_FP_LITS_FROM_MEMORY 1
#define TARGET_HAS_2ADDRESS_CODE 1
/* #define TARGET_HAS_BLOCKMOVE 1 */   /* try later if there's time */
#define TARGET_HAS_BSS 1
#define TARGET_HAS_BYTE_INSTRUCTIONS 1
/* #define TARGET_HAS_COND_EXEC 1 */   /* try later if there's time */
#define TARGET_HAS_DIVIDE 1
/* #define TARGET_HAS_DIVREM_FUNCTION 1 */ /* try later if there's time */
#define TARGET_HAS_ELF 1
#define TARGET_HAS_IEEE 1
#define TARGET_HAS_MULTIPLY 1
#define TARGET_HAS_ROTATE 1
/* #define TARGET_HAS_SCALED_{ADD,ADDRESSING,OPS} 1: try some of these */
#define TARGET_HAS_SIGN_EXTEND 1
#define TARGET_IS_I386 1
#define TARGET_IS_LITTLE_ENDIAN 1
#define TARGET_LACKS_MULDIV_LITERALS 1
#define TARGET_LACKS_UNSIGNED_FIX 1    /* I think so. cvt uint -> flt? */
#define TARGET_LDRFK_MAX 0x7FFFFFFFL
#define TARGET_LDRFK_MIN (-2147483647-1)
#define TARGET_LDRK_MAX 0x7FFFFFFFL
#define TARGET_LDRK_MIN (-2147483647-1)
#define TARGET_MACHINE "Intel i386"
#define TARGET_PREDEFINES {"__i386__","__CLK_TCK=1000000","__JMP_BUF_SIZE=24"}
#define TARGET_STACKS_LINK 1

#define NO_INSTORE_FILES 1	       /* not sure what this does */

/*
 * For a first approximation I'm going to say there are eight
 * integer GPRs and one floating one (because we can't use the
 * eight floating ones sensibly as orthogonal registers due to the
 * stack structure).
 *
 * Physical:   0   2   3   4   5   1   6   7   8
 *  Virtual:   0   1   2   3   4   5   6   7   8
 * S/w name:  eax ecx edx esi edi ebx ebp esp st0
 *            ret tmp tmp sav sav sav  fp  sp  F
 */

#define V_EAX 0
#define V_EBX 5
#define V_ECX 1
#define V_EDX 2
#define V_ESI 3
#define V_EDI 4
#define V_EBP 6
#define V_ESP 7

#define _v2p(v) ((v)== 0L? 0L: (v)== 1L? 2L: (v)== 2L? 3L: (v)== 3L? 4L: \
		 (v)== 4L? 5L: (v)== 5L? 1L: (v)== 6L? 6L: (v)== 7L? 7L: \
		 (v)== 8L? 8L: v2p(v))

#define byteable(r) ( 1 & (0x27 >> (r)) )

#define R_IP 2L			       /* edx - gotta be something */

#define R_A1 0L
#define NARGREGS 0L		       /* with any luck NARGREGS=0 will still
				        * let results be returned in a1=eax */
#define NARGREGSA 1L		       /* allow the register allocator to
				        * use R_A1 */
#define R_T1 1L
#define NTEMPREGS 2L		       /* ecx, edx */
#define R_V1 3L
#define NVARREGS 3L		       /* esi, edi, ebx */
#define NINTREGS 8L

#define R_FA1 8L
#define NFLTARGREGS 0L   	       /* see NARGREGS above for rationale */
#define R_FT1 8L		       /* st1 */
#define NFLTTEMPREGS 7L		       /* st1-st7 */
#define R_FV1 15L		       /* none at all */
#define NFLTVARREGS 0L		       /* none at all */
#define NFLTREGS 7L		       /* this might even last to the end */

#define MAXGLOBINTREG 2L               /* hope this doesn't bite me */
#define MAXGLOBFLTREG 0L               /* and this, even more so */

#define R_SP	7L
#define R_F0	8L		       /* the lowest float reg */

#define ALLOCATION_ORDER { \
      0,			       /* result */	\
      1, 2,			       /* tmp */	\
      3, 4, 5,			       /* var */	\
      8, 9, 10, 11, 12, 13, 14,	       /* floating */	\
      255}

#endif /* _i386target_LOADED */

/* end of i386/target.h */
