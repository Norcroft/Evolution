
/* C compiler file c4p/ops.h :  Copyright (C) Codemist Ltd., 1994-2004. */
/* version 3 */

#ifndef _c4pops_LOADED
#define _c4pops_LOADED 1

/* C4P opcodes */

#define OP_LD_Br	(0<<11)
#define OP_LD_Bs	(1<<11)
#define OP_LD_Wr	(2<<11)
#define OP_LD_Ws	(3<<11)
#define OP_LD_Lr	(4<<11)
#define OP_LD_Ls	(5<<11)
#define OP_ST_Br	(6<<11)
#define OP_ST_Bs	(7<<11)
#define OP_ST_Wr	(8<<11)
#define OP_ST_Ws	(9<<11)
#define OP_ST_Lr	(10<<11)
#define OP_ST_Ls	(11<<11)
#define OP_MOVI_S	(12<<11)
#define OP_MOVI_L	(13<<11)
#define OP_JUMP_S	(14<<11)
#define OP_CALL_S	(15<<11)
#define OP_JUMP_L	(16<<11)
#define OP_CALL_L	(17<<11)

#define OP_ADDr		(18<<11)
#define OP_SUBr		(19<<11)
#define OP_ADCr		(20<<11)
#define OP_SBCr		(21<<11)
#define OP_CMPr		(22<<11)
#define OP_ADDi		(18<<11 | 1)
#define OP_SUBi		(19<<11 | 1)
#define OP_ADCi		(20<<11 | 1)
#define OP_SBCi		(21<<11 | 1)
#define OP_CMPi		(22<<11 | 1)

#define OP_SHIFT	(23<<11)
# define OP_SHLI	(23<<11 | 0x08)
# define OP_SHRI	(23<<11 | 0x0e)
# define OP_SHL		(23<<11 | 0x10)
# define OP_SHR		(23<<11 | 0x16)
/* Xin only */
# define OP_SHLI24	(23<<11 | 0x38)
# define OP_SHRI24	(23<<11 | 0x3e)
# define OP_SHLI16	(23<<11 | 0x18)
# define OP_SHRI16	(23<<11 | 0x1e)

#define OP_MUL		(26<<11 | 0<<8)
#define OP_MACC		(26<<11 | 1<<8)
#define OP_MULF		(26<<11 | 2<<8)
#define OP_MACCF	(26<<11 | 3<<8)
#define OP_MULX		(26<<11 | 4<<8)
#define OP_MACCX	(26<<11 | 5<<8)

#define OP_ORr          (27<<11 | 0)
#define OP_ANDr         (27<<11 | 1)
#define OP_XORr         (27<<11 | 2)
#define OP_TSTr         (27<<11 | 3)
#define OP_MOVr         (27<<11 | 7)
#define OP_PUSH         (27<<11 | 0x05)
#define OP_POP          (27<<11 | 0x25)
#define OP_MOVI_D       (27<<11 | 0xa5)
#define OP_INT          (27<<11 | 0xe5)

#define OP_NOP		0xf800
#define OP_RET		0xf801
#define OP_RETI		0xf802
#define OP_EI		0xf803
#define OP_DI		0xf804
#define OP_MOV2SP	0xf807
#define OP_MOVSP	0xf808
#define OP_MOV2PC	0xf809
#define OP_MOVPC	0xf80a
#define OP_WAITM	0xf80b
#define OP_ROUND	0xf810	/* f810..f81f */

/* condition codes */

#define CC_C 0
#define CC_Z 1
#define CC_N 2
#define CC_V 3
#define CC_NZ 4		/* yes, this really is 4 and CC_C really is 0.  */
#define CC_NC 5
#define CC_NN 6
#define CC_AL 7

/* register fields */
#define RD(x)		((x)<<8)
#define RS(x)		((x)<<5)

/* register names                                                        */
#define M_ARGREGS (regbit(R_A1+NARGREGS)-regbit(R_A1))
#define M_VARREGS (regbit(R_V1+NVARREGS)-regbit(R_V1))

/* relocation types */
#define LABREF_DISP9	0x80000000
#define LABREF_DISP24	0x90000000

#endif

/* end of c4p/ops.h */
