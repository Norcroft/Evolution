
/* C compiler file sparc/ops.h :  Copyright (C) Codemist Ltd., 1991. */
/* version 2 */

#ifndef _sparcops_LOADED
#define _sparcops_LOADED 1

/* AM some lines commented out temporarily and duplicated in sparcspec.h */

/* sparc opcodes: */

/* format 1 instructions (op=01) */
#define OP_FORMAT_1(x) ((x)<<30)

#define OP_CALL		OP_FORMAT_1(1) /* 0x40000000L */

/* format 2 instructions (op=00) */
#define OP_FORMAT_2(op2) ((op2)<<22)

#define OP_UNIMP        OP_FORMAT_2(0) /*          0L */
#define OP_B		OP_FORMAT_2(2) /* 0x00800000L */
#define OP_SETHI	OP_FORMAT_2(4) /* 0x01000000L */
#define OP_BF		OP_FORMAT_2(6) /* 0x01800000L */

/* format 3 instructions (op=10 or op=11) */
#define OP_FORMAT_3(op,op3) (((op)<<30) | ((op3)<<19))

/* format 3 instructions (op=10) */
#define OP_FORMAT_3_10(op3) OP_FORMAT_3(2u,op3)

/* format 3 instructions (op=10, op3=0nnnnn) */
#define OP_ADD		OP_FORMAT_3_10(0x00) /* 0x80000000L */
#define OP_AND		OP_FORMAT_3_10(0x01) /* 0x80080000L */
#define OP_OR		OP_FORMAT_3_10(0x02) /* 0x80100000L */
#define OP_XOR		OP_FORMAT_3_10(0x03) /* 0x80180000L */
#define OP_SUB		OP_FORMAT_3_10(0x04) /* 0x80200000L */
#define OP_ANDN		OP_FORMAT_3_10(0x05) /* 0x80280000L */
#define OP_ORN		OP_FORMAT_3_10(0x06) /* 0x80300000L */
#define OP_XNOR		OP_FORMAT_3_10(0x07) /* 0x80380000L */
#define OP_ADDX		OP_FORMAT_3_10(0x08) /* 0x80400000L */
#define OP_SUBX		OP_FORMAT_3_10(0x0c) /* 0x80600000L */
#define OP_ADDCC	OP_FORMAT_3_10(0x10) /* 0x80800000L */
#define OP_ANDCC	OP_FORMAT_3_10(0x11) /* 0x80880000L */
#define OP_ORCC		OP_FORMAT_3_10(0x12) /* 0x80900000L */
#define OP_XORCC	OP_FORMAT_3_10(0x13) /* 0x80980000L */
#define OP_SUBCC	OP_FORMAT_3_10(0x14) /* 0x80a00000L */
#define OP_ANDNCC	OP_FORMAT_3_10(0x15) /* 0x80a80000L */
#define OP_ORNCC	OP_FORMAT_3_10(0x16) /* 0x80b00000L */
#define OP_XNORCC	OP_FORMAT_3_10(0x17) /* 0x80b80000L */
#define OP_ADDXCC	OP_FORMAT_3_10(0x18) /* 0x80c00000L */
#define OP_SUBXCC	OP_FORMAT_3_10(0x1c) /* 0x80e00000L */

/* format 3 instructions (op=10, op3=1nnnnn) */
#define OP_MULSCC	OP_FORMAT_3_10(0x24) /* 0x81200000L */
#define OP_SLL		OP_FORMAT_3_10(0x25) /* 0x81280000L */
#define OP_SRL		OP_FORMAT_3_10(0x26) /* 0x81300000L */
#define OP_SRA		OP_FORMAT_3_10(0x27) /* 0x81380000L */
#define OP_JMPL		OP_FORMAT_3_10(0x38) /* 0x81c00000L */
#define OP_TICC		OP_FORMAT_3_10(0x3a) /* 0x81c80000L */
#define OP_SAVE		OP_FORMAT_3_10(0x3c) /* 0x81e00000L */
#define OP_RESTORE	OP_FORMAT_3_10(0x3d) /* 0x81e80000L */

/* format 3 instructions (op=11, op3=0nnnnn) */
#define OP_FORMAT_3_11_0(op3) OP_FORMAT_3(3u,op3)

#define OP_LD		OP_FORMAT_3_11_0(0x00) /* 0xc0000000L */
#define OP_LDUB		OP_FORMAT_3_11_0(0x01) /* 0xc0080000L */
#define OP_LDUH		OP_FORMAT_3_11_0(0x02) /* 0xc0100000L */
#define OP_LDD		OP_FORMAT_3_11_0(0x03) /* 0xc0180000L */
#define OP_ST		OP_FORMAT_3_11_0(0x04) /* 0xc0200000L */
#define OP_STB		OP_FORMAT_3_11_0(0x05) /* 0xc0280000L */
#define OP_STH		OP_FORMAT_3_11_0(0x06) /* 0xc0300000L */
#define OP_STD		OP_FORMAT_3_11_0(0x07) /* 0xc0380000L */
#define OP_LDSB		OP_FORMAT_3_11_0(0x09) /* 0xc0480000L */
#define OP_LDSH		OP_FORMAT_3_11_0(0x0a) /* 0xc0500000L */
#define OP_LDSTUB	OP_FORMAT_3_11_0(0x0d) /* 0xc0680000L */
#define OP_SWAP		OP_FORMAT_3_11_0(0x0f) /* 0xc0780000L */

/* format 3 instructions (op=11, op3=1n0nnn) */
#define OP_FORMAT_3_11_1n0(op3) OP_FORMAT_3(3u,op3)

#define OP_LDF		OP_FORMAT_3_11_1n0(0x20) /* 0xc1000000L */
#define OP_LDDF		OP_FORMAT_3_11_1n0(0x23) /* 0xc1180000L */
#define OP_STF		OP_FORMAT_3_11_1n0(0x24) /* 0xc1200000L */
#define OP_STFSR	OP_FORMAT_3_11_1n0(0x25) /* 0xc1280000L */
#define OP_STDF		OP_FORMAT_3_11_1n0(0x27) /* 0xc1380000L */

/* FPop format 3 instructions (op=10, op3=110100) */
#define OP_FP_FORMAT_1(opf) (OP_FORMAT_3(2u,0x34) | ((opf)<<5))

#define OP_FMOVS	OP_FP_FORMAT_1(0x01) /* 0x81a00020L */
#define OP_FNEGS	OP_FP_FORMAT_1(0x05) /* 0x81a000a0L */
#define OP_FABSS	OP_FP_FORMAT_1(0x09) /* 0x81a00120L */
#define OP_FADDS	OP_FP_FORMAT_1(0x41) /* 0x81a00820L */
#define OP_FADDD	OP_FP_FORMAT_1(0x42) /* 0x81a00840L */
#define OP_FSUBS	OP_FP_FORMAT_1(0x45) /* 0x81a008a0L */
#define OP_FSUBD	OP_FP_FORMAT_1(0x46) /* 0x81a008c0L */
#define OP_FMULS	OP_FP_FORMAT_1(0x49) /* 0x81a00920L */
#define OP_FMULD	OP_FP_FORMAT_1(0x4a) /* 0x81a00940L */
#define OP_FDIVS	OP_FP_FORMAT_1(0x4d) /* 0x81a009a0L */
#define OP_FDIVD	OP_FP_FORMAT_1(0x4e) /* 0x81a009c0L */
#define OP_FITOS	OP_FP_FORMAT_1(0xc4) /* 0x81a01880L */
#define OP_FDTOS	OP_FP_FORMAT_1(0xc6) /* 0x81a018c0L */
#define OP_FITOD	OP_FP_FORMAT_1(0xc8) /* 0x81a01900L */
#define OP_FSTOD	OP_FP_FORMAT_1(0xc9) /* 0x81a01920L */
#define OP_FSTOI	OP_FP_FORMAT_1(0xd1) /* 0x81a01a20L */
#define OP_FDTOI	OP_FP_FORMAT_1(0xd2) /* 0x81a01a40L */

/* FPop format 3 instructions (op=10, op3=110101) */
#define OP_FP_FORMAT_2(opf) (OP_FORMAT_3(2u,0x35) | ((opf)<<5))

#define OP_FCMPS	OP_FP_FORMAT_2(0x51) /* 0x81a80a20L */
#define OP_FCMPD	OP_FP_FORMAT_2(0x52) /* 0x81a80a40L */


/* conditions for OP_B: */
#define C_AL    (0x8L<<25)
#define C_RGT   (0xaL<<25)
#define C_RGE	(0xbL<<25)
#define C_REQ   (0x1L<<25)
#define C_RLT   (0x3L<<25)
#define C_RLE   (0x2L<<25)
#define C_RNE   (0x9L<<25)
#define C_RGTU  (0xcL<<25)
#define C_RGEU  (0xdL<<25)
#define C_NC    (0xdL<<25)
#define C_RLTU  (0x5L<<25)
#define C_C     (0x5L<<25)
#define C_RLEU  (0x4L<<25)
#define C_V     (0x7L<<25)
#define C_NV    (0xfL<<25)
#define C_N     (0x6L<<25)
#define C_P     (0xeL<<25)

/* conditions for OP_BF: */
#define C_FAL	(0x8L<<25)
#define C_FRGT	(0x6L<<25)
#define C_FRGE	(0xbL<<25)
#define C_FREQ	(0x9L<<25)
#define C_FRLT	(0x4L<<25)
#define C_FRLE	(0xdL<<25)
#define C_FRNE	(0x1L<<25)

/* register fields */
#define F_RS1(r)  (((r)&0x1f)<<14L)     /* source for R-type      */
#define F_RS2(r)  ((r)&0x1f)		/* source for R-type      */
#define F_RD(r)   (((r)&0x1f)<<25)	/* destination for R-type */
#define A_IMM	0x00002000L
#define A_ANNUL	0x20000000L
#define F_SIMM13(x) (A_IMM | ((x)&0x1fff)) /* signed 13-bit immediate value */
#define F_IMM22(x)  ((x)&0x3fffff) /* 22-bit immediate value for SETHI */

#define D_RD(x,flt)  ((((x)>>25)&0x1f)+((flt)<<5))
#define D_RS1(x) (((x)>>14)&0x1f)
#define D_RS2(x) (((x)>>00)&0x1f)
#define D_SIMM13(x) (((int32)((x) << 19)) >> 19)

/* register names                                                        */
#define M_IP (regbit(R_IP))
#define M_ARGREGS (regbit(R_A1+NARGREGS)-regbit(R_A1))
#define M_VARREGS (regbit(R_V1+NVARREGS)-regbit(R_V1))
#define M_FARGREGS (regbit(R_F0+NFLTARGREGS-32)-regbit(R_F0-32))
#define M_FVARREGS \
        (regbit(R_F0+NFLTVARREGS+NFLTARGREGS-32)-regbit(R_F0+NFLTARGREGS-32))
#define M_FLTREGS  (regbit(R_F0+NFLTVARREGS+NFLTARGREGS-32)-regbit(R_F0-32))

/* relocation types */

#define LABREF_DISP22	0x80000000
#define LABREF_HI22	0x40000000
#define LABREF_LO10     0x20000000
#define LABREF_ABS32	0x10000000

/* define some common instructions */
#define OP_NOOP		(OP_SETHI | F_RD(R_ZERO) | 0)
#define OP_RET          (OP_JMPL | F_RD(R_ZERO) | F_RS1(R_LR) | F_SIMM13(8))
#define OP_RETL         (OP_JMPL | F_RD(R_ZERO) | F_RS1(R_LR1) | F_SIMM13(8))
#endif

/* end of sparc/ops.h */
