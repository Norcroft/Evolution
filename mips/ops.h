
/* C compiler file mips/ops.h :  Copyright (C) Codemist Ltd., 1991-2000. */
/* version 3 */

#ifndef _mipsops_LOADED
#define _mipsops_LOADED 1

/* AM some lines commented out temporarily and duplicated in mips/target.h */

/* MIPS R2000 opcodes: */

#define MASK_PRI        0xfc000000
#define MASK_COND       0x001f0000
#define MASK_SPECIAL    0x0000003f
#define MASK_COPZ       0x03c00000

#define OP_SPECIAL      0x00000000
#define  OP_SLL         0x00000000
#define  OP_SRL         0x00000002
#define  OP_SRA         0x00000003
#define  OP_SLLV        0x00000004
#define  OP_SRLV        0x00000006
#define  OP_SRAV        0x00000007
#define  OP_JR          0x00000008
#define  OP_JALR        0x00000009
#define  OP_SYSCALL     0x0000000c
#define  OP_BREAK       0x0000000d
#define  OP_MFHI        0x00000010
#define  OP_MTHI        0x00000011
#define  OP_MFLO        0x00000012
#define  OP_MTLO        0x00000013
#define  OP_MULT        0x00000018
#define  OP_MULTU       0x00000019
#define  OP_DIV         0x0000001a
#define  OP_DIVU        0x0000001b
#define  OP_ADD         0x00000020
#define  OP_ADDU        0x00000021
#define  OP_SUB         0x00000022
#define  OP_SUBU        0x00000023
#define  OP_AND         0x00000024
#define  OP_OR          0x00000025
#define  OP_XOR         0x00000026
#define  OP_NOR         0x00000027
#define  OP_SLT         0x0000002a
#define  OP_SLTU        0x0000002b

#define OP_BCOND        0x04000000
#define  OP_BLTZ        0x04000000
#define  OP_BGEZ        0x04010000
#define  OP_BLTZAL      0x04100000
#define  OP_BGEZAL      0x04110000

#define OP_J            0x08000000
#define OP_JAL          0x0c000000
#define OP_BEQ          0x10000000
#define OP_BNE          0x14000000
#define OP_BLEZ         0x18000000
#define OP_BGTZ         0x1c000000
#define OP_ADDI         0x20000000
#define OP_ADDIU        0x24000000
#define OP_SLTI         0x28000000
#define OP_SLTIU        0x2c000000
#define OP_ANDI         0x30000000
#define OP_ORI          0x34000000
#define OP_XORI         0x38000000
#define OP_LUI          0x3c000000

#define  OP_MFCZ        0x00000000
#define  OP_MTCZ        0x00800000
#define  OP_CFCZ        0x00400000
#define  OP_CTCZ        0x00c00000
#define  OP_BCZF        0x01000000
#define  OP_BCZT        0x01010000
#define  OP_TLBR        0x00000001
#define  OP_TLBWI       0x00000002
#define  OP_TLBWR       0x00000006
#define  OP_TLBP        0x00000009
#define  OP_RFE         0x00000010
 
#define OP_COP0         0x40000000
#define  OP_MFC0        0x40000000
#define  OP_MTC0        0x40800000
#define  OP_CFC0        0x40400000
#define  OP_CTC0        0x40c00000
#define  OP_BC0F        0x41000000
#define  OP_BC0T        0x41010000
/* #define  OP_TLBR     0x40000001 */
/* #define  OP_TLBWI    0x40000002 */
/* #define  OP_TLBWR    0x40000006 */
/* #define  OP_TLBP     0x40000009 */
/* #define  OP_RFE      0x40000010 */

#define OP_COP1         0x44000000
#define  OP_MFC1        0x44000000
#define  OP_MTC1        0x44800000
#define  OP_CFC1        0x44400000
#define  OP_CTC1        0x44c00000
#define  OP_BC1F        0x45000000
#define   OP_BC1T       0x45010000

#define  OP_ADD_S       0x46000000
#define  OP_SUB_S       0x46000001
#define  OP_MUL_S       0x46000002
#define  OP_DIV_S       0x46000003
#define  OP_ABS_S       0x46000005
#define  OP_MOV_S       0x46000006
#define  OP_NEG_S       0x46000007
#define  OP_CVT_S_S     0x46200020 /* This one does not really exist */
#define  OP_CVT_D_S     0x46000021
#define  OP_CVT_W_S     0x46000024
#define  OP_C_F_S       0x46000030
#define  OP_C_UN_S      0x46000031
#define  OP_C_EQ_S      0x46000032
#define  OP_C_UEQ_S     0x46000033
#define  OP_C_OLT_S     0x46000034
#define  OP_C_ULT_S     0x46000035
#define  OP_C_OLE_S     0x46000036
#define  OP_C_ULE_S     0x46000037
#define  OP_C_SF_S      0x46000038
#define  OP_C_NGLE_S    0x46000039
#define  OP_C_SEQ_S     0x4600003a
#define  OP_C_NGL_S     0x4600003b
#define  OP_C_LT_S      0x4600003c
#define  OP_C_NGE_S     0x4600003d
#define  OP_C_LE_S      0x4600003e
#define  OP_C_NGT_S     0x4600003f
#define  OP_ADD_D       0x46200000
#define  OP_SUB_D       0x46200001
#define  OP_MUL_D       0x46200002
#define  OP_DIV_D       0x46200003
#define  OP_ABS_D       0x46200005
#define  OP_MOV_D       0x46200006
#define  OP_NEG_D       0x46200007
#define  OP_CVT_S_D     0x46200020
#define  OP_CVT_W_D     0x46200024
#define  OP_C_F_D       0x46200030
#define  OP_C_UN_D      0x46200031
#define  OP_C_EQ_D      0x46200032
#define  OP_C_UEQ_D     0x46200033
#define  OP_C_OLT_D     0x46200034
#define  OP_C_ULT_D     0x46200035
#define  OP_C_OLE_D     0x46200036
#define  OP_C_ULE_D     0x46200037
#define  OP_C_SF_D      0x46200038
#define  OP_C_NGLE_D    0x46200039
#define  OP_C_SEQ_D     0x4620003a
#define  OP_C_NGL_D     0x4620003b
#define  OP_C_LT_D      0x4620003c
#define  OP_C_NGE_D     0x4620003d
#define  OP_C_LE_D      0x4620003e
#define  OP_C_NGT_D     0x4620003f

#define  OP_ADD_W       0x46800000 /* This one does not exist */
#define  OP_CVT_S_W     0x46800020
#define  OP_CVT_D_W     0x46800021
#define  OP_CVT_W_W     0x46800024 /* This one does not really exist */


#define OP_COP2  0x48000000
#define  OP_MFC2 0x48000000
#define  OP_MTC2 0x48800000
#define  OP_CFC2 0x48400000
#define  OP_CTC2 0x48c00000
#define  OP_BC2F 0x49000000
#define  OP_BC2T 0x49010000
#define OP_COP3  0x4c000000
#define  OP_MFC3 0x4c000000
#define  OP_MTC3 0x4c100000
#define  OP_CFC3 0x4c400000
#define  OP_CTC3 0x4cc00000
#define  OP_BC3F 0x4d000000
#define  OP_BC3T 0x4d010000

#define OP_LB           0x80000000
#define OP_LH           0x84000000
#define OP_LWL          0x88000000
#define OP_LW           0x8c000000
#define OP_LBU          0x90000000
#define OP_LHU          0x94000000
#define OP_LWR          0x98000000

#define OP_SB           0xa0000000
#define OP_SH           0xa4000000
#define OP_SWL          0xa8000000
#define OP_SW           0xac000000

#define OP_SWR          0xb8000000

#define OP_LWC0         0xc0000000
#define OP_LWC1         0xc4000000
#define OP_LWC2         0xc8000000
#define OP_LWC3         0xcc000000

#define OP_SWC0         0xe0000000
#define OP_SWC1         0xe4000000
#define OP_SWC2         0xe8000000
#define OP_SWC3         0xec000000

#define OP_pseudoADDIU_PC   0xf0000000     /* AM MIPS16 hack */
#define OP_pseudoLW_PC      0xf4000000     /* AM MIPS16 hack */

#define OP_NOOP         OP_OR

/* MIPS16 opcodes */

#define MASK16_PRI        0xf800

#define OP16_ADDIU_SP     0x0000  /* R3 = SP op u8<<2    */
#define OP16_ADDIU_PC     0x0800  /* R3 = (PC op u8<<2) & ~3 */
#define OP16_B            0x1000  /* PC += s11<<1     */
#define OP16_JAL          0x1800  /* PC = [PCHI:imm26] */
#define   OP16_JALX         0x1c00  /* PC = [PCHI:imm26] */
#define OP16_BEQZ         0x2000  /* if (R3) PC += s11<<1 */
#define OP16_BNEZ         0x2800  /* if (R3) PC += s11<<1 */

#define OP16_SHIFTform    0x3000  /* R3 = R3 << u3 */
#define   OP16_SLL          0x3000  /* R3 = R3 << u3 */
#define   OP16_DSLL         0x3001  /* R3 = R3 << u3 */
#define   OP16_SRL          0x3002  /* R3 = R3 << u3 */
#define   OP16_SRA          0x3003  /* R3 = R3 << u3 */

#define OP16_LD           0x3800  /* R3 =64 u5(R3) */
#define OP16_ADDIU3       0x4000  /* R3 = R3 op s4 */
#define OP16_DADDIU3      0x4010  /* R3 = R3 op s4 */
#define OP16_ADDIU2       0x4800  /* R3 op= s8     */
#define OP16_SLTI         0x5000  /* T = R3 op u8  */
#define OP16_SLTIU        0x5800  /* T = R3 op u8  */

#define OP16_I8form       0x6000
#define   OP16_BTEQZ        0x6000  /* if (T) PC += s8<<1 */
#define   OP16_BTNEZ        0x6100  /* if (T) PC += s8<<1 */
#define   OP16_SW_SP_LR     0x6200  /* u8<<2(SP) = LR */
#define   OP16_ADJSP        0x6300  /* SP += s8<<3 */
#define   OP16_RESTORE      0x6400  /* i7 */
#define   OP16_SAVE         0x6480  /* i7 */
#define   OP16_MOV32R       0x6500  /* R5 = R3 */
#define   OP16_spare66      0x6600
#define   OP16_MOVR32       0x6700  /* R3 = R5 */
#define   OP16_NOP          0x6500

#define OP16_LI           0x6800  /* R3 = u8 (ext=u16) */
#define OP16_CMPI         0x7000  /* T = R3 op u8 (ext=u16) */

#define OP16_SD           0x7800  /* u5(R3) =64 R3 */
#define OP16_LB           0x8000  /* R3 = u5(R3) */
#define OP16_LH           0x8800  /* R3 = u5(R3) */
#define OP16_LW_SP        0x9000  /* R3 = u8<<2(SP) */
#define OP16_LW           0x9800  /* R3 = u5(R3) */
#define OP16_LBU          0xa000  /* R3 = u5(R3) */
#define OP16_LHU          0xa800  /* R3 = u5(R3) */
#define OP16_LW_PC        0xb000  /* R3 = u8<<2(PC) */
#define OP16_LWU          0xb800  /* R3 =64 u5(R3) */
#define OP16_SB           0xc000  /* u5(R3) = R3 */
#define OP16_SH           0xc800  /* u5(R3) = R3 */
#define OP16_SW_SP        0xd000  /* u8<<2(SP) = R3 */
#define OP16_SW           0xd800  /* u5(R3) = R3 */

#define OP16_RRRform      0xe000
#define   OP16_DADDU        0xe000  /* R3 = R3 op R3 */
#define   OP16_ADDU         0xe001  /* R3 = R3 op R3 */
#define   OP16_DSUBU        0xe002  /* R3 = R3 op R3 */
#define   OP16_SUBU         0xe003  /* R3 = R3 op R3 */

#define OP16_RRform       0xe800
#define   OP16_JR           0xe800  /* PC = R3 */
#define   OP16_JR_LR        0xe820  /* PC = LR */
#define   OP16_JALR         0xe840  /* PC = R3 */
#define   OP16_JALR_LR      0xe860  /* PC = LR */
#define   OP16_JRC          0xe880  /* PC = R3 */
#define   OP16_JRC_LR       0xe8a0  /* PC = LR */
#define   OP16_JALRC        0xe8c0  /* PC = R3 */
#define   OP16_JALRC_LR     0xe8e0  /* PC = LR */

#define OP16_SDBBP        0xe801  /* u6 */
#define OP16_SLT          0xe802  /* T = R3 op R3 */
#define OP16_SLTU         0xe803  /* T = R3 op R3 */
#define OP16_SLLV         0xe804  /* R3 op= R3 */
#define OP16_BREAK        0xe805  /* breakpt */
#define OP16_SRLV         0xe806  /* R3 op= R3 */
#define OP16_SRAV         0xe807  /* R3 op= R3 */
#define OP16_DSRL         0xe808  /* R3 >>= u3 */
#define OP16_CMP          0xe80a  /* T = R3 op R3 */
#define OP16_NEG          0xe80b  /* R3 op= R3 */
#define OP16_AND          0xe80c  /* R3 op= R3 */
#define OP16_OR           0xe80d  /* R3 op= R3 */
#define OP16_XOR          0xe80e  /* R3 op= R3 */
#define OP16_NOT          0xe80f  /* R3 op= R3 */

#define OP16_MFHI         0xe810  /* R3 = HI */
#define   OP16_ZEB          0xe811  /* R3 */
#define   OP16_ZEH          0xe831  /* R3 */
#define   OP16_ZEW          0xe851  /* R3 */
#define   OP16_SEB          0xe891  /* R3 */
#define   OP16_SEH          0xe8b1  /* R3 */
#define   OP16_SEW          0xe8d1  /* R3 */
#define OP16_MFLO         0xe812  /* R3 = LO */
#define OP16_DSRA         0xe813  /* R3 >>= u3 */
#define OP16_DSLLV        0xe814  /* R3 op= R3 */
#define OP16_DSRLV        0xe816  /* R3 op= R3 */
#define OP16_DSRAV        0xe817  /* R3 op= R3 */
#define OP16_MULT         0xe818  /* HI,LO = R3 op R3 */
#define OP16_MULTU        0xe819  /* HI,LO = R3 op R3 */
#define OP16_DIV          0xe81a  /* HI,LO = R3 op R3 */
#define OP16_DIVU         0xe81b  /* HI,LO = R3 op R3 */
#define OP16_DMULT        0xe81c  /* HI,LO = R3 op R3 */
#define OP16_DMULTU       0xe81d  /* HI,LO = R3 op R3 */
#define OP16_DDIV         0xe81e  /* HI,LO = R3 op R3 */
#define OP16_DDIVU        0xe81f  /* HI,LO = R3 op R3 */

#define OP16_EXT          0xf000  /* extend */

#define OP16_MIPS64form   0xf800  /* mips64 */
#define   OP16_LD_PC        0xf000  /* R3 =64 u5<<3(PC) */
#define   OP16_SD_SP        0xf100  /* u8<<2(SP) =64 R3 */
#define   OP16_SD_SP_LR     0xf200  /* u8<<2(SP) =64 LR */
#define   OP16_DADJSP       0xf300  /* SP +=64 s8    */
#define   OP16_LD_SP        0xf400  /* R3 =64 u8<<2(SP) */
#define   OP16_DADDIU2      0xf500  /* R3 op=64 s5   */
#define   OP16_DADDIU_PC    0xf600  /* R3 =64 (PC op u5<<2) & ~3 */
#define   OP16_DADDIU_SP    0xf700  /* R3 =64 SP op u5<<2  */

/* MIPS16 SAVE/RESTORE bits */
#define SR_S1 0x10
#define SR_S0 0x20
#define SR_RA 0x40
#define SR_SAVE 0x80
    
/* conditions for OP_B: */
#define C_AL    0L
#define C_RGT   0x1L
#define C_RGE   0x2L
#define C_REQ   0x3L
#define C_RLT   0x4L
#define C_RLE   0x5L
#define C_RNE   0x6L
#define C_RGTU  0x7L
#define C_RGEU  0x8L
#  define C_NC    0x8L
#define C_RLTU  0x9L
#  define C_C     0x9L
#define C_RLEU  0xaL
#define C_V     0xbL
#define C_NV    0xcL
#define C_N     0xdL
#define C_NN    0xeL
#define C_FN    0xfL


/* turn J_OPCODE single/double bit to MIPS one: */
#define F_SINGLE 0x0000L
#define F_DOUBLE 0x0200L
#define F_FLTOFJ(x) (J_double(x) ? F_DOUBLE : F_SINGLE)

/* register fields (oriented around 32 bit units)      */
#define F_RS(r)  (v2p(r)<<21L)  /* source register      */
#define F_RD(r)  (v2p(r)<<11L)  /* destination register */
#define F_RT(r)  (v2p(r)<<16L)  /* target register */

#define F_FS(r)  (v2p(r)<<11L)  /* floating source register      */
#define F_FD(r)  (v2p(r)<<6L)   /* floating destination register */
#define F_FT(r)  (v2p(r)<<16L)  /* floating target register */

#define SHAMT(n) ((n&0x1f)<<6L)

/* opcode field extraction: */
#define s16_(x)   ((((x) & 0xffff) ^ 0x8000) - 0x8000)
#define s15_(x)   ((((x) & 0x7fff) ^ 0x4000) - 0x4000)
#define s11_(x)   ((((x) & 0x07ff) ^ 0x0400) - 0x0400)
#define s8_(x)   ((((x) & 0xff) ^ 0x80) - 0x80)
#define s4_(x)   ((((x) & 0xf) ^ 0x8) - 0x8)
#define u16_(x)   ((x) & 0xffff)
#define u8_(x)   ((x) & 0xff)
#define u5_(x)   ((x) & 0x1f)

#define D_RD(x) (((x)>>11)&0x1f)
#define D_RS(x) (((x)>>21)&0x1f)
#define D_RT(x) (((x)>>16)&0x1f)
#define D_SHAMT(x) (((x)>>6)&0x1f)

/* register names                                                        */

#define R_SP    0x1dL     /* main stack pointer                          */

/* Calling a function can disturb R_A1 to R_A4, R_IP,      but must      */
/* preserve R_V1 to R_V6, R_FP and R_SP.                                 */
/* R_SL may one day be a stack limit value                               */

#define M_VARREGS (regbit(R_V1+NVARREGS)-regbit(R_V1))

#endif

/* end of mips/ops.h */
