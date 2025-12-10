/*
 * C compiler file mcdpriv.h :  Copyright (C) Codemist Ltd., 1991-2000.
 * (Private interfaces within machine-dependent back end).
 * version 2
 */

#ifndef __mcdpriv_h
#define __mcdpriv_h 1

/* Many of these defs are local to mips/gen.c.  Move there?             */

#define LABREF_OFF16  0x00000000  /* 88000 addressing modes for forw. refs. */
#define LABREF_OFF26  0x01000000
#define LABREF_LIT32  0x02000000  /* or.h/l as part of 32 bit address   */
#define LABREF_LIT16B 0x03000000  /* addu offset for literal addresses  */

#define NONLEAF (PROC_ARGPUSH | PROC_ARGADDR | PROC_BIGSTACK | BLKCALL)
/* STACKCHECK (NONLEAF subset) defines when stack check is REALLY needed */
#define STACKCHECK (PROC_BIGSTACK | BLKCALL)

/*
 * The following are used to pass on register user information to the
 * machine-dependent peepholer.  In due course one could imagine
 * all sorts of information about the hardware resources involved in
 * instructions being provided here.
 */
#define reads_(r1,r2,r3,r4) (((r1)<<24) | ((r2)<<16) | ((r3)<<8) | (r4))
#define writes_(r1,r2,op) (((r1)<<24) | ((r2)<<16) | (op))

#define reg1_(a) (((a) >> 24) & 0xff)
#define reg2_(a) (((a) >> 16) & 0xff)
#define reg3_(a) (((a) >> 8) & 0xff)
#define reg4_(a) ((a) & 0xff)
#define opclass_(a) ((a) & 0xffff)

#define OP_NULL   0
#define OP_LOAD   1     /* loads from memory */
#define OP_STORE  2     /* stores to memory */
#define OP_BR16   4     /* does a branch (16 bit offset) */
#define OP_BR26   8     /* does a branch (26 bit offset) */
#define OP_JUMP   16    /* does a branch (register destination) */
#define OP_2WORD  32    /* load or store touches 2 registers */
#define OP_WORD   64    /* unknown behaviour! */

extern int32 wasted_in_save, wasted_in_restore;

#ifdef TARGET_HAS_DEBUGGER

#ifdef REL193_HACK
  extern int32 local_fpaddress(Binder const *b);
  /* p is a binder for a local object.  Returns the
   * offset of the object from the fp (assuming that fp and sp have not been
   * split in the frame containing the object ...).
   */
#else
  extern int32 local_fpaddress(int32 p);
  /* p is the bindaddr field from a binder for a local object.  Returns the
   * offset of the object from the fp (assuming that fp and sp have not been
   * split in the frame containing the object ...).
   */
#endif

  extern void dbg_writedebug(void);
  /* Call from the object formatter to the debug table generator to
   * cause tables to be output
   */

#ifdef REL193_HACK
  extern void obj_writedebug(void const *, int32);
#else
  extern void obj_writedebug(void *, size_t);
#endif

#endif

#endif
