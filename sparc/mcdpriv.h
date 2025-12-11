/*
 * C compiler file mcdpriv.h :  Copyright (C) Codemist Ltd., 1991.
 * (Private interfaces within machine-dependent back end).
 * version 2.
 */

#ifndef __mcdpriv_h
#define __mcdpriv_h 1

#ifdef TARGET_HAS_DEBUGGER

  extern int32 local_fpaddress(int32 p);
  /* p is the bindaddr field from a binder for a local object.  Returns the
   * offset of the object from the fp (assuming that fp and sp have not been
   * split in the frame containing the object ...).
   */

  extern void dbg_writedebug(void);
  /* Call from the object formatter to the debug table generator to
   * cause tables to be output
   */

  extern void obj_writedebug(void *, size_t);

#endif

/* START OF PIPELINE ALLOCATOR */
/*
 * The following are used to pass on register user information to the
 * machine-dependent peepholer.  In due course one could imagine
 * all sorts of information about the hardware resources involved in
 * instructions being provided here.
 */

/* An instruction can read at most four registers (eg double-precision add),
   including one of the condition-code pseudo registers, and can write at
   most two.

   We adopt the following arbitrary conventions ...
   1. R_CC appears in the reg2_ field if it is used
   2. R_IP is occasionally 'added in' to the read registers. reg4_ is used
*/

#define R_MASK          0x7f
#define R_CC		64	/* A fake for condition codes */
#define R_FCC		65	/* A fake for float condition codes */
#define R_BUS1		66	/* A fake for memory access */
#define R_BUS2		67	/* A fake for memory access */
#define R_BUS3		68	/* A fake for memory access */
#define R_BUS4		69	/* A fake for memory access */

#define reads_(r1,r2,r3,r4) (((((((r1)<<7) | (r2))<<7) | (r3))<<7) | (r4))
#define writes_(r1,r2,op)   (((((r1)<<7) | (r2))<<14) | (op))

#define reg1_(a) (((a) >> 21) & R_MASK)
#define reg2_(a) (((a) >> 14) & R_MASK)
#define reg3_(a) (((a) >>  7) & R_MASK)
#define reg4_(a) ((a) & R_MASK)

#define opclass_(a) ((a)->uses.opclass)

/* the following may not all be required - check sometime
 *#define set_reg1_(a,r) a = ((a) & ~reads_(R_MASK,0,0,0)) | reads_((r),0,0,0)
 *#define set_reg2_(a,r) a = ((a) & ~reads_(0,R_MASK,0,0)) | reads_(0,(r),0,0)
 */
#define set_reg3_(a,r) a = ((a) & ~reads_(0,0,R_MASK,0)) | reads_(0,0,(r),0)
#define set_reg4_(a,r) a = ((a) & ~reads_(0,0,0,R_MASK)) | reads_(0,0,0,(r))

#define OP_NULL		0
#define OP_LOAD		1
#define OP_STORE	2
#define OP_FLOAT	4

/* the following are used in a BasicBlock.flags field */

#define END_DEAD     0x0001 /* block has no continuation eg COUNT data */
#define END_BXX	     0x0002 /* block is a BXX table entry */
#define END_DROP     0x0004 /* block drops through to next */
#define END_CALL     0x0008 /* block terminated by CALL, then drops through */
#define END_JMPL     0x0010 /* block terminated by JMPL, to anywhere */
#define END_BCC	     0x0020 /* block terminated by conditional branch to label */
#define END_BA	     0x0040 /* block terminated by unconditional branch to label */
#define END_NOOP     0x0080 /* block ends with no-op */
#define END_ANNUL    0x0100 /* block terminated by annulled branch to label */

/* END_NOOP may appear in combination with END_(CALL,JMPL,BCC,BA) */
/* END_ANNUL may appear in combination with END_(BCC,BA) */

#define END_TYPES	\
       (END_DEAD | END_BXX | END_DROP | END_CALL | END_JMPL | END_BCC | END_BA)


/* ******* Control of optimisation *********************** */

extern int32 sparc_opt;

#define OPT_NOSCHED	(0x01)
#define OPT_TRACE	(0x02)

#define NOSCHEDULE	(sparc_opt & OPT_NOSCHED)
#define ALLMSG		(sparc_opt & OPT_TRACE)

#endif
