#ifndef NCC_I386_GEN_H
#define NCC_I386_GEN_H

#include "target.h"

typedef struct i386ins i386ins;

typedef struct dependency dependency;  /* defined in, and used by, isched.c */

struct i386ins {
    i386ins *next, *prev;	       /* doubly linked list */
    int peepflags;		       /* for peepholing */
    char text[160];		       /* the text of the instruction */
    int readreg[NINTREGS];	       /* which regs do we read? */
    int writereg[NINTREGS];	       /* which regs do we write? */
    int agi_deps;		       /* do we have AGI-prone dependents? */
    i386ins *nextsch, *nextsch2;       /* isched: used internally */
    dependency *deps;		       /* isched: insns depending on this */
    int udeps;			       /* isched: no of unissued depends */
    i386ins *scheduled;		       /* the insn scheduled in this slot */
};

#define PEEP_NECESSARY  0x0001	       /* insn can't be discarded */
#define PEEP_DISCARDED  0x0002	       /* insn has been discarded */

#define PEEP_FPU 	0x0004	       /* insn is a floating-point one */
#define PEEP_UPAIR	0x0008	       /* can pair in U pipe */
#define PEEP_VPAIR	0x0010	       /* can pair in V pipe */
#define PEEP_RDMEM	0x0020	       /* reads memory-held data */
#define PEEP_WRMEM	0x0040	       /* reads memory-held data */
#define PEEP_STACK	0x0080	       /* accesses stack */
#define PEEP_ESPI	0x0100	       /* implicit use of ESP. Can AGI ... */
#define PEEP_ESPX	0x0200	       /* ... due to explicit write of ESP. */
#define PEEP_NONINSN	0x1000	       /* not an instruction at all */
#define PEEP_NONBB	0x2000	       /* can't be part of a basic block */

extern i386ins *funhead, *funtail;

extern i386ins i386_nullins;
#define NO_INSN (&i386_nullins)

extern int uncond_align;	       /* alignment for unconditional jumps */
extern int cond_align;		       /* alignment for conditional jumps */
extern int peep_mmx;		       /* are we scheduling for MMX or not? */
extern int peep_slave;		       /* do we do register slaving? */
extern int peep_const;		       /* do we do constant optimisation? */
extern int peep_sched;		       /* do we do Pentium scheduling? */

extern void pentium_schedule(void);

#endif
