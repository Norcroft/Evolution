/*
 * i386/isched.c   Pentium instruction scheduler for ncc/i386
 */

#include <stdio.h>
#ifdef __STDC__
#  include <string.h>
#else
#  include <strings.h>
#endif
#include <stdlib.h>
#include "gen.h"

typedef struct p5state {
    int agi_regs;		       /* regs written in previous cycle */
    int this_regs;		       /* regs written in Upipe this cycle */
    enum { U, V } pipe;		       /* which pipe are we currently in? */
} p5state;

static void isched_init(void);
static void isched_queue(i386ins *insn);
static void isched_depend (i386ins *after, i386ins *before);
static void isched_flush(i386ins *output, int len);
static int isched_update(p5state *state, i386ins *insn);
static int isched_cost(p5state *state, i386ins *insn);

struct dependency {
    i386ins *before, *after;
    dependency *next, *nexti;
};

/*
 * The interface routine which is called from gen.c. This scans a
 * whole function at a time, separates out the schedulable blocks,
 * and subjects them to scheduling.
 */
void pentium_schedule(void) {
    i386ins *f, *begin;

    for (f = funhead; f; f = f->next) {
	int len;
	if (f->peepflags & PEEP_NONBB)
	    continue;
	len = 0;
	begin = f;
	isched_init();
	while (!(f->peepflags & PEEP_NONBB)) {
	    if (!(f->peepflags & PEEP_DISCARDED))
		isched_queue(f);
	    f = f->next;
	    len++;
	}
	isched_flush(begin, len);
    }
}

/*
 * These arrays are used to calculate dependencies between the
 * instructions: they keep track of which instruction was last to
 * read or write any given register.
 */

static i386ins *lastwrreg[NINTREGS];
static i386ins *lastwrmem, *laststack, *lastfloat;

static i386ins *canissue, *queue;

static dependency *alldeps;

static int bblen;

static p5state state;

/*
 * Initialise the scheduler in preparation to receive a bunch of
 * instructions.
 */
static void isched_init(void) {
    int i;

#ifdef ISCHED_DEBUG
    printf("isched_init\n");
#endif
    for (i=0; i<NINTREGS; i++)
	lastwrreg[i] = NULL;
    lastwrmem = laststack = lastfloat = NULL;
    canissue = NULL;
    queue = NULL;

    state.agi_regs = 0;		       /* can't really tell */
    state.this_regs = 0;
    state.pipe = U;
}

/*
 * Queue an instruction in the scheduler.
 */
static void isched_queue(i386ins *insn) {
    int i;

    insn->deps = NULL;
    insn->udeps = 0;
    insn->agi_deps = 0;

    bblen++;

#ifdef ISCHED_DEBUG
    printf("isched_queue(%p) `%s' depends:", insn, insn->text);
#endif
    for (i=0; i<NINTREGS; i++) {
	/*
	 * Any instruction reading or writing a register is
	 * dependent on the last instruction that wrote that
	 * register. In particular, that means successive writes to
	 * the same register will never be re-ordered.
	 */
	if ((insn->readreg[i] || insn->writereg[i]) && lastwrreg[i]) {
	    isched_depend (insn, lastwrreg[i]);
	    /*
	     * If the dependency is AGI-prone, flag this in the
	     * source instruction so we can schedule that
	     * preferentially.
	     */
	    if (insn->readreg[i] == 2)
		lastwrreg[i]->agi_deps = 1;
	}

	/*
	 * Any instruction writing a register is dependent on all
	 * the instructions that have read that register since it
	 * was last written (or since the beginning of the block,
	 * if the register hasn't been written yet in this block).
	 */
	if (insn->writereg[i]) {
	    i386ins *q;
	    /*
	     * Search backwards through the queue, flagging a
	     * dependency on every read of i we find, until we find
	     * a write of i or hit the end of the queue.
	     */
	    for (q = queue; q; q = q->nextsch2) {
		if (q->writereg[i])
		    break;
		if (q->readreg[i])
		    isched_depend (insn, q);
	    }
	}

	if (insn->writereg[i])
	    lastwrreg[i] = insn;
    }

    /*
     * Any instruction reading or writing memory is deemed
     * dependent on the last instruction that wrote memory. So
     * successive memory writes will never be reordered.
     */
    if ((insn->peepflags & (PEEP_RDMEM | PEEP_WRMEM)) && lastwrmem)
	isched_depend (insn, lastwrmem);

    /*
     * Any instruction writing memory is deemed dependent on all
     * the instructions that have read memory since it was last
     * written (or since the beginning of the block, if memory
     * hasn't been written yet in this block).
     */
    if (insn->peepflags & PEEP_WRMEM) {
	i386ins *q;
	/*
	 * Search backwards through the queue, flagging a
	 * dependency on every rdmem we find, until we find a wrmem
	 * or hit the end of the queue.
	 */
	for (q = queue; q; q = q->nextsch2) {
	    if (q->peepflags & PEEP_WRMEM)
		break;
	    if (q->peepflags & PEEP_RDMEM)
		isched_depend (insn, q);
	}
    }

    /*
     * Stack instructions can't be reordered.
     */
    if ((insn->peepflags & PEEP_STACK) && laststack)
	isched_depend (insn, laststack);

    /*
     * For simplicity, and because precise scheduling saves a lot
     * less in FPU code than in integer code, FPU instructions also
     * don't get reordered, though of course integer instructions
     * can be moved from side to side of them if there's no
     * interference.
     */
    if ((insn->peepflags & PEEP_FPU) && lastfloat)
	isched_depend (insn, lastfloat);

    if (insn->peepflags & PEEP_WRMEM)
	lastwrmem = insn;
    if (insn->peepflags & PEEP_STACK)
	laststack = insn;
    if (insn->peepflags & PEEP_FPU)
	lastfloat = insn;

    if (insn->udeps == 0) {
	insn->nextsch = canissue;
	canissue = insn;
    }

    insn->nextsch2 = queue;
    queue = insn;

#ifdef ISCHED_DEBUG
    printf("\n");
#endif
}

/*
 * Add a dependency: instruction `after' depends on instruction
 * `before'.
 */
static void isched_depend (i386ins *after, i386ins *before) {
    dependency *d;

#ifdef ISCHED_DEBUG
    printf(" %p", before);
#endif
    d = (dependency *) malloc(sizeof(dependency));
    d->before = before;
    d->after = after;
    d->nexti = before->deps;
    d->next = alldeps;
    before->deps = alldeps = d;
    after->udeps++;
}

/*
 * Rearrange and output the queued instructions. 
 */
static void isched_flush(i386ins *output, int len) {
    i386ins **insp, **issuep, *insn;
    dependency *d;
    int cost, bestcost;

#ifdef FOOBAR_HACK
    {
	static int i = 0, j = -1;
	if (j==-1) {
	    FILE *fp = fopen("foobar", "r");
	    if (fp) {
		fscanf(fp, "%d", &j);
		fclose(fp);
		if (j>0)
#ifdef ISCHED_DEBUG
		    printf("isched_hack: only scheduling first %d bbs\n", j);
#endif
	    }
	}
	i++;
	if (j>0 && i>=j)
	    return;
    }
#endif
#ifdef ISCHED_DEBUG
    printf("isched_flush\n");
#endif
    while (canissue) {
	issuep = NULL;
	for (insp = &canissue; *insp; insp = &(*insp)->nextsch) {
	    cost = isched_cost(&state, *insp);
#ifdef ISCHED_DEBUG
	    printf("Can issue %p at cost %d\n", *insp, cost);
#endif
	    if (cost < bestcost || !issuep) {
		issuep = insp;
		bestcost = cost;
	    }
	}
#ifdef ISCHED_DEBUG
	printf("Choosing %p\n", *issuep);
#endif
	insn = *issuep;
	*issuep = (*issuep)->nextsch; /* delete from `canissue' list */
	for (d = insn->deps; d; d = d->nexti) {
	    d->after->udeps--;
	    if (d->after->udeps == 0) {
		d->after->nextsch = canissue;
		canissue = d->after;
#ifdef ISCHED_DEBUG
		printf("Can now issue %p\n", d->after);
#endif
	    }
	}
	isched_update(&state, insn);
	output->scheduled = insn;
	output = output->next;
	len--;
	bblen--;
    }
    if (bblen) {
#ifdef ISCHED_DEBUG
	printf("PANIC! Lost instructions\n");
#endif
    }
    while (len--) {
	output->scheduled = NO_INSN;
	output = output->next;
    }

    /*
     * FIXME: deallocate all the dependencies.
     */
}

i386ins i386_nullins;		       /* got to define this _somewhere_ */

/*
 * Returns number of wasted half-cycles; may be ignored of course.
 */
static int isched_update(p5state *state, i386ins *insn) {
    int regmask;
    int agi;
    int i;

    /*
     * Registers written by this insn.
     */
    regmask = 0;
    for (i=0; i<NINTREGS; i++)
	if (insn->writereg[i])
	    regmask |= 1<<i;

    /*
     * Check for AGI. Note: presence of AGI automatically moves us
     * on to the U-pipe of the next cycle, wasting 1 or 2 half-
     * cycles depending on where we were.
     */
    agi = 0;
    for (i=0; i<NINTREGS; i++)
	if (insn->readreg[i] == 2 && (state->agi_regs & (1<<i)))
	    agi = (state->pipe == U ? 2 : 1);

    if (state->pipe == U || agi) {
	/*
	 * Currently scheduling for the U-pipe.
	 */
	if (insn->peepflags & PEEP_UPAIR) {
	    /*
	     * Can pair in the U-pipe.
	     */
	    state->pipe = V;
	    state->this_regs = regmask;
	    return agi;
	} else {
	    /*
	     * Can't pair in the U-pipe; generally means can't pair
	     * at all. So assume it takes a cycle (or more; we
	     * don't know the difference) by itself.
	     */
	    state->pipe = U;
	    state->agi_regs = regmask;
	    state->this_regs = 0;
	    return agi;
	}
    } else {
	/*
	 * Currently scheduling for the V-pipe.
	 */
	if (insn->peepflags & PEEP_VPAIR) {
	    int can_do = 1;
	    /*
	     * Can pair in the V-pipe, provided we don't read or
	     * write any registers the U-pipe instruction wrote.
	     */
	    for (i=0; i<NINTREGS; i++)
		if ((insn->readreg[i] || insn->writereg[i]) &&
		    (state->this_regs & (1<<i)))
		    can_do = 0;
	    if (can_do) {
		state->pipe = U;
		state->agi_regs = state->this_regs | regmask;
		state->this_regs = 0;
		return 0;
	    }
	}
	/*
	 * Can't pair in the V-pipe; move it to the U-pipe of
	 * the next cycle. It may or may not be pairable there.
	 */
	state->agi_regs = state->this_regs;
	state->this_regs = regmask;
	if (insn->peepflags & PEEP_UPAIR) {
	    state->pipe = V;
	} else {
	    state->pipe = U;
	    state->agi_regs = state->this_regs;
	    state->this_regs = 0;
	}
	return 1;
    }
}

static int isched_cost(p5state *state, i386ins *insn) {
    p5state tempstate = *state;	       /* structure copy */
    int wasted_slots;
    int pipe_specific;
    int agi_bonus;
    int score;

    /*
     * Number of half-cycles wasted, due to dependency or AGI, by
     * scheduling this instruction now. Loses points.
     */
    wasted_slots = isched_update(&tempstate, insn);

    /*
     * Bonus marks are awarded for an instruction which will pair
     * in the pipe we're currently in _but not the other one_, on
     * the grounds that we would do well to schedule such an
     * instruction as soon as we can because otherwise we might run
     * out of the right kind of slot before we get round to it.
     */
    if (state->pipe == U &&
	(insn->peepflags & PEEP_UPAIR) &&
	!(insn->peepflags & PEEP_VPAIR)) {
	pipe_specific = 1;
    } else if (state->pipe == V &&
	       (insn->peepflags & PEEP_VPAIR) &&
	       !(insn->peepflags & PEEP_UPAIR)) {
	pipe_specific = 1;
    } else
	pipe_specific = 0;

    /*
     * Bonus marks for an instruction which has a dependent that's
     * AGI-prone, because we should schedule such instructions
     * sooner rather than later on the grounds that if we don't
     * then the AGI may become unavoidable.
     */
    agi_bonus = insn->agi_deps;

    /*
     * The above criteria are now weighted and transformed into the
     * final score, with the weighting as follows:
     *
     * 100 points lost for each wasted slot.
     * 75 points gained for a pipe-specific instruction.
     * 100 points gained for the AGI bonus.
     */
    score = 0;
    score -= 100 * wasted_slots;
    score +=  75 * pipe_specific;
    score += 100 * agi_bonus;
    return -score;
}
