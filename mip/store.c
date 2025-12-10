/*
 * mip/store.c: Storage allocation for the Codemist C compiler
 * Copyright (C) Codemist Ltd., 1987-1992.
 * Copyright (C) Acorn Computers Ltd., 1988-1990.
 * Copyright (C) Advanced RISC Machines Limited, 1990-1992.
 */

/*
 * RCS $Revision: 1.11 $
 * Checkin $Date: 1995/07/06 14:40:35 $
 * Revising $Author: amycroft $
 */

#ifdef __STDC__
#  include <stdlib.h>
#  include <string.h>
#else
#  include "stddef.h"                                   /* for size_t */
#  include "strings.h"
extern char *malloc();
extern free();
#endif
#include "globals.h"
#include "store.h"
#include "defs.h"
#include "mcdep.h"  /* usrdbg(xxx) */
#include "errors.h"

void ClearToNull(void **a, int32 n) {
  while (--n >= 0) a[n] = NULL;
}

#define STORE_TRASHING 0
#define CHECKING_TRASH 0
/* STORE_TRASHING and CHECKING_TRASH allow building a compiler that     */
/* fills all allocations with trash, fills all disposed memory with     */
/* trash and checks all allocations to make sure they still contain     */
/* trash.  @@@ These are yet complete.  STORE_TRASHING works but does   */
/* not trash memory freed by alloc_unmark and drop_local_store.  Also   */
/* discard2 & discard3 trash their blocks in a different way.           */
/* CHECKING_TRASH does not work because of the above problems with      */
/* STORE_TRASHING                                                       */

typedef struct AllocHeader AllocHeader;
struct AllocHeader {
        AllocHeader *next;
#if STORE_TRASHING
        int32 size;
#endif
};

static AllocHeader *alloc_chain;    /* see alloc_init(), alloc_dispose() */

static int32 stuse_total, stuse_waste;
static int32 stuse[SU_Other-SU_Data+1];
static int32 maxAEstore;

#if STORE_TRASHING
typedef struct Trasher Trasher;
struct Trasher { int32 a[8]; };
static const Trasher trash =
    { { 0x50ff8001, 0x50ff8001, 0x50ff8001, 0x50ff8001,
        0x50ff8001, 0x50ff8001, 0x50ff8001, 0x50ff8001 } };

static void trash_block(VoidStar p, unsigned32 size) {
    Trasher *t = (Trasher *)p;
    while (sizeof(Trasher) < size)
    {
        *t++ = trash;
        size -= sizeof(Trasher);
    }
    memcpy(t, &trash, size);
}

#if CHECKING_TRASH
static void check_trashed(VoidStar p, unsigned32 size) {
    Trasher *t = (Trasher *)p;
    while (sizeof(Trasher) < size)
    {
        if (memcmp(t++, p, sizeof(Trasher)) != 0)
            syserr("free memory has been altered somewhere between [%p and %p)\n",
                   t - 1, t);
        size -= sizeof(Trasher);
    }
    if (memcmp(t, p, size) != 0)
        syserr("free memory has been altered somewhere between [%p and %p)\n",
               t, (char*)t + size);
}
#else
#define check_trashed(p, size) ((void)0)
#endif

#else
#define trash_block(p, size) ((void)0)
#define check_trashed(p, size) ((void)0)
#endif


static VoidStar cc_alloc(int32 n)
{   AllocHeader *p;
    stuse_total += n;
/* The next line's test probably only generates code on a PC.           */
    p = (sizeof(size_t) < sizeof(int32) &&
         (unsigned32)(n+sizeof(AllocHeader)) > 0xffff) ? 0 :
          (AllocHeader *)malloc((size_t)(n+sizeof(AllocHeader)));
    if (p != 0)
    { p->next = alloc_chain;
#if STORE_TRASHING
      p->size = n+sizeof(AllocHeader);
      trash_block((VoidStar)((char*)p + sizeof(AllocHeader)), n);
#endif
      alloc_chain = p;
      return (char*)p + sizeof(AllocHeader);
    }
#ifdef TARGET_IS_ARM
    if (usrdbg(DBG_ANY))
        cc_fatalerr(misc_fatalerr_space2);
    else
#endif
        cc_fatalerr(misc_fatalerr_space3);
    return 0;   /* stop compiler wingeing re implicit junk return */
}

void alloc_dispose(void)
{
    unsigned32 count = 0;
    if (debugging(DEBUG_STORE)) cc_msg("Freeing block(s) at:");
    while (alloc_chain != NULL)
    {   AllocHeader *next = alloc_chain->next;
        if (debugging(DEBUG_STORE))
          cc_msg("%s %p", count++ % 8 == 0 ? "\n":"", alloc_chain);
        trash_block((VoidStar)alloc_chain, alloc_chain->size);
        free(alloc_chain);
        alloc_chain = next;
    }
    if (debugging(DEBUG_STORE)) cc_msg("\n");
}

typedef struct Mark {
    struct Mark *prev;
    int syn_segno;
    char *syn_allp; int32 syn_hwm;
    int bind_segno;
    char *bind_allp; int32 bind_hwm;
} Mark;

static Mark *marklist;
static Mark *freemarks;

typedef struct FreeList {
    struct FreeList *next;
    int32 rest[1];
} FreeList;

static int     globallcnt;       /* count of segments allocated (int ok) */
static int32   globallxtra;      /* oversize global store allocated.    */
static char    *globallp;        /* pointers into symbol table          */
static char    *globalltop;      /* end of symbol table                 */
struct CurrentFnDetails currentfunction;

/* 'segmax' is the size of the notional arrays synsegbase[] etc.        */
static int segmax;
#define SEGMAX_INIT  16
#define SEGMAX_FACTOR 4

/* AM: one day turn segbase/segptr into a struct.                       */
static char    **synsegbase;       /* array of blocks of 'per routine' store */
static char    **synsegptr;        /* array of corresponding free addresses  */
static int     synsegcnt;          /* number thereof 0..segmax */
static char *synallp, *synalltop;  /* allocators therein */
static int32 synallhwm, synallmax; /* high water         */
static FreeList *synall2;          /* and a dispose list */
static FreeList *synall3;

static char    **bindsegbase;       /* list of blocks of 'per routine' store */
static char    **bindsegptr;        /* list of corresponding free addresses  */
static int     bindsegcnt;          /* number thereof    0..segmax */
static int     bindsegcur;          /* next block to use 0..segmax */
static char *bindallp, *bindalltop; /* allocators therein */
static int32 bindallhwm, bindallmax;/* high water         */
static FreeList *bindall2;          /* and a dispose list */
static FreeList *bindall3;

char *phasename;

static char *new_global_segment(void)
{
    char *w;
/* I will recycle a segment that had been used for local space if there  */
/* are any such available.                                               */
    if (bindsegcur < bindsegcnt)
    {   w = bindsegbase[--bindsegcnt];
        if (debugging(DEBUG_STORE))
            cc_msg("Global store %d from binder size %ld at %p\n",
                    (int)globallcnt, (long)SEGSIZE, w);
        check_trashed((VoidStar)w, SEGSIZE);
    }
    else
    {   w = (char *)cc_alloc(SEGSIZE);
        if (debugging(DEBUG_STORE))
            cc_msg("Global store alloc %d size %ld at %p (in $r)\n",
                    (int)globallcnt, (long)SEGSIZE, w, currentfunction.symstr);
    }
    globallcnt++;
    globallp = w, globalltop = w + SEGSIZE;
    return w;
}

/*
 * The value RR here is used when rounding store allocations up - it
 * is intended to ensure that this code runs properly when hosted on machines
 * where sizeof(char *) == 8.  Put a pad_to_hosttype() macro in util.h?
 */
#define RR (sizeof(char *) - 1)

VoidStar GlobAlloc(StoreUse t, int32 n)
{   char *p = globallp;
    n = (n + RR) & ~(int32)RR;          /* n = pad_to_hosttype(n, IPtr) */
    if (n > SEGSIZE)
    {   /* Big global store requests get a single oversize page.        */
        p = (char *)cc_alloc(n);
        if (debugging(DEBUG_STORE))
            cc_msg("Global overlarge store alloc size %ld at %p (in $r)\n",
                    (long)n, p, currentfunction.symstr);
        globallxtra += n;               /* could update globallcnt?     */
    }
    else
    {   if (p+n > globalltop)
            stuse_waste += globalltop-p,
            p = new_global_segment();
        else
            check_trashed(p, n);
        globallp = p + n;
    }
    stuse[(int)t] += n;
#ifndef ALLOC_DONT_CLEAR_MEMORY
    memset(p, 0xbb, n);
#endif
    return p;
}

VoidStar xglobal_cons2(StoreUse t, IPtr a, IPtr b)
{
    IPtr *p = (IPtr *) GlobAlloc(t, sizeof(IPtr[2]));
    p[0] = a; p[1] = b;
    return (VoidStar) p;
}

VoidStar xglobal_list3(StoreUse t, IPtr a, IPtr b, IPtr c)
{
    IPtr *p = (IPtr *) GlobAlloc(t, sizeof(IPtr[3]));
    p[0] = a; p[1] = b; p[2] = c;
    return (VoidStar) p;
}

VoidStar xglobal_list4(StoreUse t, IPtr a, IPtr b, IPtr c, IPtr d)
{
    IPtr *p = (IPtr *) GlobAlloc(t, sizeof(IPtr[4]));
    p[0] = a; p[1] = b; p[2] = c; p[3] = d;
    return (VoidStar) p;
}

VoidStar xglobal_list5(StoreUse t, IPtr a, IPtr b, IPtr c, IPtr d, IPtr e)
{
    IPtr *p = (IPtr *) GlobAlloc(t, sizeof(IPtr[5]));
    p[0] = a; p[1] = b; p[2] = c; p[3] = d; p[4] = e;
    return (VoidStar)p;
}

VoidStar xglobal_list6(StoreUse t, IPtr a, IPtr b, IPtr c, IPtr d, IPtr e, IPtr f)
{
    IPtr *p = (IPtr *) GlobAlloc(t, sizeof(IPtr[6]));
    p[0] = a; p[1] = b; p[2] = c; p[3] = d; p[4] = e; p[5] = f;
    return (VoidStar)p;
}

/* Volatile storage allocation for use within treatment of 1 function.  */

/* The following functions avoid a fixed limit on the number of pages   */
/* of local allocation without excessive store use.                     */
/* @@@ make this function more global soon (e.g. for ICODE &c)          */
/* The argument sizes are in bytes and old is unexamined if oldsize=0.  */
static VoidStar expand_array(VoidStar oldp, int32 oldsize, int32 newsize)
{   /* beware the next line if we ever record GlobAlloc's:              */
    VoidStar newp = GlobAlloc(SU_Other, newsize);
    if (oldsize != 0) memcpy(newp, oldp, (size_t)oldsize);
    trash_block(oldp, oldsize);
    return newp;
}

static void expand_segmax(int newsegmax)
{   int32 osize = (int32)segmax * sizeof(char *),
          nsize = (int32)newsegmax * sizeof(char *);
    synsegbase =  (char **)expand_array((VoidStar)synsegbase, osize, nsize);
    synsegptr  =  (char **)expand_array((VoidStar)synsegptr, osize, nsize);
    bindsegbase = (char **)expand_array((VoidStar)bindsegbase, osize, nsize);
    bindsegptr  = (char **)expand_array((VoidStar)bindsegptr, osize, nsize);
    segmax = newsegmax;
}

static char *new_bindalloc_segment(void)
{
    if (bindsegcur >= bindsegcnt)
    {   char *w = (char *)cc_alloc(SEGSIZE);
        if (bindsegcnt >= segmax) expand_segmax(segmax * SEGMAX_FACTOR);
        if (debugging(DEBUG_STORE))
            cc_msg("Binder store alloc %d size %ld at %p (%s in $r)\n",
                    (int)bindsegcnt, (long)SEGSIZE, w,
                    phasename, currentfunction.symstr);
        bindsegbase[bindsegcnt++] = w;
    }
    else
        check_trashed(bindsegbase[bindsegcur], SEGSIZE);
    return bindsegbase[bindsegcur++];
}

static char *new_synalloc_segment(void)
{
    char *w;
    if (synsegcnt >= segmax) expand_segmax(segmax * SEGMAX_FACTOR);
    if (bindsegcur < bindsegcnt)
    {   w = bindsegbase[--bindsegcnt];
        if (debugging(DEBUG_2STORE) && synsegcnt>0)
            cc_msg("Syntax store %d from binder size %ld at %p\n",
                    (int)synsegcnt, (long)SEGSIZE, w);
        check_trashed(w, SEGSIZE);
    }
    else
    {   w = (char *)cc_alloc(SEGSIZE);
        if (debugging(DEBUG_STORE))
            cc_msg("Syntax store alloc %d size %ld at %p (%s in $r)\n",
                    (int)synsegcnt, (long)SEGSIZE, w,
                    phasename, currentfunction.symstr);
    }
    return synsegbase[synsegcnt++] = w;
}

VoidStar BindAlloc(int32 n)
{
    char *p = bindallp;
    n = (n + RR) & ~(int32)RR;          /* n = pad_to_hosttype(n, IPtr) */
    if (n > SEGSIZE) syserr(syserr_overlarge_store1, (long)n);
    if (p + n > bindalltop)
    {   int i;                                 /* 0..segmax */
        if (bindsegcur > 0)
            bindsegptr[bindsegcur-1] = p;      /* stash highest used */
        for (i = bindsegcur;;)                 /* search for scraps  */
        {   --i;
            if (i < marklist->bind_segno)      /* nowhere big enough */
            {   p = new_bindalloc_segment();
                bindalltop = p + SEGSIZE;
                break;
            }
            p = bindsegptr[i];                 /* hope springs eternal */
            bindalltop = bindsegbase[i] + SEGSIZE;
            if ((n > 3*sizeof(int32)) && (p+n <= bindalltop))
                 /* fingers crossed      */
            {   /* we have scavenged something useful - swap to current */
                char *t = bindsegbase[i];
                bindsegbase[i] = bindsegbase[bindsegcur-1];
                bindsegbase[bindsegcur-1] = t;
                bindsegptr[i] = bindsegptr[bindsegcur-1];
                if (debugging(DEBUG_2STORE))
                {   cc_msg("Scavenge binder %d (%p), %ld left\n",
                            (int)i, t, (long)(bindalltop-(p+n)));
                }
                break;
            }
        }
        bindsegptr[bindsegcur-1] = (char *)DUFF_ADDR;
    }
    check_trashed(p, n);
    bindallp = p + n;
    if ((bindallhwm += n) > bindallmax) bindallmax = bindallhwm;
#ifndef ALLOC_DONT_CLEAR_MEMORY
    memset(p, 0xcc, n);
#endif
    return p;
}

VoidStar SynAlloc(int32 n)
{   char *p = synallp;
    n = (n + RR) & ~(int32)RR;          /* n = pad_to_hosttype(n, IPtr) */
    if (n > SEGSIZE) syserr(syserr_overlarge_store2, (long)n);
    if (p + n > synalltop)
    {   int i;                                 /* 0..segmax */
        if (synsegcnt > 0)
            synsegptr[synsegcnt-1] = p;        /* stash highest used */
        for (i = synsegcnt;;)                  /* search for scraps  */
        {   --i;
            if (i < marklist->syn_segno)       /* nowhere big enough */
            {   p = new_synalloc_segment();
                synalltop = p + SEGSIZE;
                break;
            }
            p = synsegptr[i];                  /* hope springs eternal */
            synalltop = synsegbase[i] + SEGSIZE;
            if ((n > 3*sizeof(int32)) && (p+n <= synalltop))
                /* fingers crossed      */
            {   /* we have scavenged something useful - swap to current */
                char *t = synsegbase[i];
                synsegbase[i] = synsegbase[synsegcnt-1];
                synsegbase[synsegcnt-1] = t;
                synsegptr[i] = synsegptr[synsegcnt-1];
                if (debugging(DEBUG_2STORE))
                {   cc_msg("Scavenge syntax %d (%p), %ld left\n",
                            (int)i, t, (long)(synalltop-(p+n)));
                }
                break;
            }
        }
        synsegptr[synsegcnt-1] = (char *)DUFF_ADDR;
    }
    check_trashed(p, n);
    synallp = p + n;
    if ((synallhwm += n) > synallmax) synallmax = synallhwm;
#ifndef ALLOC_DONT_CLEAR_MEMORY
    memset(p, 0xaa, n);
#endif
    return p;
}

VoidStar discard2(VoidStar p)
{
/* As cdr_(p) but returns the cell p to freestorage pool.      */
/* The freechain has a funny number xored in to help debugging */
    FreeList *pp = (FreeList *) p;
    VoidStar q = (VoidStar) pp->next;
    int i;                   /* 0..segmax */
    pp->rest[0] ^= 0x99990000;   /* to help with debugging */
    for (i = synsegcnt; i > 0; )
    {  --i;
       if (synsegbase[i] <= (char *)pp && (char *)pp < synsegbase[i]+SEGSIZE)
       {   pp->next = synall2;
           synall2 = (FreeList *)(((IPtr)pp) ^ 0x6a6a6a6a);
           return q;
       }
    }
    for (i = bindsegcur; i > 0;)
    {  --i;
       if (bindsegbase[i] <= (char *)pp && (char *)pp < bindsegbase[i]+SEGSIZE)
       {   pp->next = bindall2;
           bindall2 = (FreeList *)(((IPtr)pp) ^ 0x5a5a5a5a);
           return q;
       }
    }
    syserr(syserr_discard2, (VoidStar) pp);
    return q;
}

VoidStar xsyn_list2(IPtr a, IPtr b)
{   IPtr *p;
    if (synall2==NULL)
        p = (IPtr *) SynAlloc(sizeof(IPtr[2]));
    else
    {   p = (IPtr *)((IPtr) synall2 ^ 0x6a6a6a6a);
        synall2 = (FreeList *) p[0];
    }
    p[0] = a; p[1] = b;
    return (VoidStar) p;
}

VoidStar xbinder_list2(IPtr a, IPtr b)
{   if (bindall2==NULL)
    {   IPtr *p = (IPtr *) BindAlloc(sizeof(IPtr[2]));
        p[0] = a; p[1] = b;
        return (VoidStar) p;
    }
    else
    {   IPtr *p = (IPtr *)((IPtr) bindall2 ^ 0x5a5a5a5a);
        bindall2 = (FreeList *) p[0];
        p[0] = a; p[1] = b;
        return (VoidStar) p;
    }
}

VoidStar discard3(VoidStar p)
{
/* Returns the cell p to freestorage pool, with a funny number xored in to
 * help debugging.
 * Return value is (the old value of) p->next */
    FreeList *pp = (FreeList *) p;
    VoidStar q = (VoidStar) pp->next;
    int i;                   /* 0..segmax */
    pp->rest[0] ^= 0x99990000;   /* to help with debugging */
    pp->rest[1] ^= 0x99990000;   /* to help with debugging */
    for (i = synsegcnt; i > 0;)
    {   --i;
        if (synsegbase[i] <= (char *)pp && (char *)pp < synsegbase[i]+SEGSIZE)
        {   pp->next = synall3;
            synall3 = (FreeList *)(((IPtr)pp) ^ 0x6a6a6a6a);
            return q;
        }
    }
    for (i = bindsegcur; i > 0;)
    {   --i;
        if (bindsegbase[i] <= (char *)pp && (char *)pp < bindsegbase[i]+SEGSIZE)
        {   pp->next = bindall3;
            bindall3 = (FreeList *)(((IPtr)pp) ^ 0x5a5a5a5a);
            return q;
        }
    }
    syserr(syserr_discard3, (VoidStar) pp);
    return q;
}

VoidStar xbinder_list3(IPtr a, IPtr b, IPtr c)
{
    IPtr *p;
    if (bindall3 == NULL)
        p = (IPtr *) BindAlloc(sizeof(IPtr[3]));
    else {
        p = (IPtr *)((IPtr) bindall3 ^ 0x5a5a5a5a);
        bindall3 = (FreeList *) p[0];
    }
    p[0] = a; p[1] = b; p[2] = c;
    return (VoidStar) p;
}

VoidStar xsyn_list3(IPtr a, IPtr b, IPtr c)
{   IPtr *p;
    if (synall3 == NULL)
        p = (IPtr *) SynAlloc(sizeof(IPtr[3]));
    else {
        p = (IPtr *)((IPtr) synall3 ^ 0x6a6a6a6a);
        synall3 = (FreeList *) p[0];
    }
    p[0] = a; p[1] = b; p[2] = c;
    return (VoidStar) p;
}

VoidStar xsyn_list4(IPtr a, IPtr b, IPtr c, IPtr d)
{   IPtr *p = (IPtr *) SynAlloc(sizeof(IPtr[4]));
    p[0] = a, p[1] = b, p[2] = c, p[3] = d;
    return (VoidStar) p;
}

VoidStar xsyn_list5(IPtr a, IPtr b, IPtr c, IPtr d, IPtr e)
{
    IPtr *p = (IPtr *) SynAlloc(sizeof(IPtr[5]));
    p[0] = a; p[1] = b; p[2] = c; p[3] = d; p[4] = e;
    return (VoidStar) p;
}

VoidStar xsyn_list6(IPtr a, IPtr b, IPtr c, IPtr d, IPtr e, IPtr f)
{   IPtr *p = (IPtr *) SynAlloc(sizeof(IPtr[6]));
    p[0] = a, p[1] = b, p[2] = c, p[3] = d, p[4] = e, p[5] = f;
    return (VoidStar) p;
}

VoidStar xsyn_list7(IPtr a, IPtr b, IPtr c, IPtr d, IPtr e, IPtr f,
                   IPtr g)
{   IPtr *p = (IPtr *) SynAlloc(sizeof(IPtr[7]));
    p[0] = a, p[1] = b, p[2] = c, p[3] = d, p[4] = e, p[5] = f, p[6] = g;
    return (VoidStar) p;
}

/* @@@ not used essentially for C but it is used for C++ */
void alloc_mark(void)
{
    Mark *p;
    if ((p = freemarks) != NULL)
        freemarks = p->prev;
    else
        p = (Mark *) GlobAlloc(SU_Other, sizeof(Mark));

    p->prev = marklist; marklist = p;
    p->syn_segno = synsegcnt;
    p->syn_allp = synallp; p->syn_hwm = synallhwm;
    p->bind_segno = bindsegcur;
    p->bind_allp = bindallp; p->bind_hwm = bindallhwm;

    if (debugging(DEBUG_STORE))
        cc_msg("Mark %d, %p, %lx :: %d, %p, %lx\n",
                synsegcnt, synallp, (long)synallhwm,
                bindsegcur, bindallp, (long)bindallhwm);
}

/* #ifdef PASCAL_OR_FORTRAN_OR_CPLUSPLUS -- comment out? */
void alloc_unmark(void)
{
    Mark *p = marklist;
    if (p->prev == NULL) syserr(syserr_alloc_unmark);
    if (synsegcnt > p->syn_segno)
        syserr(syserr_alloc_unmark1);
    marklist = p->prev;
    p->prev = freemarks; freemarks = p;
    synsegcnt = p->syn_segno; synallp = p->syn_allp;
    synalltop = (synallp == DUFF_ADDR) ? (char *)DUFF_ADDR
                                       : synsegbase[synsegcnt-1] + SEGSIZE;
    synallhwm = p->syn_hwm;
    /* NULLing out the free lists like this will lose the blocks */
    /* that are between synallp and synalltop until the segment is */
    /* recycled but it's considerably cheaper than scanning the free */
    /* lists and segment array */
    synall2 = NULL; synall3 = NULL;
    bindsegcur = p->bind_segno; bindallp = p->bind_allp;
    bindalltop = (bindallp == DUFF_ADDR) ? (char *)DUFF_ADDR
                                         : bindsegbase[bindsegcur-1] + SEGSIZE;
    bindallhwm = p->bind_hwm;
    bindall2 = NULL; bindall3 = NULL;

    if (debugging(DEBUG_STORE))
        cc_msg("Unmark %d, %p, %lx :: %d, %p, %lx\n",
                synsegcnt, synallp, (long)synallhwm,
                bindsegcur, bindallp, (long)bindallhwm);
}
/* #endif */

void drop_local_store(void)
{
/* Here the threat issued using SynAlloc or syn_xxx materialises, and a
   lot of local store is trampled upon. */
/* N.B. drop_local_store *MUST* be called before reinit_alloc()          */
   while (synsegcnt > marklist->syn_segno)
    {    char *p = synsegbase[--synsegcnt];
#ifdef never
         if (debugging(DEBUG_2STORE))
             cc_msg("Re-using syntax store %p as binder %d\n",
                     p, (int)bindsegcnt);
#endif
/* we do not need to mess with limits here as set to SEGSIZE when used */
         if (bindsegcnt >= segmax) expand_segmax(segmax * SEGMAX_FACTOR);
         bindsegbase[bindsegcnt++] = p;
    }
    synallp = marklist->syn_allp;
    synalltop = (synallp == DUFF_ADDR) ? (char *)DUFF_ADDR
                                       : synsegbase[synsegcnt-1] + SEGSIZE;
    if (debugging(DEBUG_2STORE) && synallhwm==synallmax)
        cc_msg("Max SynAlloc %ld in $r\n",
                (long)synallmax, currentfunction.symstr);
    synallhwm = marklist->syn_hwm;
    synall2 = NULL; synall3 = NULL;  /* see comment in alloc_unmark */
}

void alloc_reinit(void)
{   if (synsegcnt > marklist->syn_segno ||
        synallp != marklist->syn_allp ||
        synalltop != ((synallp == DUFF_ADDR) ? (char *)DUFF_ADDR
                                       : synsegbase[synsegcnt-1] + SEGSIZE) ||
        synall2 != NULL ||
        synall3 != NULL
       )
        syserr(syserr_alloc_reinit);
    bindallhwm = marklist->bind_hwm;
    bindsegcur = marklist->bind_segno; bindallp = marklist->bind_allp;
    bindalltop = (bindallp == DUFF_ADDR) ? (char *)DUFF_ADDR
                                         : bindsegbase[bindsegcur-1] + SEGSIZE;
    bindall2 = NULL; bindall3 = NULL;   /* see comment in alloc_unmark */
}

void alloc_init(void)
{
    /* reset the following vars for each one of a many file compilation */
    stuse_total = 0, stuse_waste = 0;
    memclr(stuse, sizeof(stuse));
    if (alloc_chain != NULL) syserr("alloc_init notices there was no alloc_dispose");
    synsegcnt = 0;
    synallp = synalltop = (char *)DUFF_ADDR;
    synall2 = NULL; synall3 = NULL;
    synallhwm = 0, synallmax = 0;
    bindsegcur = 0, bindsegcnt = 0;
    bindallp = bindalltop = (char *)DUFF_ADDR;
    bindallhwm = 0, bindallmax = 0;
    bindall2 = NULL; bindall3 = NULL;
    globallcnt = 0; globallxtra = 0;
    globallp = globalltop = (char *)DUFF_ADDR;
    marklist = NULL; freemarks = NULL;
    maxAEstore = 0;
    synsegbase = synsegptr = bindsegbase = bindsegptr = (char **)DUFF_ADDR;
    segmax = 0; expand_segmax(SEGMAX_INIT);
    alloc_mark();
}

void alloc_noteAEstoreuse(void)
/* Calculate as blocks allocated minus space unused in (only) LAST BLOCK */
{   int32 n = ((int32)synsegcnt*SEGSIZE - (synalltop - synallp)) +
            ((int32)bindsegcur*SEGSIZE - (bindalltop - bindallp));
    if (n > maxAEstore) maxAEstore = n;
}

void show_store_use(void)
{
#ifdef ENABLE_STORE
    cc_msg(
        "Total store use (excluding stdio buffers/stack) %ld bytes\n",
        (long)stuse_total);
    cc_msg("Global store use %ld/%ld + %ld bytes\n",
        (long)((int32)globallcnt*SEGSIZE - (globalltop - globallp)),
        (long)((int32)globallcnt*SEGSIZE),
        (long)globallxtra);
    cc_msg(
        "  thereof %ld+%ld bytes pended relocation, %ld bytes pended data\n",
        (long)stuse[(int)SU_Xref],
        (long)stuse[(int)SU_Xsym],
        (long)stuse[(int)SU_Data]);
    cc_msg(
        "  %ld bytes symbols, %ld bytes top-level vars, %ld bytes types\n",
        (long)stuse[(int)SU_Sym],
        (long)stuse[(int)SU_Bind],
        (long)stuse[(int)SU_Type]);
    cc_msg(
        "  %ld bytes constants, %ld bytes pre-processor, %ld bytes wasted\n",
        (long)stuse[(int)SU_Const], (long)stuse[(int)SU_PP], (long)stuse_waste);
    cc_msg( "Local store use %ld+%ld/%ld bytes - front end max %ld\n",
        (long)synallmax, (long)bindallmax,
        (long)((int32)(int)(synsegcnt+bindsegcnt)*SEGSIZE),
        (long)maxAEstore);
#endif /* ENABLE_STORE */
}

/* end of mip/store.c */
