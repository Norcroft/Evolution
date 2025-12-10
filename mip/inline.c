/*
 * inline.c: inline function expansion
 * Copyright (C) Advanced Risc Machines Ltd., 1993
 */

/*
 * RCS $Revision: 1.38 $
 * Checkin $Date: 1995/11/01 13:35:37 $
 * Revising $Author: hmeekings $
 */

/* Nasties to tidy up:
** (1) the use of h0_(Binder)
** (2) lab_xname_()/lab_name_().  Use small ints everywhere for labelnumbers?
*/

#include <string.h>
#include <stddef.h>  /* offsetof */

#include "globals.h"
#include "store.h"
#include "flowgraf.h"
#include "codebuf.h"
#include "regalloc.h"
#include "aeops.h"
#include "aetree.h"
#include "bind.h"
#include "jopcode.h"
#include "inline.h"
#include "cg.h"
#include "regsets.h"
#include "simplify.h"
#include "errors.h"
#include "builtin.h"

typedef struct SavedFnList SavedFnList;
typedef union { RegSort rs; VRegnum r; } Inline_VRegIndex;

typedef struct Inline_ArgDesc Inline_ArgDesc;
struct Inline_ArgDesc {
  Inline_ArgDesc *cdr;
  Binder *arg;
  int flags;
  int32 accesssize;
  Inline_ArgSubstList *argsubst;
};

#define IA_Updated 1
#define IA_OddAccess 2
#define IA_AccessSet 4
#define IA_UpdatedOutsideCore 8
#define IA_OddUseForPlusInCore 16

typedef unsigned char BlockMap;

typedef enum {
  IS_Ord,
  IS_Ctor,
  IS_Dtor
} Inline_Sort;

struct SavedFnList {
  SavedFnList *cdr;
  Inline_SavedFn fn;
  int32 maxlabel;
  VRegnum maxreg;
  Inline_VRegIndex *vregtypetab;
  Inline_ArgDesc *args;
  Inline_Sort sort;
  union {
    BlockMap *ctor;
    BlockHead *dtor;
  } a;
  bool usedoutofline;
};

typedef struct BindListIndex BindListIndex;
struct BindListIndex {
  BindListIndex *cdr;
  BindList *orig;
  BindList *copy;
};

typedef struct {
  SynBindList *copied;
  BindListIndex *sharedbindlists;
  Inline_VRegIndex *vregindex;
  BindList *nullenv;
} SaveFnState;

static SavedFnList *saved_fns;

Inline_SavedFn *Inline_FindFn(Binder *b) {
  SavedFnList *fn = (SavedFnList *)bindinline_(b);
  if (fn != NULL)
    return &fn->fn;
  return NULL;
}

void Inline_RealUse(Binder *b) {
  SavedFnList *fn = (SavedFnList *)bindinline_(b);
  attributes_(b) |= A_REALUSE;
  if (fn != NULL) fn->usedoutofline = YES;
}

static BindList *GlobalSharedBindListCopy(SaveFnState *st, BindList *bl);

static Binder *GlobalBinderCopy(SaveFnState *st, Binder *b) {
  if (h0_(b) != s_binder) return (Binder *)(IPtr)h0_(b);
  { Symstr *sym = bindsym_(b);
    Binder *bnew;
    size_t n;
    if (bindstg_(b) & (bitofstg_(s_virtual)|b_globalregvar|bitofstg_(s_auto)))
      n = sizeof(Binder);
    else {
      if (attributes_(b) & A_GLOBALSTORE) return b;
      n = SIZEOF_NONAUTO_BINDER;
    }
    if (isgensym(sym)) sym = gensymvalwithname(YES, symname_(sym));
    bnew = global_mk_binder(NULL, sym, bindstg_(b), bindtype_(b));
    memcpy(bnew, b, n);
    bindsym_(bnew) = sym;
    h0_(b) = (AEop)(IPtr)bnew;
    st->copied = mkSynBindList(st->copied, b);
    if (bindstg_(b) & b_bindaddrlist)
      bindbl_(bnew) = GlobalSharedBindListCopy(st, bindbl_(b));
    return bnew;
  }
}

static BindList *GlobalBindListCopy(SaveFnState *st, BindList *bl) {
  BindList *bl_new = NULL;
  BindList *bp, **bpp = &bl_new;
  for (; bl != NULL; bl = bl->bindlistcdr, bpp = &bp->bindlistcdr) {
    Binder *b = GlobalBinderCopy(st, bl->bindlistcar);
    bp = (BindList *)global_cons2(SU_Inline, NULL, b);
    *bpp = bp;
  }
  return bl_new;
}

static BindList *GlobalSharedBindListCopy(SaveFnState *st, BindList *bl) {
  BindListIndex *p = st->sharedbindlists;
  if (bl == NULL) return NULL;

  for (; p != NULL; p = cdr_(p))
    if (bl == p->orig)
      return p->copy;

  { BindList *bl_new = (BindList *)global_cons2(SU_Inline,
                                                GlobalSharedBindListCopy(st, bl->bindlistcdr),
                                                GlobalBinderCopy(st, bl->bindlistcar));
    st->sharedbindlists = (BindListIndex *)syn_list3(st->sharedbindlists, bl, bl_new);
    return bl_new;
  }
}

static Inline_ArgDesc *ArgDesc_Find(Binder *b, Inline_ArgDesc *ad) {
  for (; ad != NULL; ad = cdr_(ad))
    if (b == ad->arg)
      return ad;
  return NULL;
}

static bool IsSTRWExpansion(Icode *ic, int32 *offset) {
  /* Already checked that ic->op is STRBK */
  Icode *ic2 = ic+1,
        *ic3 = ic+2;
  if ((ic2->op & J_TABLE_BITS) == J_SHRK && ic2->m.i == 8 &&
      ic->r1.r == ic2->r2.r &&
      (ic3->op & J_TABLE_BITS) == J_STRBK && ic3->r2.r == ic->r2.r &&
      ic3->r1.r == ic2->r1.r &&
      (target_lsbytefirst ? ic3->m.i == ic->m.i+1 :
                            ic3->m.i+1 == ic->m.i)) {
    *offset = target_lsbytefirst ? ic->m.i : ic->m.i-1;
    return YES;
  }
  return NO;
}

static BlockMap *NewBlockMap(unsigned32 n) {
  size_t size = (size_t)((n + 7)/ 8);
  BlockMap *map = (BlockMap *)GlobAlloc(SU_Inline, size);
  memset(map, 0, size);
  return map;
}

static bool MapBitSet(BlockMap *map, unsigned32 n) {
  BlockMap *p = &map[n / 8];
  unsigned bit = 1 << (n % 8);
  return ((*p & bit) != 0);
}


static bool SetMapBit(BlockMap *map, unsigned32 n) {
  BlockMap *p = &map[n / 8];
  unsigned bit = 1 << (n % 8);
  if ((*p & bit) == 0) {
    *p |= bit;
    return YES;
  } else
    return NO;
}


static void AddToMap(BlockMap *map, LabelNumber *lab) {
  if (!is_exit_label(lab)) {
    BlockHead *b = lab->block;
    if (SetMapBit(map, lab_name_(lab))) {
      if (blkflags_(b) & BLKSWITCH) {
        int32 n = blktabsize_(b);
        LabelNumber **table = blktable_(b);
        while (--n >= 0) AddToMap(map, table[n]);
      } else {
        if (blkflags_(b) & BLK2EXIT)
          AddToMap(map, blknext1_(b));
        AddToMap(map, blknext_(b));
      }
    }
  }
}

/* The compiler prepends code to call new() to constructors, and postpends
 * code to call delete() to destructors. When called for objects which don't
 * need allocation/deallocation, this gives rise to dead code which latter
 * phases of compilation are capable of removing, but at a cost (the increased
 * number of basic blocks slows down both cse and regalloc). Also, the code
 * to call new() assigns to __this, preventing it from being substituted when
 * the constructor is inlined, and therefore (perhaps spuriously) leaving the
 * constructed object marked as address-taken.
 */
static void FindStructorCore(SavedFnList *p) {
  BlockHead *b = blkdown_(top_block);
  BindList *argbl = currentfunction.argbindlist;
  Binder *arg1 = argbl == NULL ? NULL : argbl->bindlistcar;
  p->sort = IS_Ord;
  if ((blkflags_(b) & BLK2EXIT) &&
      blklength_(b) == 2) {
    Icode *ic1 = blkcode_(b),
          *ic2 = ic1 + 1;
    if (ic1->op == J_LDRV+J_ALIGN4 && ic1->m.b == arg1 &&
        (ic2->op & ~Q_MASK) == J_CMPK && Q_issame((ic2->op & Q_MASK), Q_NE) &&
        ic2->m.i == 0 && ic2->r2.r == ic1->r1.r) {
      BlockMap *map = NewBlockMap(p->maxlabel);
      SetMapBit(map, lab_name_(blklab_(top_block)));
      AddToMap(map, blknext1_(b));
      p->a.ctor = map; p->sort = IS_Ctor;
    }
  } else {
    BlockHead *b2 = blkup_(bottom_block);
    BlockHead *b1 = blkup_(b2);
    if (b1 != NULL &&
        !(blkflags_(b2) & (BLK2EXIT+BLKSWITCH)) && blknext_(b2) == blklab_(bottom_block) &&
        (blkflags_(b1) & BLK2EXIT) && Q_issame(blkflags_(b1) & Q_MASK, Q_EQ) &&
        blknext_(b1) == blklab_(b2) && blknext1_(b1) == blklab_(bottom_block) &&
        blklength_(b1) >= 2 &&
        argbl != NULL && argbl->bindlistcdr != NULL) {
      Binder *arg2 = argbl->bindlistcdr->bindlistcar;
      int32 n = blklength_(b1);
      Icode *ic1 = &blkcode_(b1)[n-2];
      Icode *ic2 = ic1 + 1;
      if (ic1->op == J_LDRV+J_ALIGN4 && ic1->m.b == arg2 &&
          (ic2->op & ~Q_MASK) == J_CMPK && ic2->m.i == 0 && ic2->r2.r == ic1->r1.r) {
        p->a.dtor = b1; p->sort = IS_Dtor;
        /* this is tentative: we need to know that arg2 isn't assigned to */
        /* before we're certain                                           */
      }
    }
  }
}

bool Inline_Save(Binder *b, BindList *local_binders, BindList *regvar_binders) {
  SavedFnList *p = (SavedFnList *)GlobAlloc(SU_Inline, sizeof(*p));
  SaveFnState st;
  bindinline_(b) = p;
  p->usedoutofline = (attributes_(b) & A_REALUSE) != 0;
  st.copied = NULL;
  st.sharedbindlists = NULL;
  p->fn.fndetails = currentfunction;
  if (p->fn.fndetails.structresult != NULL)
    p->fn.fndetails.structresult = GlobalBinderCopy(&st, p->fn.fndetails.structresult);
  if (p->fn.fndetails.xrflags & xr_defext)
    Inline_RealUse(b);
  p->fn.fndetails.argbindlist = GlobalBindListCopy(&st, p->fn.fndetails.argbindlist);
  p->fn.var_binders = GlobalBindListCopy(&st, local_binders);
  p->fn.reg_binders = GlobalBindListCopy(&st, regvar_binders);
  p->fn.top_block = p->fn.bottom_block = NULL;
  p->maxreg = vregister(INTREG);
  { LabelNumber *l = nextlabel();
    p->maxlabel = lab_name_(l);
  }
  FindStructorCore(p);
  { VRegnum n = p->maxreg;
    Inline_VRegIndex *vt = (Inline_VRegIndex *)GlobAlloc(SU_Inline, p->maxreg * sizeof(*vt));
    while (--n > NMAGICREGS) vt[n].rs = vregsort(n);
    p->vregtypetab = vt;
  }
  { BindList *bl = p->fn.fndetails.argbindlist;
    Inline_ArgDesc **adp = &p->args;
    p->args = NULL;
    for (; bl != NULL; bl = bl->bindlistcdr) {
      Inline_ArgDesc *ad = (Inline_ArgDesc *)GlobAlloc(SU_Inline, sizeof(*ad));
      cdr_(ad) = NULL; ad->arg = bl->bindlistcar; ad->flags = 0;
      *adp = ad; adp = &cdr_(ad);
    }
  }
  { BlockHead *b = top_block;
    BlockHead *blast = NULL, *bnew;
    for (b = top_block; b != NULL; b = blkdown_(b), blast = bnew) {
      bnew = (BlockHead *)GlobAlloc(SU_Inline, sizeof(*bnew));
      *bnew = *b;
      blkup_(bnew) = blast;
      if (blast == NULL)
        p->fn.top_block = bnew;
      else
        blkdown_(blast) = bnew;
      if (blkflags_(b) & BLKSWITCH) {
        int32 n = blktabsize_(b);
        LabelNumber **sw_old = blktable_(b);
        LabelNumber **sw_new = (LabelNumber **)GlobAlloc(SU_Inline, n * sizeof(LabelNumber*));
        while (--n >= 0) sw_new[n] = (LabelNumber *)(IPtr)lab_xname_(sw_old[n]);
        blktable_(bnew) = sw_new;
      } else {
        blknext_(bnew) = (LabelNumber *)(IPtr)lab_xname_(blknext_(b));
        if (blkflags_(b) & BLK2EXIT)
          blknext1_(bnew) = (LabelNumber *)(IPtr)lab_xname_(blknext1_(b));
      }
      blklab_(bnew) = (LabelNumber *)(IPtr)lab_name_(blklab_(b));
      blkstack_(bnew) = GlobalSharedBindListCopy(&st, blkstack_(b));
      { int32 n = blklength_(b), len = n;
        Icode *code_old = blkcode_(b);
        Icode *code_new = n == 0 ? (Icode *)DUFF_ADDR :
                                   (Icode *)GlobAlloc(SU_Inline, n * sizeof(Icode));
        bool inctorcore = YES;
        if (p->sort == IS_Ctor)
          inctorcore = MapBitSet(p->a.ctor, lab_name_(blklab_(b)));
        else if (p->sort == IS_Dtor && b == p->a.dtor)
          p->a.dtor = bnew;
        blkcode_(bnew) = code_new;
        while (--n >= 0) {
          Icode *ic_old = &code_old[n];
          Icode *ic_new = &code_new[n];
          J_OPCODE op = ic_old->op & J_TABLE_BITS;
          *ic_new = *ic_old;
          if (op == J_SETSPENV) {
            ic_new->m.bl = GlobalSharedBindListCopy(&st, ic_new->m.bl);
            ic_new->r2.bl = GlobalSharedBindListCopy(&st, ic_new->r2.bl);
          } else if (op == J_SETSPGOTO) {
            ic_new->r2.bl = GlobalSharedBindListCopy(&st, ic_new->r2.bl);
            ic_new->m.l = (LabelNumber *)(IPtr)lab_xname_(ic_new->m.l);
          } else if (uses_stack(op) || op == J_CALLK ||
                     op==J_ADCON || op == J_INIT || op == J_INITF || op == J_INITD) {
            Binder *b = ic_new->m.b = GlobalBinderCopy(&st, ic_new->m.b);
            Inline_ArgDesc *ad = ArgDesc_Find(b, p->args);
            if (ad != NULL) {
              if (stores_r1(op)) {
                if (inctorcore)
                  ad->flags |= IA_Updated;
                else
                  ad->flags |= IA_UpdatedOutsideCore;
              } else if (op == J_LDRV) {
                Icode *next1 = ic_new + 1;
                if (n+1 < len) {
                  J_OPCODE nextop = next1->op & J_TABLE_BITS;
                  if (uses_mem(nextop) && reads_r2(nextop) && !reads_r3(nextop) &&
                     next1->r2.r == ic_new->r1.r) {
                    int32 accesssize;
#ifdef TARGET_LACKS_HALFWORD_STORE
                    int32 k;
                    if (nextop == J_STRBK &&
                        n+3 < len && IsSTRWExpansion(next1, &k) && k == 0)
                      accesssize = MEM_W;
                    else
#endif
                    if (next1->m.i == 0) {
                      accesssize = j_memsize(nextop);
                      if ((ad->flags & IA_AccessSet) && ad->accesssize != accesssize)
                        accesssize = -1;
                    } else
                      accesssize = -1;
                    ad->accesssize = accesssize;
                    ad->flags |= IA_AccessSet;
                    continue;

                  } else if (nextop == J_MOVR)
                    continue;
                }
                if (inctorcore) ad->flags |= IA_OddUseForPlusInCore;

              } else
                ad->flags |= IA_OddAccess;
            }
          } else if (op==J_STRING)
            ic_new->m.s = globalize_strseg(ic_new->m.s);
        }
      }
    }
    blkdown_(blast) = NULL;
    p->fn.bottom_block = blast;
  }
  { SynBindList *bl = st.copied;
    for (; bl != NULL; bl = bl->bindlistcdr)
      h0_(bl->bindlistcar) = s_binder;
  }
  if (p->sort == IS_Dtor) {
    /* retract if the second argument isn't read-only */
    Inline_ArgDesc *ad = cdr_(p->args);
    if (ad->flags & (IA_OddAccess + IA_Updated + IA_UpdatedOutsideCore))
      p->sort = IS_Ord;
  }
  if (debugging(DEBUG_CG)) {
    cc_msg("Inline_Save %s", symname_(p->fn.fndetails.symstr));
    if (p->sort == IS_Dtor) {
      cc_msg(": Dtor: %ld\n", (int32)blklab_(p->a.dtor));
    } else if (p->sort == IS_Ctor) {
      unsigned32 i;
      BlockMap *map = p->a.ctor;
      int c = '{';
      cc_msg(": Ctor: ");
      for (i = 0; i < p->maxlabel; i++)
        if (MapBitSet(map, i)) { cc_msg("%c%ld", c, (long)i); c = ' '; }
      cc_msg("}\n");
    } else
      cc_msg("\n");
  }
  cdr_(p) = saved_fns;
  saved_fns = p;
  return YES;
}

static BindList *FromGlobalSharedBindListCopy(SaveFnState *st, BindList *bl);

static Binder *FromGlobalBinderCopy(SaveFnState *st, Binder *b) {
  if (h0_(b) != s_binder) return (Binder *)(IPtr)h0_(b);
  { Binder *bnew;
    size_t n;
    if (bindstg_(b) & (bitofstg_(s_virtual)|b_globalregvar|bitofstg_(s_auto)))
      n = sizeof(Binder);
    else {
      if (attributes_(b) & A_GLOBALSTORE) return b;
      n = SIZEOF_NONAUTO_BINDER;
    }
    bnew = mk_binder(bindsym_(b), bindstg_(b), bindtype_(b));
    memcpy(bnew, b, n);
    h0_(b) = (AEop)(IPtr)bnew;
    st->copied = mkSynBindList(st->copied, b);
    if (bindstg_(b) & bitofstg_(s_auto)) {
      if (bindstg_(b) & b_bindaddrlist)
        bindbl_(bnew) = FromGlobalSharedBindListCopy(st, bindbl_(b));
      if (bindxx_(b) != GAP) bindxx_(bnew) = st->vregindex[bindxx_(b)].r;
    }
    return bnew;
  }
}

static BindList *FromGlobalBindListCopy(SaveFnState *st, BindList *bl) {
  BindList *bl_new = NULL;
  BindList *bp, **bpp = &bl_new;
  for (; bl != NULL; bl = bl->bindlistcdr, bpp = &bp->bindlistcdr) {
    Binder *b = FromGlobalBinderCopy(st, bl->bindlistcar);
    bp = (BindList *)binder_cons2(NULL, b);
    *bpp = bp;
  }
  return bl_new;
}

static BindList *FromGlobalSharedBindListCopy(SaveFnState *st, BindList *bl) {
  BindListIndex *p = st->sharedbindlists;
  if (bl == NULL) return st->nullenv;
  for (; p != NULL; p = cdr_(p))
    if (bl == p->orig)
      return p->copy;

  { BindList *bl_new = (BindList *)binder_cons2(FromGlobalSharedBindListCopy(st, bl->bindlistcdr),
                                                FromGlobalBinderCopy(st, bl->bindlistcar));
    st->sharedbindlists = (BindListIndex *)syn_list3(st->sharedbindlists, bl, bl_new);
    return bl_new;
  }
}

static LabelNumber *FromGlobalLabel(LabelNumber **index, Inline_RestoreControl *rc, LabelNumber *old) {
  /* (LabelNumber *) values like 'old' here only hold small ints.       */
  return !is_exit_label(old) ? index[(IPtr)old] :
         rc != NULL && rc->exitlabel != NULL ? rc->exitlabel :
                               old;
}

static BindList *BindListCopy(BindList *bl) {
  BindList *bl_new = NULL;
  BindList *bp, **bpp = &bl_new;
  for (; bl != NULL; bl = bl->bindlistcdr, bpp = &bp->bindlistcdr) {
    bp = (BindList *)binder_cons2(NULL, bl->bindlistcar);
    *bpp = bp;
  }
  return bl_new;
}

static SavedFnList *FindSavedFn(Inline_SavedFn *fn) {
  SavedFnList *p;
  for (p = saved_fns; p != NULL; p = cdr_(p))
    if (fn->fndetails.symstr == p->fn.fndetails.symstr)
      return p;
  return NULL;
}

void Inline_Restore(Inline_SavedFn *fn, Inline_RestoreControl *rc) {
  SaveFnState st;
  SavedFnList *p = FindSavedFn(fn);
  Inline_VRegIndex *vt = p->vregtypetab;
  int32 n = p->maxreg;
  BindList *argb_orig = fn->fndetails.argbindlist;
  Inline_ArgDesc *ad;
  BlockMap *map = NULL;
  BlockHead *dtorlast = NULL;
  BlockHead *bottom = p->fn.bottom_block;
  bool skipblock2 = NO;
  while (--n >= 0) vt[n].r = n <= NMAGICREGS ? n : vregister(vt[n].rs);
  st.copied = NULL;
  st.sharedbindlists = NULL;
  st.vregindex = p->vregtypetab;
  if (fn->fndetails.structresult != NULL)
    fn->fndetails.structresult = FromGlobalBinderCopy(&st, fn->fndetails.structresult);
  fn->fndetails.argbindlist = FromGlobalBindListCopy(&st, argb_orig);
  for (ad = p->args; ad != NULL; ad = cdr_(ad))
      ad->argsubst = NULL;
  if (rc != NULL) {
    Inline_ArgSubstList *as = rc->argreplace;
    BindList **blp = &fn->fndetails.argbindlist;
    int ia_cantsubst = IA_Updated + IA_OddAccess + IA_UpdatedOutsideCore;
    for (ad = p->args; ad != NULL; ad = cdr_(ad)) {
    /* Prune from the argument bindlist those arguments which we know can */
    /* be substituted for                                                 */
      BindList *bl = *blp;
      if (as == NULL) break;
      if (as->arg == ad->arg) {
        Inline_ArgSubstList *thisas = as;
        bool subst = YES;
        as = cdr_(as);
        if (ad == p->args && p->sort == IS_Ctor &&
            !(ad->flags & (IA_Updated + IA_OddAccess))) {
          if (thisas->notnull) {
            if (thisas->sort == T_Plus && (ad->flags & IA_OddUseForPlusInCore))
              subst = NO;
            map = p->a.ctor;
            ia_cantsubst = IA_Updated + IA_OddAccess;
          } else {
            if (thisas->sort == T_Int && intval_(thisas->replacement.ex) == 0)
              skipblock2 = YES;
            if (ad->flags & IA_UpdatedOutsideCore)
              subst = NO;
          }
        } else if (p->sort == IS_Dtor && ad == cdr_(p->args) &&
                   !(ad->flags & ia_cantsubst) &&
                   thisas->sort == T_Int && intval_(thisas->replacement.ex) == 0)
          dtorlast = p->a.dtor;

        else if ((thisas->sort == T_Plus && (ad->flags & IA_OddUseForPlusInCore)) ||
                 ad->flags & ia_cantsubst)
          subst = NO;

        if (subst) {
          *blp = bl->bindlistcdr;
          ad->argsubst = thisas;
          if (debugging(DEBUG_CG)) {
            cc_msg("Substitution for %s$b: ",
              thisas->sort==T_AdconV ? "*" : "",
              thisas->arg);
            pr_expr(thisas->replacement.ex);
            cc_msg("\n");
          }
          continue;
        }
        thisas->refsleft = YES;
      }
      blp = &bl->bindlistcdr;
    }
  }
  if (rc != NULL) {
    BindList *bl = BindListCopy(fn->fndetails.argbindlist);
    BindList *argbl = bl;
    n = length((List *)bl);
    rc->env = st.nullenv = (BindList *)nconc((List *)bl, (List *)rc->env);
    for (; --n >= 0; argbl = argbl->bindlistcdr) {
        Binder *b = argbl->bindlistcar;
        bindbl_(b) = argbl;
        bindstg_(b) |= b_bindaddrlist;
    }
  } else
    st.nullenv = NULL;
  fn->var_binders = FromGlobalBindListCopy(&st, fn->var_binders);
  fn->reg_binders = FromGlobalBindListCopy(&st, fn->reg_binders);
  { LabelNumber **labelindex = (LabelNumber **)SynAlloc(p->maxlabel * sizeof(LabelNumber *));
    BlockHead *b = fn->top_block;
    BlockHead *globtop = b;
    BlockHead *blast = NULL, *bnew = NULL;
    Inline_ArgDesc *ad;
    Inline_ArgSubstList *sl;
    for (n = 1; n < p->maxlabel; n++) labelindex[n] = nextlabel();
    fn->top_block = fn->bottom_block = NULL;
    for (; b != NULL; b = blkdown_(b), blast = bnew)
      if ( skipblock2 ? b != blkdown_(globtop) :
          map != NULL ? MapBitSet(map, (IPtr)blklab_(b)) :
                        YES) {
      int32 restoload = 0;
      bool resvoided = YES;
      bnew = (BlockHead *)BindAlloc(sizeof(BlockHead));
      *bnew = *b;
      blkup_(bnew) = blast;
      if (blast == NULL)
        fn->top_block = bnew;
      else
        blkdown_(blast) = bnew;
      if (blkflags_(b) & BLKSWITCH) {
        int32 n = blktabsize_(b);
        LabelNumber **sw_old = blktable_(b);
        LabelNumber **sw_new = (LabelNumber **)BindAlloc(n * sizeof(LabelNumber*));
        while (--n >= 0) sw_new[n] = FromGlobalLabel(labelindex, rc, sw_old[n]);
        blktable_(bnew) = sw_new;
      } else {
        BlockHead *b1 = b;
        if (b == dtorlast) {
          b1 = bottom;
          blkflags_(bnew) &= ~BLK2EXIT;
        } else {
          if (skipblock2 && blast == NULL)
            b1 = blkdown_(b);
          if (blkflags_(b) & BLK2EXIT)
            blknext1_(bnew) = FromGlobalLabel(labelindex, rc, blknext1_(b));
        }
        if (is_exit_label(blknext_(b1)) && rc != NULL) {
          restoload = (1L << rc->nresults) - 1;
          if (rc->nresults > 0 && rc->newresultregs[0] != GAP) resvoided = NO;
        }
        blknext_(bnew) = FromGlobalLabel(labelindex, rc, blknext_(b1));
      }
      { LabelNumber *lab = FromGlobalLabel(labelindex, rc, blklab_(b));
        blklab_(bnew) = lab;
        lab->block = bnew;
      }
      blkstack_(bnew) = FromGlobalSharedBindListCopy(&st, blkstack_(b));
      { int32 n_in = blklength_(b), n_out = n_in, n;
        Icode *code_old = blkcode_(b);
        Icode *code_new;
        bool copied = NO;
        VRegnum discardload = GAP;
        int32 noopcount = 0;
        for (n = n_in; --n >= 0; ) {
          J_OPCODE op = code_old[n].op & J_TABLE_BITS;
          if (op == J_ADCON) {
            Binder *b = code_old[n].m.b;
            if ((bindstg_(b) & bitofstg_(s_extern)) &&
                !(bindstg_(b) & (b_undef|b_fnconst)) &&
                !(binduses_(b) & (u_bss|u_constdata)) &&
                b != datasegment &&
                bindaddr_(b) != 0) {
              n_out++;
            }
          }
        }
        if (b == dtorlast) {
          n_out = n_out - 2 + blklength_(bottom);
          n_in = blklength_(bottom);
          code_old = blkcode_(bottom);
        }
        code_new = n_out == 0 ? (Icode *)DUFF_ADDR :
                                (Icode *)BindAlloc(n_out * sizeof(Icode));
        blkcode_(bnew) = code_new;
        blklength_(bnew) = n_out;
        for (n = n_out; --n, --n_in >= 0; ) {
          J_OPCODE op = code_old[n_in].op;
          VRegInt r1 = code_old[n_in].r1,
                  r2 = code_old[n_in].r2,
                  m = code_old[n_in].m;
          J_OPCODE opx = op & J_TABLE_BITS;
          if (uses_r1(op)) r1.r = vt[r1.r].r;
          if (uses_r2(op)) r2.r = vt[r2.r].r;
          if (uses_r3(op)) m.r = vt[m.r].r;
          if (opx == J_SETSPENV) {
            if (m.bl == r2.bl && !copied && rc != NULL) {
            /* m = r2 only when both are empty, just before function exits
               (deliberately not optimised out to give somewhere for
                CSE-introduced binders to be popped).
             */
              n_out--;
              goto maybeswitchblock;
            }
            m.bl = FromGlobalSharedBindListCopy(&st, m.bl);
            r2.bl = FromGlobalSharedBindListCopy(&st, r2.bl);
          } else if (opx == J_SETSPGOTO) {
            r2.bl = FromGlobalSharedBindListCopy(&st, r2.bl);
            m.l = FromGlobalLabel(labelindex, rc, m.l);
          } else if (opx == J_LDRV &&
                     (ad = ArgDesc_Find(m.b, p->args)) != NULL &&
                     (sl = ad->argsubst) != NULL) {
            switch (sl->sort) {
            default:        syserr("Inline_Restore"); break;
            case T_Binder:  m.b = sl->replacement.b; break;
            case T_Adcon:   m.b = sl->replacement.b; op = J_ADCON; break;
            case T_Int:     op = J_MOVK, m.i = intval_(sl->replacement.ex); break;
            case T_Plus:
              { Binder *b = (Binder *)arg1_(sl->replacement.ex);
                int32 k = intval_(arg2_(sl->replacement.ex));
                Icode *next1 = &code_new[n+1];
                J_OPCODE nextop = next1->op & J_TABLE_BITS;
                if (n+1 < n_out) {
                  if (uses_mem(nextop) && reads_r2(nextop) && !reads_r3(nextop) &&
                      next1->r2.r == r1.r) {
                    int32 k1;
#ifdef TARGET_LACKS_HALFWORD_STORE
                    if (nextop == J_STRBK &&
                        n+3 < n_out && IsSTRWExpansion(next1, &k1)) {
                      code_new[n+3].m.i += k;
                    }
#endif
                    m.b = b;
                    next1->m.i += k;
                    break;
                  } else if (nextop == J_MOVR && next1->m.r == r1.r) {
                    m.b = b;
                    next1->op = J_ADDK;
                    next1->r2.r = r1.r;
                    next1->m.i = k;
                    break;
                  }
                }
                if (loads_r1(op) && r1.r == discardload) {
                  op = J_NOOP;
                  noopcount++;
                  discardload = GAP;
                } else
                  m.b = FromGlobalBinderCopy(&st, m.b);
                break;
              }
            case T_AdconV:
              { Icode *next1 = &code_new[n+1];
                J_OPCODE nextop = next1->op & J_TABLE_BITS;
                m.b = sl->replacement.b;
                if ( n+1 < n_out &&
                     uses_mem(nextop) && reads_r2(nextop) && !reads_r3(nextop) &&
                     next1->r2.r == r1.r) {
                  int32 k;
                  if (sl->size == -1 || sl->size != ad->accesssize) {
#ifdef TARGET_LACKS_HALFWORD_STORE
                    if (nextop == J_STRBK &&
                        n+3 < n_out && IsSTRWExpansion(next1, &k)) {
                      Icode *next3 = &code_new[n+3];
                      next3->op = J_addvk(next3->op);
                      next3->r2 = next3->m;
                      next3->m.b = sl->replacement.b;
                    }
#endif
                    op = J_addvk(next1->op), r2 = next1->m;
                  } else {
#ifdef TARGET_LACKS_HALFWORD_STORE
                    if (nextop == J_STRBK &&
                        n+3 < n_out && IsSTRWExpansion(next1, &k) && k == 0) {
                      (next1+1)->op = J_NOOP;
                      (next1+2)->op = J_NOOP;
                      noopcount += 2;
                      op = J_STRV+J_ALIGN4;
                    } else
#endif
                    if (nextop == J_LDRBK || nextop == J_LDRWK)
                      op = J_LDRV+J_ALIGN4;
                    else if (nextop == J_STRWK || nextop == J_STRBK)
                      op = J_STRV+J_ALIGN4;
                    else
                      op = J_KtoV(next1->op);
                  }
                  r1 = next1->r1;
                  next1->op = J_NOOP;
                  noopcount++;
                } else
                  op = J_ADCONV;
              }
              break;
            }
          } else if (opx == J_ADCON) {
            m.b = FromGlobalBinderCopy(&st, m.b);
            if ((bindstg_(m.b) & bitofstg_(s_extern)) &&
                !(bindstg_(m.b) & (b_undef|b_fnconst)) &&
                !(binduses_(m.b) & (u_bss|u_constdata)) &&
                m.b != datasegment) {
              if (bindaddr_(m.b) != 0) {
                code_new[n].op = J_ADDK;
                code_new[n].r1 = r1;
                code_new[n].r2 = r1;
                code_new[n].m.i = bindaddr_(m.b);
                n--;
              }
              m.b = datasegment;
            }
          } else if (uses_stack(op) || opx == J_CALLK ||
                     opx == J_INIT || opx == J_INITF || opx == J_INITD)
            m.b = FromGlobalBinderCopy(&st, m.b);

          if (loads_r1(op)) {
            if (restoload != 0) {
              int i;
              for (i = 0; i < rc->nresults; i++)
                if (r1.r == rc->resultregs[i]) {
                  restoload &= ~regbit(i);
                  if (!resvoided) {
                    r1.r = rc->newresultregs[i];
                    blkflags_(b) |= BLKREXPORTED;
                  } else {
                    if (op == J_MOVR) discardload = m.r;
                    op = J_NOOP;
                    noopcount++;
                  }
                  break;
                }
            } else if (r1.r == discardload) {
              op = J_NOOP;
              noopcount++;
              discardload = GAP;
            }
          } else if (uses_r1(op) && r1.r == discardload)
            discardload = GAP;

          copied = YES;
          code_new[n].op = op;
          code_new[n].r1 = r1;
          code_new[n].r2 = r2;
          code_new[n].m = m;
maybeswitchblock :
          if (n_in == 0 && n > 0 && b == dtorlast) {
            n_in = blklength_(dtorlast) - 2;
            code_old = blkcode_(dtorlast);
            b = bottom;
          }
        }
        if (noopcount > 0) {
          int32 n = 0, n_in = 0;
          for (; n_in < n_out; n_in++)
            if (code_new[n_in].op != J_NOOP) {
              if (n != n_in)
                code_new[n] = code_new[n_in];
              n++;
            }
          n_out = n;
        }
        blklength_(bnew) = n_out;
      }
    }
    blkdown_(blast) = NULL;
    fn->bottom_block = blast;
  }
  { SynBindList *bl = st.copied;
    for (; bl != NULL; bl = bl->bindlistcdr)
      h0_(bl->bindlistcar) = s_binder;
  }
  { VRegnum n = p->maxreg;
    Inline_VRegIndex *vt = p->vregtypetab;
    while (!isany_realreg_(--n)) vt[n].rs = vregsort(vt[n].r);
  }
}

void Inline_Init() {
  saved_fns = NULL;
}

static void Inline_CompileOutOfLineCopy(Inline_SavedFn *fn) {
  char v[128];
  cg_sub_reinit();
  Inline_Restore(fn, NULL);
  currentfunction = fn->fndetails;
  top_block = fn->top_block; bottom_block = fn->bottom_block;
  if (debugging(DEBUG_CG)) {
    sprintf(v, "Out-of-line %s", symname_(fn->fndetails.symstr));
    flowgraf_print(v, NO);
  }
  cg_topdecl2(fn->var_binders, fn->reg_binders);
}

void Inline_Tidy() {
  SavedFnList *p;
  for (p = saved_fns; p != NULL; p = cdr_(p))
    if (p->usedoutofline) {
      { char v[128+5];
        strcpy(v, "x$i$"); strcpy(v+4, symname_(p->fn.fndetails.symstr));
        var_cc_private_flags |= 0x40000000;   /* set COMDEF attribute */
        codebuf_reinit1(v);
      }
      Inline_CompileOutOfLineCopy(&p->fn);
    }
}
