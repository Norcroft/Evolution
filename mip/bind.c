/*
 * bind.c: various binding and lexing routines for C compiler
 * Copyright (C) Codemist Ltd., 1987-1992.
 * Copyright (C) Acorn Computers Ltd., 1988-1990.
 * Copyright (C) Advanced RISC Machines Limited, 1991-1992.
 */

/*
 * RCS $Revision: 1.130 $  Codemist 55
 * Checkin $Date: 1996/01/10 14:54:08 $
 * Revising $Author: hmeeking $
 */

/* AM memo: @@@ the use of assert(0) in the development of PASCAL is    */
/* untidy but only temporary.                                           */

/* #define DEBUG_TENTATIVE_DEFS 1        -- BUT ONLY during development */

/* AM Dec-90: the BSS code was broken in several interesting ways.      */
/*   Mend it, and move PCC mode nearer to being ANSI code with only     */
/*   local differences.  u_tentative is now dead.                       */
/*   BSS is now merged with tentatives both in ansi and pcc mode.       */
/* AM Jul-87: experiment with memo-ising globalize_typeexpr(). */
/* exports globalize_int(), globalize_typeexpr() */

#ifndef _BIND_H
#include <stddef.h>         /* for offsetof() */
#include <string.h>
#include <ctype.h>
#include "globals.h"
#include "defs.h"
#include "aetree.h"
#include "util.h"           /* for padstrlen()... */
#include "codebuf.h"        /* for padstatic()... */
#include "cgdefs.h"         /* @@@ just for GAP */
#include "bind.h"
#include "builtin.h"
#include "lex.h"            /* for curlex... */
#include "sem.h"            /* for prunetype, equivtype... */
#include "store.h"
#include "vargen.h"         /* for initstaticvar()... */
#include "xrefs.h"          /* for LIT_LABEL */
#include "errors.h"
#include "aeops.h"
/* for C-only compilers... */
static int accessOK;
#define merge_default_arguments(a,b) 0
#define check_access(a,b) 0
#define nullbinder(cl) 0
#define set_linkage(b,l,m) 0
#endif

#ifdef PASCAL
/* for production qualify compilers prefer syserr() over assert().      */
#  include <assert.h>
#endif


static int gensymline, gensymgen;         /* For generating unique syms   */
static Symstr *(*hashvec)[BIND_HASHSIZE]; /* Symbol table buckets         */
char *sym_name_table[s_NUMSYMS];          /* translation back to strings  */

#define NO_CHAIN     256

#ifdef PASCAL
/* the following parameterisation allows PASCAL systems to use case-    */
/* insensitive name matching while preserving the original case.        */
#define lang_hashofchar(x) safe_tolower(x)
static int lang_namecmp(char *s, char *t)
{   for (;;)
    {   if (safe_tolower(*s) != safe_tolower(*t)) return 1;
        if (*s == 0) return 0;
        s++, t++;
    }
}
#else
#  define lang_hashofchar(x) (x)
#  define lang_namecmp(a, b) strcmp(a, b)
#endif

Symstr *(sym_lookup)(char const *name, int glo)
{   int32 wsize;
    unsigned32 hash, temp;
    char const *s;
    Symstr *next, **lvptr = NULL;
  /*
   * 'glo' ==  SYM_LOCAL  => allocate in Binder store
   *       ==  SYM_GLOBAL => allocate in Global store
   *  glo'  &  NO_CHAIN   => don't chain to symtab buckets
   */
    if (glo & NO_CHAIN)
        glo &= ~NO_CHAIN;
    else
    {   hash = 1;
        for (s = name; *s != 0; ++s)
        {   temp = just32bits_(hash << 7);
            hash = ((hash >> 25)^(temp >> 1)^(temp >> 4) ^
                                             lang_hashofchar(*s)) & 0x7fffffff;
        }
        lvptr = &(*hashvec)[hash % BIND_HASHSIZE];
        while ((next = *lvptr) != NULL)
        {   if (lang_namecmp(symname_(next), name) == 0) return(next);
            lvptr = &symchain_(next);
        }
    }
    wsize = offsetof(Symstr, symname[0]) + padstrlen(strlen(name));
    next = glo != SYM_LOCAL ? (Symstr *) GlobAlloc(SU_Sym, wsize)
                            : (Symstr *) BindAlloc(wsize);
    memclr(next, (size_t)wsize);
    if (lvptr != NULL)
    {   *lvptr = next;
        symchain_(next) = NULL;
    } else
        symchain_(next) = next;         /* non-hashed: see isgensym().  */
    symtype_(next) = s_identifier;
    bind_global_(next) = NULL; symlab_(next) = NULL; next->symtag = NULL;
    symext_(next) = NULL; symfold_(next) = NULL;
    strcpy(symname_(next), name);
    return(next);
}

Symstr *sym_insert(char const *name, AEop type)
{   Symstr *p = (sym_lookup)(name, SYM_GLOBAL);
    symtype_(p) = type;
    return(p);
}

Symstr *sym_insert_id(char const *name)
{   return (sym_lookup)(name, SYM_GLOBAL);
}

static void check_extern(Symstr *s)
{   Symstr *p;
/* First an option to check ansi conformance ... */
/* (The return below ensures only one is executed.  Because of room for  */
/* the back pointer only one can be active at once, unfortunately).      */
    if (feature & FEATURE_6CHARMONOCASE)
    {   char ch, v[6+1];
        int n = 0;
        while ((ch = symname_(s)[n]) != 0 && n < 6) v[n++] = safe_tolower(ch);
        v[n] = 0;
        p = sym_lookup(v, SYM_GLOBAL);
        if (symfold_(p) == 0)
            symfold_(p) = s;
        if (symfold_(p) != s)
            cc_warn(bind_warn_extern_clash, s, symfold_(p));
        return;
    }
/* ... now a compiled-in form for things like 370 linkers */
#ifdef TARGET_HAS_LINKER_NAME_LIMIT    /* e.g. 370 */
    {   char ch, v[LINKER_NAME_MAX+1];
        int n = 0;
        while ((ch = symname_(s)[n]) != 0 && n < LINKER_NAME_MAX)
            v[n++] = LINKER_NAME_MONOCASE ? safe_tolower(ch) : ch;
        v[n] = 0;
        p = sym_lookup(v, SYM_GLOBAL);
        if (symfold_(p) == 0)
            symfold_(p) = s;
        if (symfold_(p) != s)
            cc_err(LINKER_NAME_MONOCASE ? bind_err_extern_clash_monocase,
                                        : bind_err_extern_clash,
                   s, symfold_(p), (long)LINKER_NAME_MAX);
    }
#endif /* TARGET_HAS_LINKER_NAME_LIMIT */
}

Symstr *gensymval(bool glo)
{
    /* Generates a new symbol with a unique name.                    */
    /* (Not quite unique in that line 1 may occur in 2 files, but    */
    /*  we RELY on NO_CHAIN below to treat as unique).               */
    char name[30];
#ifdef TARGET_IS_CLIPPER
    /* the next line is a hack to ensure these gensyms are assemblable */
    /* Note that this HOPEs that the user has no names Intsym_nnn      */
    sprintf(name, "Intsym_%d", ++gensymgen);
#else
    if (gensymline != curlex.fl.l)
        gensymline = curlex.fl.l, gensymgen = 1;
    else
        ++gensymgen;
    sprintf(name, "<Anon%d_at_line_%d>", gensymgen, gensymline);
#endif
    return sym_lookup(name, (glo ? SYM_GLOBAL+NO_CHAIN : SYM_LOCAL+NO_CHAIN));
}

Symstr *gensymvalwithname(bool glo, char const *name)
{
    return sym_lookup(name, (glo ? SYM_GLOBAL+NO_CHAIN : SYM_LOCAL+NO_CHAIN));
}

bool isgensym(Symstr const *sym)      /* actually is-unchained-gensym!        */
{   return symchain_(sym) == sym;
}

Binder *global_mk_binder(Binder *b, Symstr *c, SET_BITMAP d, TypeExpr *e)
{
    int32 size = d & (bitofstg_(s_virtual)|b_globalregvar|bitofstg_(s_auto)) ?
                     sizeof(Binder) : SIZEOF_NONAUTO_BINDER;
    Binder *p = (Binder*) GlobAlloc(SU_Bind, size);
 /*
  * This consistency check is removed so that front-ends for languages
  * other than C can create binders for auto variables in global store.
>>> if (d & bitofstg_(s_auto)) syserr("Odd global binder(%lx)", (long)d); <<<
  */
    if (d & bitofstg_(s_extern)) check_extern(c);
    h0_(p) = s_binder;
    bindcdr_(p)=b;
    bindsym_(p)=c;
    attributes_(p) = A_GLOBALSTORE;
    bindstg_(p) = d;
    bindtype_(p) = e;
    bindaddr_(p) = 0;  /* soon BINDADDR_UNSET - remember 'datasegment' */
    bindparent_(p) = 0;         /* local_scope->class_tag? */
    bindconst_(p) = 0;
    bindinline_(p) = 0;
#ifdef PASCAL /*ECN*/
    p->bindlevel = 0;
    p->synflags = 0;
#endif
    if (size == sizeof(Binder))
        bindxx_(p) = GAP;
    if (d & bitofstg_(s_auto))
        bindmcrep_(p) = NOMCREPCACHE;
    return p;
}

Binder *mk_binder(Symstr *c, SET_BITMAP d, TypeExpr *e)
{
    Binder *p = (Binder*) BindAlloc(
        (d & (bitofstg_(s_auto)|bitofstg_(s_virtual))) ?
            sizeof(Binder) : SIZEOF_NONAUTO_BINDER);
    if (d & bitofstg_(s_extern)) check_extern(c);
    h0_(p) = s_binder;
    bindcdr_(p)=0;
    bindsym_(p)=c;
    attributes_(p) = A_LOCALSTORE;
    bindstg_(p) = d;
    bindtype_(p) = e;
    bindaddr_(p) = BINDADDR_UNSET;
    bindparent_(p) = 0;         /* local_scope->class_tag? */
    bindconst_(p) = 0;
    bindinline_(p) = 0;
#ifdef PASCAL /*ECN*/
    p->bindlevel = 0;
    p->synflags = 0;
#endif
    if (d & bitofstg_(s_auto))
    {   bindxx_(p) = GAP;
        bindmcrep_(p) = NOMCREPCACHE;
    }
    return p;
}

TagBinder *global_mk_tagbinder(TagBinder *b, Symstr *c, AEop d)
{   SET_BITMAP bits = bitoftype_(d);
    TagBinder *p = (TagBinder*) GlobAlloc(SU_Bind, sizeof(TagBinder));
    h0_(p) = s_tagbind;
    tagbindcdr_(p)=b;
    tagbindsym_(p)=c;
    attributes_(p) = A_GLOBALSTORE;
    tagbindbits_(p) = bits;
    tagbindmems_(p) = 0;
    tagbindtype_(p) = globalize_typeexpr(primtype2_(bits, p));
    if (LanguageIsCPlusPlus)
    {   p->friends = NULL;
        p->tagparent = current_member_scope();
    } else
        p->tagparent = NULL;
    p->tagtext = -1;
#ifdef TARGET_HAS_DEBUGGER
    p->tagbinddbg = 0;
#endif
    return p;
}

static TagBinder *mk_tagbinder(Symstr *c, AEop d)
{   SET_BITMAP bits = bitoftype_(d);
    TagBinder *p = (TagBinder*) SynAlloc(sizeof(TagBinder));
    h0_(p) = s_tagbind;
    tagbindcdr_(p)=0;
    tagbindsym_(p)=c;
    attributes_(p) = A_LOCALSTORE;
    tagbindbits_(p) = bits;
    tagbindmems_(p) = 0;
    tagbindtype_(p) = primtype2_(bits, p);
    if (LanguageIsCPlusPlus)
    {   p->friends = NULL;
        p->tagparent = current_member_scope();
    } else
        p->tagparent = NULL;
    p->tagtext = -1;
#ifdef TARGET_HAS_DEBUGGER
    p->tagbinddbg = 0;
#endif
    return p;
}

extern LabBind *mk_labbind(LabBind *b, Symstr *c)
{
    LabBind *p = (LabBind*) SynAlloc(sizeof(LabBind));
    p->labcdr = b;
    p->labsym = c;
    p->labinternlab = 0;
    p->labuses = 0;
    p->labu.ref = 0;
    return p;
}

/* Binding:
   There are 5 overloading classes, of which 3 (labels, vars, struct tags)
   are bindings in the traditional sense.  All code concerning binding
   and unbinding is in this file.  Access routes are the procedures below:
     Labels:  label_xxx;
     Vars:    instate_declaration, findbinding.
     Tags:    instate_tagbinding, findtagbinding.
     Scopes:  push_scope, pop_scope, clear_stacked_scopes.

   Note that typedef names share the same binding space with variables.

   Labels have function scope and function scopes do not nest. Function
   scopes and the global (file) scope for variables and tags is implemented
   using a hash table of Symstrs with separate Binder, TagBinder and LabBind
   pointer fields.

   Local scopes for Binders and TagBinders are implemented using the 'deep
   binding' strategy, as are C++ class scopes. For each scope there is a
   list of Binders/ TagBinders which can be searched for the matching
   Symstr. If the search fails in each local scope then the global
   binding (if any) is found in O(1) time.

   In a C++ class scope, class members, class-scope Binders and class-scope
   TagBinders exist on a single list, discrimiated by the leading AEop field
   of each entry (s_binder, s_tagbind, s_member).

   APOLOGY: This assumes that Binder, TagBinder and ClassMember all begin:
   {AEop sort;  SelfType *cdr;  Symstr *sv; ...} so we can pun. It can be
   made cleaner with a common initial structure and casts, but this spreads
   the filth over several modules rather than localising it here in bind.c.

   NOTE: binding information persists only for the duration of parsing -
   consequently it makes sense for the parse tree to contain references to
   binding records rather than the main symbol table entries. Toplevel
   binding are allocated in 'global' store which is not reclaimed after each
   top-level phrase.

   NOTE: This also means that local Binders/TagBinders may not be inspected
   during/after register allocation (which reuses syntax store).

*/

/* AM: create a globalized Binder.  For use in rd_decl and
   implicit_decl.  Beware: its components still need globalizing.
   Possible optimisation: overwrite if already there on re-definition.
   Precondition to call: loc must not represent a local binding.
*/
#define topbind2(sv, stg, typ) \
   (bind_global_(sv) = topbindingchain = \
        global_mk_binder(topbindingchain, sv, stg, typ))

static Binder *topbindingchain;                                 /* vars */
static TagBinder *toptagbindchain;                              /* tags */
static LabBind *labelchain;                                     /* labels */

typedef struct Scope {
    struct Scope *prev;
    Binder *scopemems;          /* A scope either contains Binders      */
                                /* and TagBinders (a local scope)       */
                                /* and members too (class scope).       */
                                /* (unused if class_tag != 0).          */
    TagBinder *class_tag;
    bool arg_scope;
} Scope;

static Scope *local_scope, *freeScopes;
static bool tag_found_in_local_scope;
        /* used to avoid searching twice in instate_xxx... */
static int scope_level;

static int push_init_scope(TagBinder *class_tag, ScopeSaver init,
        bool arg_scope)
{   Scope *scope = freeScopes;
    if (scope != NULL)
        freeScopes = freeScopes->prev;
    else
        scope = (Scope *) GlobAlloc(SU_Bind, sizeof(Scope));
    if (debugging(DEBUG_BIND))
        cc_msg("push_scope($b) -> %d\n", class_tag, scope_level);
    scope->scopemems = init;
    scope->prev = local_scope;
    local_scope = scope;
    scope->class_tag = class_tag;
    scope->arg_scope = arg_scope;
    return scope_level++;
}

int push_scope(TagBinder *class_tag, bool arg_scope)
{   return push_init_scope(class_tag, 0, arg_scope);
}

int push_var_scope(ScopeSaver init)
{   return push_init_scope(0, init, NO);
}

/* The pop_scope routines return values giving the (last) popped        */
/* scope so this can be restored for argument scopes in fn defs.        */
static ScopeSaver pop_scope_1(int level, bool check_unrefd)
{   Scope *scope = local_scope;
    ScopeSaver poppling = 0;
    Binder *p;
    if (level > scope_level)
        syserr("pop_scope: level=%d >= scope_level=%d", level, scope_level);
  while (scope_level > level)
  { if (debugging(DEBUG_BIND)) cc_msg("pop_scope(%d)\n", scope_level);
    if (scope == NULL) syserr("pop_scope: NULL scope pointer");
    poppling = scope->scopemems;        /* empty for class scopes       */
    if (scope->class_tag == NULL && check_unrefd)
    {   /* Check for unreferenced names in local scopes */
        Symstr *sv;
        for (p = poppling;  p != 0;  p = bindcdr_(p))
        {   if (h0_(p) != s_binder || h0_(bindtype_(p)) == t_ovld) continue;
            sv = bindsym_(p);
            /* do a bit more in the next line for used/set */
            /* suppress warning for gensym'd vars, which patch up user errs */
            if (!(binduses_(p) & u_referenced) &&
                !isgensym(sv) &&
                !(bindstg_(p) & b_pseudonym))
            {
#ifdef CPLUSPLUS
                if (sv == thissym)
                    cc_warn(bind_warn_unused_this_in_member);
                else
#endif
                  if (bindstg_(p) & bitofstg_(s_typedef))
                    cc_warn(bind_warn_typedef_not_used, p);
                  else if (bindstg_(p) & b_fnconst)
                    cc_warn(bind_warn_function_not_used, p);
                  else
                    cc_warn(bind_warn_variable_not_used, p);
            }
        }
    }
    else
    /* The next line fixes nasties like:                                */
    /* "struct d { struct d { int a; } c; } x;"  by inhibiting the      */
    /* outer setting and so recovering to "struct d { int a; }.         */
    {   TagBinder *b = scope->class_tag;
        scope->class_tag = NULL;                /* @@@ kill... */
        /* This is only for C: C++ has already closed the class (in     */
        /* cpp_end_strdecl). Hence no need to do this for the core class*/
        if (b != NULL && (tagbindbits_(b) & TB_BEINGDEFD))
            tagbindbits_(b) = (tagbindbits_(b) & ~TB_BEINGDEFD) | TB_DEFD;
    }
    local_scope = scope->prev;
    scope->prev = freeScopes;
    freeScopes = scope;
    scope = local_scope;
    --scope_level;
  }
  return poppling;
}

ScopeSaver pop_scope(int level)
{   return pop_scope_1(level, YES);
}

ScopeSaver pop_scope_no_check(int level)
{   return pop_scope_1(level, NO);
}

static Scope *set_local_block_scope(void)
{   Scope *scope0 = local_scope;
    if (!LanguageIsCPlusPlus)
    {   Scope *scope;
        for (scope = scope0;  scope != 0;  scope = scope->prev)
            if (scope->class_tag == 0) break;
        /* Intentionally, if local_scope is 0, instate_declaration_1()  */
        /* will syserr(). Note: this is the ONLY caller...              */
        local_scope = scope;
    }
    return scope0;
}

static TagBinder *findtag_in_members(Symstr *sv, ClassMember *ll)
{   TagBinder *l = (TagBinder *)ll;
    for (; l != NULL;  l = tagbindcdr_(l))
    {   if (debugging(DEBUG_BIND))
            cc_msg("findtag try $r %lx\n", memsv_(l), (long)attributes_(l));
        if (h0_(l) == s_tagbind &&
            (h0_(sv)==s_tagbind && l == (TagBinder *)sv || bindsym_(l)==sv))
                break;
    }
    return l;
}

TagBinder *findtagbinding(Symstr *sv, TagBinder *cl, int fbflags)
{   Scope *scope;
    bool first = 1;
    tag_found_in_local_scope = 0;
    /* assert: cl != 0 <=> fbflags == INCLASSONLY.                      */
    /* @@@ note that we don't inherit classes via bases!                */
    if (cl != NULL)
        return
            findtag_in_members(sv, tagbindmems_(core_class(cl)));

    if (fbflags == ALLSCOPES)
        for (scope = local_scope;  scope != NULL;  scope = scope->prev)
        {   ClassMember *l;
            TagBinder *b;
            if ((cl = scope->class_tag) != NULL)
            {   if (LanguageIsCPlusPlus)
                    l = tagbindmems_(core_class(cl));
                else
                    continue;           /* C class scopes don't nest.   */
            }
            else
                l = scope->scopemems;
            if ((b = findtag_in_members(sv, l)) != 0)
                return (tag_found_in_local_scope = first, b);
            first = 0;
        }
    return tag_global_(sv);
}

/* routine finds both members and binders -- in C these can be checked  */
/* to be consistent by caller.  In C++ they are equivalent.             */
/* find_scopemember finds ANY suitably named member, but will get       */
/* the nearest in case of ambiguity.                                    */
/* For anonymous unions it returns the whole union, not the element.    */
static ClassMember *find_scopemember(Symstr *sv, ClassMember *p)
{   ClassMember *l;
    for (l = p;  l != NULL;  l = memcdr_(l))
    {   if (debugging(DEBUG_BIND))
            cc_msg("findscopemember try $r %lx\n",
                memsv_(l), (long)attributes_(l));
        if ((h0_(l) == s_member || h0_(l) == s_binder) && memsv_(l) == sv)
            return l;
    }
    return 0;
}

static TagBinder *qualifyingBase;   /* extra IN arg to path_to_member_1 */

static Expr *path_to_member_1(ClassMember *member, TagBinder *tb, int flags,
        ClassMember *vbases, TagBinder *privately_deriving_class)
{   ClassMember *l;
    int32 sort = tagbindbits_(tb) & CLASSBITS;

    /* This can never happen C-only... */
    if (qualifyingBase != 0 &&
        (qualifyingBase == tb || core_class(qualifyingBase) == tb))
            flags &= ~FB_NOTYET;

if (!(flags & FB_NOTYET))
{   if ((tagbindbits_(tb) & TB_DEFD) && !(tagbindbits_(tb) & TB_SIZECACHED))
        (void)sizeofclass(tb, NULL);

    for (l = tagbindmems_(tb);  l != NULL;  l = memcdr_(l))
    {   if (debugging(DEBUG_BIND))
            cc_msg("see $r %lx\n", memsv_(l), (long)attributes_(l));
        if (h0_(member) == s_identifier)
        {   Symstr *sv = (Symstr *)member;
            if (memsv_(l) != sv) continue;
        }
        else if (l != member)
            continue;

/* The next line avoids spurious errors for class-within-class defn.    */
/* (path_to_member() ignores s_tagbind's.)                              */
        if (h0_(l) == s_tagbind) continue;

        curlex_member = l;  /* this saves some searching and helps with */
                            /* diagnosing access faults.                */
        if (LanguageIsCPlusPlus)
        {   if (memtype_(l) == ACCESSADJ) /* access decl */
            {   accessOK = 1;
                continue;
            }
            check_access(l, privately_deriving_class);
        }
        /* the following save store when searching for tyenames...      */
        if (flags & (FB_CLASSNAME|FB_TYPENAME|FB_FNBINDER))
            return (Expr *)l;

        if (LanguageIsCPlusPlus)
        {   if (h0_(l) == s_binder)
            {   Binder *b = (Binder *)l;
                if (bindstg_(b) & b_pseudonym) b = realbinder_(b);
                if (!(flags & FB_FNBINDER)
                    &&
                    /* BEWARE: a local memfn_a is static... */
                    ( (bindstg_(b) & b_memfna) ||
                     !(bindstg_(b) & (bitofstg_(s_static)|bitofstg_(s_typedef)))
                    )
                    &&
                /* overloaded fns can't be typedefs; use ovld-stg bit?  */
                /* match both generic and specific functions.           */
                    (h0_(bindtype_(b)) == t_ovld || h0_(bindtype_(b)) == t_fnap)
                   )
                    /* exprdotmemfn_() */
                    return mk_exprwdot(s_dot, bindtype_(b),
                        nullbinder(bindparent_(b)), (IPtr)b);
                /* Otherwise: a typedef name, static member, enumerator */
                /* or a function binder was requested via FB_FNBINDER.  */
                return (Expr *)b;
            }
        }
        /* Assert: h0_(l) == s_member, whether C or C++ */
        return mk_exprbdot(s_dot, memtype_(l), nullbinder(tb), memwoff_(l), membits_(l),
            target_lsbitfirst ? MAXBITSIZE-membits_(l)-memboff_(l) : memboff_(l));
    }
}
    if (LanguageIsCPlusPlus && (flags & FB_INHERIT))
        return path_to_base_member(member, tb, flags, vbases,
                    privately_deriving_class);
    return NULL;
}

Expr *path_to_member(ClassMember *member, TagBinder *b, int flags)
{   Expr *e;
    if (b == 0) syserr("path_to_member(0,...)");
    if (member == NULL) return NULL;
    if (debugging(DEBUG_BIND))
    {   Symstr *sv = h0_(member) == s_identifier ?
            (Symstr *)member : memsv_(member);
        cc_msg("path_to_member($r, $b, 0x%.3x) at $l\n", sv, b, flags);
    }
    if (LanguageIsCPlusPlus)
    {   accessOK = 0;
        curlex_member = 0;
        {   ClassMember *l = tagbindmems_(b);
            if (l != NULL && !(attributes_(l) & CB_CORE)) l = NULL;
            e = path_to_member_2(member, b, flags, l, NULL);
        }
        if (e != 0 && h0_(e) == s_invisible && !(flags & FB_KEEPI))
            e = arg2_(e);
    }
    else
        e = path_to_member_1(member, b, flags, NULL, NULL);
    return e;
}

Expr *findpath(Symstr *sv, TagBinder *cl, int flags, TagBinder *inBase)
{   Scope *scope;
    ClassMember *member = (ClassMember *)sv;
    if ((qualifyingBase = inBase) != NULL) flags |= FB_NOTYET;
    if (cl != NULL)
    {   Expr *e;
        e = path_to_member(member, cl, flags);
        return e;
    }
    for (scope = local_scope;  scope != NULL;  scope = scope->prev)
    {   Binder *b = NULL;
        cl = scope->class_tag;
        if (cl != NULL && (flags & FB_CLASSES))
            b = (Binder *)path_to_member(member, cl, flags);
        else if (cl == NULL && (flags & FB_LOCALS))
            b = find_scopemember(sv, scope->scopemems);
        if ((flags & FB_THISSCOPE)
            ||
            b != NULL && (
             LanguageIsCPlusPlus &&
             ((flags & FB_CLASSNAME) == 0 ||
               h0_(b) == s_binder && (bindstg_(b) & bitofstg_(s_typedef)) &&
               isclasstype_(princtype(bindtype_(b)))
             )
             ||
            !LanguageIsCPlusPlus &&
             ((flags & FB_TYPENAME) == 0 ||
              (cl == NULL) ||
              h0_(b) == s_binder && (bindstg_(b) & bitofstg_(s_typedef))
             )
           ))
           return (Expr *)b;
    }
    return (Expr *)bind_global_(sv);
}

Binder *findbinding(Symstr *sv, TagBinder *cl, int flags)
{   return (Binder *)findpath(sv, cl, flags | FB_FNBINDER, 0);
}

void add_toplevel_binder(Binder *b)
{   bind_global_(bindsym_(b)) = b;
    bindcdr_(b) = topbindingchain;
    topbindingchain = b;
}

static Binder **insertionpoint(Scope *scope)
{
/* Now we are supposedly about to ADD to a partially made scope.        */
/* Hence we must have either a local scope or a TB_BEINGDEFD class.     */
/* We need to worry about CB_CORE for partially made class scopes...    */
    TagBinder *cl = scope->class_tag;
/* duplicate names must be seen first for local (auto) scopes since     */
/* argument narrowing relies on it, similarly members must be placed    */
/* in proper order (at least eventually):                               */
    if (cl == 0)
        return &scope->scopemems;
    else
    {   ClassMember **q, *l;
        if (!(tagbindbits_(cl) & TB_BEINGDEFD))
            syserr("adding to completed scope $c", cl);
        cl = core_class(cl);
        q = &tagbindmems_(cl);
        while ((l = *q) != NULL) q = &bindcdr_(l);
        return q;
    }
}

static void add_local_binder(Binder *b)
{   Scope *scope = local_scope;
    Binder **p = insertionpoint(scope);
    if (debugging(DEBUG_BIND))
    {   cc_msg("add_local_binder($b) in scope %p\n", b, scope);
        if (debugging(DEBUG_TYPE))
            cc_msg("princtype %ld\n", h0_(bindtype_(b)));
    }
    bindcdr_(b) = *p;
    *p = b;
}

static void add_local_tagbinder(TagBinder *b, bool implicit)
/* Note that in C, class scopes don't nest; see also findtagbinding().  */
/* 'implicit' is set when we have a non-explicit declaration which is   */
/* not a definition of a class tag, within another class or a formal    */
/* parameter list. Lift to first unnamed scope.                         */
{   Scope *scope = local_scope;
    if (implicit || !LanguageIsCPlusPlus)
        while (scope && (scope->class_tag != NULL ||
                         scope->arg_scope && LanguageIsCPlusPlus))
            scope = scope->prev;
    if (scope == NULL)
    {   if (!LanguageIsCPlusPlus) syserr("add_local_tagbinder");
        tagbindcdr_(b) = toptagbindchain;
        tag_global_(tagbindsym_(b)) = toptagbindchain = b;
    }
    else
    {   TagBinder **p = (TagBinder **)insertionpoint(scope);
        tagbindcdr_(b) = *p;
        *p = b;
    }
    if (LanguageIsCPlusPlus)
    {   /* local classes MUST have INTERNAL linkage... */
        while (scope && (scope->class_tag != NULL || scope->arg_scope))
            scope = scope->prev;
        if (scope != NULL) set_linkage((Binder *)b, A_NOLINKAGE, NULL);
    }
}

static void add_member(ClassMember *m)
{   Scope *scope = local_scope;
    ClassMember **p = insertionpoint(scope);
    if (debugging(DEBUG_BIND))
        cc_msg("add_member($r) in scope %p\n", memsv_(m), scope);
    memcdr_(m) = *p;
    *p = m;
}

static ClassMember *instate_member_1(DeclRhsList *d, int bindflg)
{   ClassMember *m;
    int32 bitsize;
    TypeExpr *t;
    Symstr *sv = d->declname;
    SET_BITMAP access = attribofstgacc_(d->declstg);
/* note: other users of d->declstg should use killstgacc_(d->declstg).  */
    if (access==0) syserr("instate_member(access==0)");
    m = (sv == NULL) ? NULL :
        find_scopemember(sv, tagbindmems_(local_scope->class_tag));
        /* LDS: 29-Mar-95: Changed (O(N**2) performance reasons) from:  */
        /*      findbinding(sv, local_scope->class_tag, INCLASSONLY);   */
    if (m != NULL)
    {   if (bindstg_(m) & d->declstg & bitofstg_(s_typedef))
        {   if ((d->declstg & bitofstg_(s_typedef)) &&
                (d->declstg & u_implicitdef))
                return NULL;
            else if ((bindstg_(m) & bitofstg_(s_typedef)) &&
                     (bindstg_(m) & u_implicitdef))
                /* supercede it... */
                bindstg_(m) |= u_referenced;
            else
                syserr("instate_member_1");
        } else
        {   cc_rerr(syn_rerr_duplicate_member(sv, local_scope->class_tag));
            d->declname = gensymval(1);
        }
    }
/* bindflg is set so that all structs are globalized except within fns. */
/* This includes structs declared in formal parameter lists whose scope */
/* is only the function.                                                */
    t = d->decltype;
/*/* @@@ LDS 05-Oct-94: the first ! was missing - what is really meant? */
/* Do we still need to globalize for local classes??                    */
    if (!LanguageIsCPlusPlus && !(bindflg & (GLOBALSTG|TOPLEVEL)))
    {   m = (ClassMember *)BindAlloc(SIZEOF_CLASSMEMBER);
    }
    else
    /* always allocate globally for C++ local class memfns.             */
    {   m = (ClassMember *)GlobAlloc(SU_Bind, SIZEOF_CLASSMEMBER);
        t = globalize_typeexpr(t);
    }
    /* We now evaluate a bitfield's size here, rather than later: by so */
    /* doing, membits no longer tells us whether a member is a bitfield */
    /* but this is only a consistency check since memtype & BITFIELD is */
    /* what really distinguishes bitfields.                             */
    bitsize = (declbits_(d) == NULL) ? 0 : evaluate(declbits_(d));
    h0_(m) = s_member;
    memcdr_(m) = NULL;
    memsv_(m) = sv;                    /* 0 for padding (:0) bit fields */
    memtype_(m) = t;
    memwoff_(m) = OFFSET_UNSET; memboff_(m) = 0; membits_(m) = bitsize;
    attributes_(m) = access;
    bindstg_(m) = 0;                    /* b_member? */
    bindparent_(m) = local_scope->class_tag;
    add_member(m);
    return m;
}

/* struct/union/enum tag bindings ... */

TagBinder *instate_tagbinding(Symstr *sv, AEop s, TagDefSort defining,
            int bindflg, bool *newtag)
{   TagBinder *b;
    if (sv == 0)
    {   sv = gensymval(1);
        defining = TD_ContentDef;
    }
    *newtag = NO;
    if (bindflg & TOPLEVEL)
    {   b = tag_global_(sv);
        if (b == 0 || defining != TD_NotDef)
        {   /* new tag or tag being defined... */
            if (debugging(DEBUG_BIND))
                cc_msg("top level struct $r@%p\n", sv, (VoidStar)b);
            if (b == 0)
                /* introduction of new tag */
            {   *newtag = YES;
                tag_global_(sv) = toptagbindchain = b =
                    global_mk_tagbinder(toptagbindchain,sv,s);
                if (b->tagparent != 0) syserr("instate_tagbinding($b)", b);
                if (!LanguageIsCPlusPlus &&
                    defining != TD_NotDef &&
                    local_scope != 0 &&
                    !isgensym(sv))
                    cc_warn(bind_warn_cpp_scope_differ, b);
            }
            else if (((tagbindbits_(b) & TB_DEFD) && defining != TD_Decl) ||
                      /* re-definition */
                     (tagbindbits_(b) & TB_BEINGDEFD))
                cc_err(bind_err_duplicate_tag, tagbindsort(b),b);
        }
    }
    else
    {   b = findtagbinding(sv, 0, ALLSCOPES);
        if (b == 0 || defining != TD_NotDef)
        {   /* new tag or tag being defined... */
            bool implicit = 0;
            if (debugging(DEBUG_BIND))
                cc_msg("local struct $r@%p\n", sv, (VoidStar)b);
            if (b != 0 && tag_found_in_local_scope &&
                (((tagbindbits_(b) & TB_DEFD) && defining != TD_Decl)
                 ||  /* re-definition */
                 (tagbindbits_(b) & TB_BEINGDEFD)))
            {
                cc_err(bind_err_duplicate_tag, tagbindsort(b), b);
                b = 0;
            }
            if (b == 0 ||
                defining == TD_ContentDef && !tag_found_in_local_scope)
            {   *newtag = YES;
/* bindflg & GLOBALSTG refers to tags in formals: these need careful    */
/* treatment in that they are somewhat visible.  e.g. equivtype needs   */
/* to see "f(struct a { int b,c;})" differing from g of similar type.   */
/* For C++, always allocate even local structs in global store since    */
/* they may have member fns.                                            */
                b = (bindflg & GLOBALSTG) || LanguageIsCPlusPlus ?
                    global_mk_tagbinder(0, sv, s) : mk_tagbinder(sv, s);
/* The next lines avoids 'true' scoping as a hack to make B=B in:       */
/*      class A { class B *p; }; class B { whatever };                  */
/* Also in class A {friend class B; }; class B { whatever };            */
                if (defining == TD_NotDef)
                    implicit = 1;
                else if (!LanguageIsCPlusPlus)
                {   if (!isgensym(sv) && current_member_scope() != 0)
                        cc_warn(bind_warn_cpp_scope_differ, b);
                    implicit = 1;
                }
                if (implicit) b->tagparent = 0;
                add_local_tagbinder(b, implicit);
            }
        }
    }
    if ((tagbindbits_(b) & ENUMORCLASSBITS) != bitoftype_(s) &&
        ((tagbindbits_(b) & (bitoftype_(s_union)|bitoftype_(s_enum)))
         || s == s_union || s == s_enum))
        cc_err(bind_err_reuse_tag, tagbindsort(b), b, s);
    if (defining == TD_ContentDef)
        tagbindbits_(b) = (tagbindbits_(b) & ~ENUMORCLASSBITS) | bitoftype_(s);
/* AM: the next test was != TD_NotDef but BEINGDEFD should not be set   */
/* after "struct A;" (to match with "struct A *p").                     */
/* Has anyone started using TB_BEINGDEFD to distinguish?                */
    if (defining == TD_ContentDef) tagbindbits_(b) |= TB_BEINGDEFD;
    return b;
}

/* variable and typedef bindings... */

Binder *implicit_decl(Symstr *a, int32 fn)
{
/* implicit declaration of 'extern int x' or 'extern int f()'  */
/* N.B. the information has to be generated in the global heap */
    TypeExpr *t = te_int;
    TypeExprFnAux s;
    if (fn)
        t = g_mkTypeExprfn(t_fnap, t, 0, 0,
            packTypeExprFnAux(s, 0, LanguageIsCPlusPlus ? 1999 : 999, 0, 0,
                fpregargs_disabled ? f_nofpregargs : 0));   /* minargs_ */
    topbind2(a, (fn ? bitofstg_(s_extern)|b_undef|b_fnconst :
                bitofstg_(s_extern)|b_undef), t);
    binduses_(topbindingchain) |= u_implicitdef;
    return topbindingchain;
}

/*
 * Used below and in instate_declaration for tentative Ansi definitions.
 * To say that this code is NASTY is a gross understatement. In fact,
 * it is hard to describe its nastiness in mere words.
 * @@@ AM (Sep 89) wants to re-work all this structure soon.
 */
typedef struct TentativeDefn
{
    struct TentativeDefn *next;
    DataDesc   data;
    int32      size, align;
    int32      elt_size;                              /* for open array */
/* When TARGET_HAS_BSS is NOT set, maybebss=1 <==> size=0.              */
    bool       maybebss;
    bool       statik;
    Symstr     *sv;
} TentativeDefn;

static TentativeDefn *tentative_defs;     /* also init'd by bind_init */
#ifndef TARGET_IS_INTERPRETER
static DataInit *datahead,
                *datasplice,
                *datatail;
static TentativeDefn saved_vg_state;
#endif

#ifndef TARGET_IS_INTERPRETER
static void save_vargen_state(TentativeDefn *td)
{
    td->data.head = datap->head; td->data.tail = datap->tail;
    td->data.size = datap->size;
}

static void restore_vargen_state(TentativeDefn *td)
{
    datap->head = td->data.head; datap->tail = td->data.tail;
    datap->size = td->data.size;
}
#endif

/*
 * Used to make a tentative defns list for the following routines.
 * Basically, this routine is called to record information about
 * the state of vargen.c before a zero initialiser for a tentative
 * static is processed.
 * It also holds whether we hope to allocate a top-level variable in bss.
 */
static bool addTentativeDefn(Symstr *sv, int32 size, int32 elt_size, int32 align, bool statik)
{
    TentativeDefn *td = NULL;
    for (td = tentative_defs; td != NULL; td = td->next)
        if (td->sv == sv) break;
    if (td == NULL)
    {   td = (TentativeDefn *) GlobAlloc(SU_Other, sizeof(TentativeDefn));
        td->data.head = td->data.tail = (DataInit *)DUFF_ADDR;
        td->data.size = 0;
        td->next      = tentative_defs;
        td->size      = 0;
        td->elt_size  = elt_size;
        td->align     = align;
        td->sv        = sv;
        td->maybebss  = YES;
        td->statik    = statik;
        tentative_defs= td;
    }
/* The following test fills in the size details in:  int a[],a[100];    */
    if (size != 0)
    {   td->size = size;
#ifdef TARGET_HAS_BSS
        td->maybebss = (feature & FEATURE_PCC) || (size > BSS_THRESHOLD);
#else
        td->maybebss = 0;
#endif
#ifndef TARGET_IS_INTERPRETER
        save_vargen_state(td); /* size = 0 => tentative foo[] */
#endif
    }
#ifdef DEBUG_TENTATIVE_DEFS
if (debugging(DEBUG_BIND)) cc_msg(
"addTentativeDefn %lx, %lx next %lx (%s) head %lx tail %lx loc %ld name %s size %ld\n",
    (int32)datap,
    (int32) td, (int32) td->next, (td->next) ? symname_(td->next->sv) : "",
    (int32)datap->head, (int32)datap->tail, (int32)datap->size, symname_(sv), size);
#endif
    return td->maybebss;
}

#ifdef DEBUG_TENTATIVE_DEFS

static void show_vargen_state(char *when)
{
    if (debugging(DEBUG_BIND))
    {   DataInit *tmpdataq;
        cc_msg("vargen state %lx %s restoration:-\n", (int32)datap, when);
        for (tmpdataq = datap->head; tmpdataq != 0; tmpdataq = tmpdataq->datacdr)
        {
            cc_msg(
                "DataInit %lx: cdr %lx rpt %ld sort %lx len %ld val %ld\n",
                (int32) tmpdataq, (int32) tmpdataq->datacdr, tmpdataq->rpt,
                tmpdataq->sort, tmpdataq->len, tmpdataq->val);
        }
        cc_msg("head = %lx tail = %lx size = %ld\n\n",
            (int32)datap->head, (int32)datap->tail, datap->size);
    }
}

#endif
/*
 * The following routines are the ones that do the messing around with
 * vargen pointers in order to throw away old (zero) tentative initialisers
 * and replace them with new initialisers.
 */
static bool is_tentative(Symstr *sv)
{
    TentativeDefn *td = tentative_defs, *prev;

    for (prev = NULL;  td != NULL;  prev = td, td = td->next)
    {   if (td->sv == sv)
        {   /* we are going to return TRUE at the end of this iteration.  */
#ifndef TARGET_IS_INTERPRETER
            int32 count=0;
            DataInit *tmpdataq;
            /*
             * Found a tentative definition so let's reset vargen's state
             * ready for the initialiser... but only if (old) size != 0.
             * @@@ the 'size' test next is becoming subsumed by the bss test.
             */
            if (td->size != 0 && !td->maybebss)
            {   save_vargen_state(&saved_vg_state);
#ifdef DEBUG_TENTATIVE_DEFS
if (debugging(DEBUG_BIND)) show_vargen_state("before");
#endif
                /* Restore vg's state to that before reading the initialiser */
                restore_vargen_state(td);
                datahead = td->data.head;
#ifdef DEBUG_TENTATIVE_DEFS
if (debugging(DEBUG_BIND)) show_vargen_state("after");
#endif
                /*
                 *  Throw away old tentative (zero) initialiser ready for
                 *  replacement by genstaticparts().
                 */
                if (datap->head == 0)
                    tmpdataq = saved_vg_state.data.head;
                else
                {
                    tmpdataq = data.tail->datacdr;
                    datap->tail->datacdr = 0;
                }
                while (count < td->size)
                {
                    if (tmpdataq == NULL)
                        syserr(syserr_tentative);
                    /* skip labels in static init chain */
                    if (tmpdataq->sort != LIT_LABEL)
                    {
/* AM: insert check for ->len field being valid (i.e. not LABEL/ADCON   */
/* maybe this cannot happen, but AM gets very worried by this sort      */
/* of 'if not LABEL then nice' reasoning.                               */
                        if (tmpdataq->sort == LIT_ADCON)
                            syserr(syserr_tentative1);
                        count += tmpdataq->rpt * tmpdataq->len;
                        datasplice = tmpdataq;
                    }
                    tmpdataq = tmpdataq->datacdr;
                }
                if (count != padsize(td->size,alignof_toplevel_static))
                    syserr(syserr_tentative2);
                datatail = tmpdataq;
                /* set flag for reset_vg_after_init_of_tentative_defn() */
                saved_vg_state.size = td->size;
            }
            else
            {
if (debugging(DEBUG_BIND)) cc_msg("maybebss found\n");
            }
#endif
            /* remove entry from tentative list */
            if (prev == NULL)
                tentative_defs = td->next;
            else
                prev->next = td->next;
            return YES;
        }
    }
    return NO;
}

#ifndef TARGET_IS_INTERPRETER
void reset_vg_after_init_of_tentative_defn(void)
{
    TentativeDefn *td;
    if (saved_vg_state.size == 0) return;
    /*
     * Vargen has now inserted the new initialiser just where we want it.
     * Hum, however I might have removed an item from the list which I
     * have in my own tables.  Check for this and update any items found.
     */
    for (td = tentative_defs;  td != 0;  td = td->next)
        if (td->data.tail == datasplice) td->data.tail = datap->tail;

    /* link new initialiser to the rest of the chain */
    datap->tail->datacdr = datatail;

    /*
     * Reset all the pointers so that vargen does not realise that its
     * internal lists have been modified.
     */
    if (datahead != 0) datap->head = saved_vg_state.data.head;
    if (datatail != 0) datap->tail = saved_vg_state.data.tail;
    datap->size = saved_vg_state.data.size;

    saved_vg_state.size = 0; /* unset flag to prevent recall */
}
#endif

static void check_for_incomplete_tentative_defs(TentativeDefn *td)
{
    for (; td != NULL;  td = td->next) {
        if (td->size == 0) {
            if (td->statik) {
                cc_err(bind_err_incomplete_tentative, td->sv);
                continue;
            }
            td->maybebss = YES; /* Bodge: need to sort out allocation into
                                   data area (for fwd-refd struct too) */
            td->size = td->elt_size;
        }
#ifdef TARGET_HAS_BSS
        if (td->maybebss)
            addbsssym(td->sv, td->size, td->align, td->statik, 0);
#endif
    }
}

static void check_for_fwd_static_decl(Binder *b, Symstr *sv)
{
    /* The following feature optionally enables spurious forward */
    /* static declarations to be weeded out.                     */
    if (feature & FEATURE_PREDECLARE &&
        !(binduses_(b) & u_referenced) &&
        bindstg_(b) & bitofstg_(s_static))
        cc_warn(bind_warn_unused_static_decl, sv);
}

static void check_ansi_linkage(Binder *b, DeclRhsList *d)
{
    if ((bindstg_(b) ^ d->declstg) & PRINCSTGBITS)
    {   /* Linkage clash, but do not moan about stray 'extern type name's */
        if ((d->declstg & b_globalregvar) ||
            (d->declstg & bitofstg_(s_static)) &&
                 !(d->declstg & b_implicitstg) ||
            (d->declstg & bitofstg_(s_extern)) &&
                   (d->declstg & b_implicitstg))
        {   /* Oldest linkage wins... */
/* ECN - convert errors about different linkage types to warnings */
            if (suppress & D_LINKAGE)
              cc_warn(bind_rerr_linkage_disagreement,
                      d->declname, bindstg_(b) & PRINCSTGBITS);
            else
              cc_rerr(bind_rerr_linkage_disagreement,
                      d->declname, bindstg_(b) & PRINCSTGBITS);
            /* patch d->declstg to a compatible tentative type... */
            d->declstg = (d->declstg &~ b_implicitstg) ^
                         (bitofstg_(s_static) | bitofstg_(s_extern));

        } else if (bindstg_(b) & bitofstg_(s_static))
            /* test is needed because of global register variables */
            /* /* Explicit extern here, previous was static.
                  Change d to say static and no longer extern, implicit nor C linkage
                  Check OK for C++ */
            d->declstg = (d->declstg & ~(b_implicitstg|b_clinkage)) ^
                         (bitofstg_(s_static) | bitofstg_(s_extern));
        else if (bindstg_(b) & bitofstg_(s_extern))
            /* test is needed because of global register variables */
            /* /* Implicit static here, previous was extern.
                  Change d to say extern and no longer static.
                  Check OK for C++ */
            d->declstg ^= (bitofstg_(s_static) | bitofstg_(s_extern));
    }
    else check_for_fwd_static_decl(b, d->declname);
}

static TypeExpr *is_openarray(TypeExpr *t)
{
    t = princtype(t);                          /* skip leading typedefs */
    if (h0_(t) == t_subscript && typesubsize_(t) == 0) return typearg_(t);
    return NULL;
}

void instate_alias(Symstr *a, Binder *b)
/*
 * Make the symbol a an alias for b. This curious facility is used to
 * provide local pseudonyms ___first_arg and ___last_arg which share
 * binders with the first and last args in a function's definition, and
 * which are sometimes useful when implementing va_args on awkward machines.
 * Note that pop_varenv (qv) will not moan if the symbols I use as aliases
 * are unused.
 */
{   /* should take care over storage lifetimes here... */
    Binder *pseudonym = mk_binder(a, bindstg_(b)|b_pseudonym, bindtype_(b));
    realbinder_(pseudonym) =    /* cautious: shouldn't ever happen now. */
        (bindstg_(b) & b_pseudonym) ? realbinder_(b) : b;
    add_local_binder(pseudonym);
}

#ifdef FOR_ACORN
#ifndef PASCAL
#ifndef FORTRAN
static int cfront_special_name(Binder *b)
{   char *s;
    s = symname_(bindsym_(b));
    if (s[0] == '_' && s[1] == '_')
    {   s += 2;
        if (isdigit(*s))
        {   while (isdigit(*++s));
            if (s[0] == '_' && s[1] == '_') return 1;
        }
        else if (strcmp(s, "link") == 0) return 1;
    }
    return 0;
}
#endif
#endif
#endif

static Binder *instate_declaration_1(DeclRhsList *d, int declflag)
/* only the TOPLEVEL and DUPL_OK bits of declflag are examined */
{
/* I have just parsed a declarator, and in that identifiers are held    */
/* as Symstr *'s which contain an h0 of s_identifier.  Instate the      */
/* declaration, returning the Binder record hung off the symbind_ entry.*/
    Symstr *sv = d->declname;
    Binder *b;
    int32 olduses = 0;
    bool maybebss = 0;
#ifdef PASCAL /*ECN*/
    int level = declflag >> 2;
    declflag &= 3;
#endif
    if (sv == 0 || h0_(sv) != s_identifier)     /* check/remove*/
    {   syserr(syserr_instate_decl, (long)(sv==0 ? 0 : h0_(sv)));
        return 0;
    }
    if (attribofstgacc_(d->declstg)) syserr("instate_decl(access!=0)");
    if (debugging(DEBUG_BIND))
    {   cc_msg("instate_declaration(%x, %lx): $r\n", declflag, d->declstg, sv);
        pr_typeexpr(d->decltype, sv); cc_msg("\n");
    }
/* No need to prunetype as inner typedef would have already been used:  */
    if (LanguageIsCPlusPlus &&
        (d->declstg & bitofstg_(s_typedef)) &&
            isprimtypein_(d->decltype, ENUMORCLASSBITS))
    {   TagBinder *tb = typespectagbind_(d->decltype);
        if (isgensym(tagbindsym_(tb)))
        {   tagbindsym_(tb) = sv;
            if (declflag & TOPLEVEL) tag_global_(sv) = tb;
        }
    }
    if (declflag & TOPLEVEL)
/* Top level declarations may only surplant previous top level extern decls */
/* Really we should also unify 'local' extern decls.  @@@ not done yet.     */
    {   TypeExpr *glotype = 0;
        b = bind_global_(sv);
#ifdef PASCAL /*ECN*/
        if (b && (bindstg_(b) & b_synbit1)) b = 0;
#endif
        if (b != 0 && (bindstg_(b) & bitofstg_(s_typedef)) &&
            (bindstg_(b) & u_implicitdef))
        {   if (LanguageIsCPlusPlus && d->declstg & bitofstg_(s_typedef) &&
                !(d->declstg & u_implicitdef) &&
                equivtype(bindtype_(b), d->decltype) != 2)
                cc_err(bind_err_type_disagreement, sv);
            b = 0;
        }

        if (b != 0)
        {   bool discardb = NO;
            olduses = binduses_(b);
            /* check the types match */
            if (h0_(bindtype_(b)) == t_fnap && h0_(d->decltype) == t_fnap)
            {   /* propagate #pragma -v and -y info from decl to defn */
                if (typefnaux_(d->decltype).variad == 0)
                    typefnaux_(d->decltype).variad =
                        typefnaux_(bindtype_(b)).variad;
                typefnaux_(d->decltype).flags |=
                    typefnaux_(bindtype_(b)).flags &
                    ~typefnaux_(d->decltype).flags;
                if ((d->declstg & bitofstg_(s_inline)) &&
                    (bindstg_(b) & b_maybeinline))
                        bindstg_(b) = (bindstg_(b) & ~bitofstg_(s_extern)) |
                            bitofstg_(s_static);
                bindstg_(b) &= ~b_maybeinline;
/* /* @@@ old-style bit here. */
            }
            switch (equivtype(bindtype_(b), d->decltype))
            {   default:
                         if ((suppress & D_MPWCOMPATIBLE) &&
                             widened_equivtype(b->bindtype, d->decltype))
                             /* f(char) incompatibility with f(int)     */
                             /* (or f(c) char c; {}) gets just a warning*/
                             cc_warn(bind_err_type_disagreement, sv);
                         else
                         {   TypeExpr *bt = princtype(bindtype_(b));
                             TypeExpr *dt = princtype(d->decltype);
/* The next line helps the 'SPECMARK' suite...                          */
                             if (h0_(bt) == t_fnap && h0_(dt) == t_fnap)
                                 cc_rerr(bind_err_type_disagreement, sv);
                             else
                                 cc_err(bind_err_type_disagreement, sv);
                             discardb = YES;
                             /* new one wins (or trouble if old was a fn
                                declaration and this is a var definition
                              */
                         }
                         break;
                case 2:  glotype = bindtype_(b);  /* IDENTICAL */
                case 1:
                         if (LanguageIsCPlusPlus &&
                             h0_(bindtype_(b)) == t_fnap &&
                             merge_default_arguments(b->bindtype,
                                                     d->decltype) == 2)
                             glotype = bindtype_(b);  /* IDENTICAL */
                         break;
            }
/* It would be nice to merge the type/stgclass errors so we don't get   */
/* 2 errors for 'typedef int a; extern double a;'                       */
            /* Check for duplicate and conflicting definitions */
            if ((bindstg_(b) | d->declstg) & bitofstg_(s_typedef))
            {   if ((bindstg_(b) & d->declstg) & bitofstg_(s_typedef))
                {   /* can duplicate a typedef in C++, not in C */
                    if (!LanguageIsCPlusPlus)
                        cc_rerr(bind_rerr_duplicate_typedef, sv);
                }
                else
                {   if ((d->declstg & bitofstg_(s_typedef)) &&
                        (d->declstg & u_implicitdef))
                        return NULL;
                    else if ((bindstg_(b) & bitofstg_(s_typedef)) &&
                             (bindstg_(b) & u_implicitdef))
                        discardb = 1;
                    else
                        cc_err(bind_err_duplicate_definition, sv);
                }
            }
/* The next two tests are perhaps more natural the other way round --   */
/* if we have two *definitions* then fault (unless one is tentative),   */
/* otherwise essentially ignore one of the *declarations*.              */
/* It is arguable the PCC case should make a more careful test on u_bss */
/* but this is compatible with previous version.                        */
            else if (bindstg_(b) & b_undef  &&
                     !(bindstg_(b) & u_bss) &&
                      (feature & FEATURE_PCC ||
                        !(bindstg_(b) & bitofstg_(s_extern) &&
                          bindstg_(b) & b_implicitstg &&
                          is_openarray(bindtype_(b)))
                      ) || d->declstg & b_undef)
            {   /* At least one of the declarations has no initialiser. */
                /* N.B. an existing static declaration will appear to   */
                /*    have an initialiser, as do ansi tentatives.       */
                /*    (Provisional) BSS tentatives have b_undef but     */
                /*    they also have u_bss, which we avoid.  Messy!     */
                /*    The delicate case: plain int [] has b_undef.      */
                if (feature & FEATURE_PCC)
                {   /* pcc/as faults b is static or initialised plain,  */
                    /* d is static or plain (whether or not init'd).    */
                    if (!(bindstg_(b) & b_undef) &&
                         (d->declstg & bitofstg_(s_static) ||
                          d->declstg & bitofstg_(s_extern) &&
                          d->declstg & b_implicitstg))
                        cc_err(bind_err_duplicate_definition, sv);
                    else
                        check_for_fwd_static_decl(b, sv);
                }
                else if (LanguageIsCPlusPlus &&
                         !(h0_(bindtype_(b)) == t_fnap) &&
                         (bindstg_(b) & b_implicitstg) &&
                         (d->declstg & b_implicitstg))
                    cc_err(bind_err_duplicate_definition, sv);
                else
                    check_ansi_linkage(b, d);
            }
            else if (!(bindstg_(b) & b_fnconst) &&
                     !(d->declstg  & b_fnconst) &&
                     !(feature & FEATURE_PCC)   &&
                     is_tentative(sv))
                check_ansi_linkage(b, d);
            else
                cc_err(bind_err_duplicate_definition, sv);

            if (!(bindstg_(b) & b_clinkage) && (d->declstg & b_clinkage))
                cc_rerr(bind_rerr_linkage_previously_c,b);

            if (discardb) {
                binduses_(b) |= u_superceded;
                b = 0;
            }
        }
        else
        { if (feature & FEATURE_PREDECLARE)
          { /* The following is a feature to enable policing of a software */
            /* quality policy which says "only objects previously DECLARED */
            /* as extern (presumably in a header) may be DEFINED extern".  */
            if ((d->declstg & bitofstg_(s_extern)) &&
                (!(d->declstg & b_undef) || (d->declstg & b_implicitstg)) &&
                sv != mainsym)
                cc_warn(bind_warn_not_in_hdr, sv);
          }
          if (LanguageIsCPlusPlus && h0_(d->decltype) == t_fnap)
              (void) merge_default_arguments(d->decltype, d->decltype);
        }
        /* Maybe we wish to turn off the following for non-hosted system.  */
        if (sv == mainsym && (d->declstg & bitofstg_(s_extern)))
        {   TypeExpr *t = princtype(d->decltype);
            /* check args here too one day? */
            if (h0_(t) != t_fnap || !equivtype(typearg_(t), te_int))
                if (!(feature & FEATURE_PCC) || (feature & FEATURE_FUSSY))
                    cc_warn(bind_warn_main_not_int);
        }

        if (feature & FEATURE_PCC)
        {    if ((d->declstg &
                  (bitofstg_(s_static)|b_fnconst|b_globalregvar|bitofstg_(s_typedef))
                 ) == bitofstg_(s_static) && (d->declstg & b_undef))
            {   maybebss = addTentativeDefn(sv, sizeoftype(d->decltype), 0,
                                   alignoftype(d->decltype),
                                   (d->declstg&bitofstg_(s_static)) != 0)
#ifdef CONST_DATA_IN_CODE
                        && !(d->declstg & u_constdata)
#endif
                           ;
                if (!maybebss) d->declstg &= ~b_undef;
            }
        }
        else
        {   if ((b == 0 || bindstg_(b) & b_undef)                  &&
                 !(d->declstg & (b_fnconst|b_globalregvar|bitofstg_(s_typedef))) &&
                 (d->declstg & b_undef)                            &&
                 (d->declstg & bitofstg_(s_static) ||
                  d->declstg & bitofstg_(s_extern) &&
                  d->declstg & b_implicitstg))
            {
                /* no pre-exisiting defn and not a function defn and    */
                /* no initializer and (static blah... or plain blah...) */
                TypeExpr *elt_t = is_openarray(d->decltype);
                int32 size, elt_size;
                if (elt_t == NULL)
                    size = sizeoftype(d->decltype), elt_size = 0;
                else
                    size = 0, elt_size = sizeoftype(elt_t);
                maybebss = addTentativeDefn(sv, size, elt_size,
                                   alignoftype(d->decltype),
                                   (d->declstg&bitofstg_(s_static)) != 0)
#ifdef CONST_DATA_IN_CODE
                        && !(d->declstg & u_constdata)
#endif
                           ;
                if (!maybebss) d->declstg &= ~b_undef;
            }
        }

        /*
         * Decls such as 'extern int a;' may be superceded by decls such
         * as 'extern int a=1;'.  However, decls such as 'extern int a=1;'
         * may not be superceded (but useless decls such as 'extern int a;'
         * will still be accepted). BUT BEWARE: 'extern int foo[]' MUST be
         * superceded by [extern] int foo[FOOSIZE] or chaos will ensue in
         * -pcc mode (only the size distinguishes Common Def from Ext Ref).
         */
        if ((b != 0) && (glotype == 0))
        {   /*
             * Assert: glotype == 0 iff equivtype(b, d->...) == 1
             *         equivtype(b,d...) == 1 iff one of b, d is blah[].
             * No: could be (int (*b)()) and (int (*d)(int)).
             * Now check for b (the original decl) being an open array.
             */
            if (is_openarray(bindtype_(b)))
            {   binduses_(b) |= u_superceded;
                b = 0;    /* force treatment of d, below, as if new */
            }
        }
        if (b == 0 || !(d->declstg & b_undef))
        {
#ifdef PASCAL /*ECN*/
            TypeExpr *gt = glotype ? glotype : d->decltype;
#else
            TypeExpr *gt = glotype ? glotype : globalize_typeexpr(d->decltype);
#endif
#ifndef OLD_VSN
/* This code updates the old binder when a DEFINITION supercedes a      */
/* DECLARATION.  Thus bind_global_(sv) is set (init. 0) at most once.   */
/* (except for the is_openarray() supercession above.).                 */
            if (b == 0)
                b = topbind2(sv, d->declstg, gt);
            else if (!(bindstg_(b) & b_globalregvar))
            {   /* Assert: !(d->declstg & b_undef) */
                bindstg_(b) = d->declstg |
                  (bindstg_(b) &
                   (bitofstg_(s_virtual)|bitofstg_(s_inline)|b_impl|b_purevirtual));
                bindtype_(b) = gt;      /* @@@ bindstg/bindconst too?   */
            }
#else /* !OLD_VSN */
            /* suppress new DECLARATION if already DEFINED.             */
            if (b != 0) binduses_(b) |= u_superceded;
            topbind2(sv, d->declstg, gt);      /* sets bind_global_(sv) */
            b = bind_global_(sv);
#endif
        }
#ifdef TARGET_HAS_BSS
        if (maybebss)
        {   /* addbsssym() now called when we know for certain. */
            bindaddr_(b) = BINDADDR_UNSET;
        }
#endif
    }
    else
    {   /* NOT a top-level declaration */
        Binder *bnew;
        Scope *saved_local_scope = set_local_block_scope();
        if (local_scope == NULL)
            syserr("instate_declaration_1 - no local scope/local block scope");
        if ((b = find_scopemember(sv, local_scope->scopemems)) != NULL)
        {   if ((bindstg_(b) | d->declstg) & bitofstg_(s_typedef))
            {   if ((d->declstg & bitofstg_(s_typedef)) &&
                    (d->declstg & u_implicitdef))
                {   if (!(d->declstg & b_undef))
                    {   bindstg_(b) = d->declstg | (bindstg_(b) &
                                (bitofstg_(s_virtual)|bitofstg_(s_inline)));
                        bindtype_(b) = globalize_typeexpr(d->decltype);
                    }
                    return NULL;
                }
                else if ((bindstg_(b) & bitofstg_(s_typedef)) &&
                         (bindstg_(b) & u_implicitdef))
                    declflag |= DUPL_OK;
            }
            if (!(declflag & DUPL_OK))
            {   if (!(bindstg_(b) & d->declstg & bitofstg_(s_extern) ||
                      (LanguageIsCPlusPlus &&
                       bindstg_(b) & d->declstg & bitofstg_(s_typedef))))
                    cc_err(bind_err_duplicate_definition, sv);
                else if (equivtype(d->decltype, bindtype_(b)) == 0)
                    cc_err(bind_err_duplicate_definition, sv);
                else if (LanguageIsCPlusPlus && h0_(bindtype_(b)) == t_fnap)
                    merge_default_arguments(bindtype_(b), d->decltype);
            }
            /* flag old one as referenced to avoid spurious warns:  */
            binduses_(b) |= u_referenced;
        }
        else
          if (LanguageIsCPlusPlus && h0_(d->decltype) == t_fnap)
              (void) merge_default_arguments(d->decltype, d->decltype);
/* AM: at some time we may wish to check or export C 'local' extern     */
/* decls for checking purposes.  At that point we must ensure           */
/* that d->decltype is globalize()d.                                    */
        if (b != NULL && !(declflag & DUPL_OK))
            bnew = b;
        else
        {   if (declflag & GLOBALSTG)     /* TOPLEVEL doesn't come here.  */
                bnew = global_mk_binder(NULL, sv, d->declstg,
                           d->decltype = globalize_typeexpr(d->decltype));
            else
                bnew = mk_binder(sv, d->declstg, d->decltype);
            add_local_binder(bnew);
/* stop regalloc moan about typefnaux */
            if (d->declstg & bitofstg_(s_extern) && isfntype(bindtype_(bnew)))
                bindtype_(bnew) = globalize_typeexpr(bindtype_(bnew));
        }
/* If a local extern is already bound, try to find on topbindingchain:  */
        if ((d->declstg & b_undef) &&
            ((d->declstg & (bitofstg_(s_extern))) ||
             ((d->declstg & (bitofstg_(s_static))) &&
               (h0_(d->decltype) == t_fnap))))
        {
/* The following lines are written to cope with curios like:            */
/*    extern int i=0; void f() { auto i; { extern int i; ...            */
/* However, note ANSI ambiguities in:                                   */
/*    typedef int i; void f() { auto int i; { extern int i; ...         */
/* and                                                                  */
/*    void g(double); void f() { extern void g(); g(1); }               */
            Binder *btop = 0, *t;
            for (t = topbindingchain; t != NULL; t = bindcdr_(t))
                if (bindsym_(t) == sv && !(binduses_(t) & u_superceded))
                    btop = t;
            if (btop)
            {   if (!equivtype(bindtype_(btop), d->decltype) ||
                        bindstg_(btop) & bitofstg_(s_typedef))
                    /* warn about above ambiguities?                    */
                    cc_rerr_cppwarn(bind_rerr_local_extern, sv);
/* The following lines specifically please INMOS, but are otherwise OK. */
/* Update the storage class/bindaddr field, but NOT type (ambiguity):   */
/* @@@ what about local externs to register globals (extension)?        */
                else
                {   /* Inherit old storage class:                       */
                    bindstg_(bnew) = bindstg_(bnew) & ~(STGBITS | u_loctype) |
                                     bindstg_(btop) & (STGBITS | u_loctype);
                    if (!(bindstg_(btop) & b_undef))
                    {   /* change undef extern to this-module-defd.     */
                        bindaddr_(bnew) = bindaddr_(btop);
                        bindstg_(bnew) &= ~b_undef;
                    }
                }
            }
        }
        if (h0_(d->decltype) != t_fnap &&
            (d->declstg & bitofstg_(s_static))) {
#ifdef TARGET_HAS_BSS
/* Note that that this BSS_THRESHOLD applies in PCC mode too.           */
            if ( (d->declstg & b_undef) &&
                 sizeoftype(d->decltype) > BSS_THRESHOLD &&
                 !(d->declstg & u_constdata) )
                maybebss = YES;
            else
#endif
                d->declstg &= ~b_undef, bindstg_(bnew) &= ~b_undef;
        }
        b = bnew;
#ifdef TARGET_HAS_BSS
        if (maybebss)
        {   TypeExpr *t = bindtype_(b);
            bindaddr_(b) = addbsssym(sv, sizeoftype(t), alignoftype(t),
                                (bindstg_(b) & bitofstg_(s_static)) != 0, YES);
        }
#endif
        local_scope = saved_local_scope;
    }
    /*
     * Make sure information about old definitions and previous references
     * gets carried over from the old binder to the new binder.
     * (ie. '{f();}; f(){}').
     */
#ifdef PASCAL /*ECN*/
    b->bindlevel = level;
    b->synflags = d->synflags;
#endif
    binduses_(b) |= olduses & u_referenced | (d->declstg & u_constdata)
#ifdef TARGET_HAS_BSS
/* @@@ Dec 90: when does the u_bss get removed if later init'ed?        */
                                          | (maybebss ? u_bss : 0)
#endif
                                          ;
#ifdef  FOR_ACORN
#ifndef PASCAL
#ifndef FORTRAN
    if (cplusplus_flag && cfront_special_name(b)) binduses_(b) |= u_referenced;
#endif
#endif
#endif

    return b;
}

ClassMember *instate_member(DeclRhsList *d, int bindflg)
{   return LanguageIsCPlusPlus ? instate_member_cpp(d, bindflg) :
                                 instate_member_1(d, bindflg);
}

Binder *instate_declaration(DeclRhsList *d, int declflag)
{   return LanguageIsCPlusPlus ? instate_declaration_cpp(d, declflag) :
                                 instate_declaration_1(d, declflag);
}

/* label bindings... */

static LabBind *label_create(Symstr *id)
/* Called when a label is referenced - arranges for a check to be made   */
/* at the end of the block to ensure that the label is properly defined. */
{   LabBind *x = symlab_(id);
    if (x == 0) labelchain = symlab_(id) = x = mk_labbind(labelchain, id);
    return x;
}

LabBind *label_define(Symstr *id)
/* Called when a label is defined.  NULL return iff duplicate */
{   LabBind *x = label_create(id);
    if (x->labuses & l_defined)
    {   cc_err(bind_err_duplicate_label, id);
        return 0;
    }
    x->labuses |= l_defined;
    return x;
}

LabBind *label_reference(Symstr *id)
/* Called when a label is referenced - arranges for a check to be made   */
/* at the end of the block to ensure that the label is properly defined. */
{   LabBind *x = label_create(id);
    x->labuses |= l_referenced;
    return x;
}

void label_resolve(void)
{
    LabBind *lc;
    for (lc = labelchain; lc!=NULL; lc = lc->labcdr)
    {   Symstr *id = lc->labsym;
        symlab_(id) = NULL;
        if (!(lc->labuses & l_defined))
            cc_err(bind_err_unset_label, id);
/* NB the CG or SEM should ignore goto's to label 0 (undef'd). */
        if (!(lc->labuses & l_referenced))
            cc_warn(bind_warn_label_not_used, id);
    }
    labelchain = NULL;
}

void bind_cleanup(void)
/* see comment on unbindlocals */
{   TagBinder *p;
    Binder *b;

    for (p = toptagbindchain; p != 0; p = tagbindcdr_(p))
    {   Symstr *sv = bindsym_(p);
        if (debugging(DEBUG_BIND))
            cc_msg("top struct unbind $r of %p\n",
                sv, (VoidStar)tag_global_(sv));
        tag_global_(sv) = 0;             /* restore previous binding */
        /* warning on undefined struct/union now removed */
        /*if (!(tagbindbits_(b) & TB_DEFD) && !(suppress & D_STRUCTWARN))
            { cc_warn(bind_rerr_undefined_tag, tagbindsort(p), p); }
         */
    }
    toptagbindchain = 0;             /* just for tidyness */

    check_for_incomplete_tentative_defs(
        (TentativeDefn *) dreverse((List *)tentative_defs));

    for (b = topbindingchain; b != 0; b = bindcdr_(b))
    {   Symstr *sv = bindsym_(b);
        bind_global_(sv) = 0;            /* restore previous binding */
        if (binduses_(b) & u_superceded);
        else if (bindstg_(b) & bitofstg_(s_static))
        {   if (!(bindstg_(b) & b_generated))
            {   if (binduses_(b) & u_referenced)
                {   if (bindstg_(b) & b_undef)
                    {   /* surely b_undef static MUST be fnconst after  */
                        /* tentative resolution?  u_bss?                */
                        /* @@@ check_for_imcomplete_tentative_defs()    */
                        /* does not unset b_undef for [] nor bss!       */
                        if (bindstg_(b) & b_fnconst)
                        {   if (suppress & D_LINKAGE) /* @@@ this should probably be removed now */
                                cc_warn(bind_err_undefined_static, b);
                            else
                                cc_pccwarn(bind_err_undefined_static, b);
                        }
                    }
                }
                else if (!(bindstg_(b) & bitofstg_(s_inline)) &&
/* @@@ of course we shouldn't generate unreferenced inline fns!         */
                         !(issimpletype_(princtype(bindtype_(b))) &&
                           (qualifiersoftype(bindtype_(b)) &
                            bitoftype_(s_const))))
/* @@@ of course we shouldn't generate unreferenced static consts!      */
                    cc_warn(bind_warn_static_not_used, b);
            }
        }
        else if (feature & FEATURE_NOUSE)
        {   if (!(binduses_(b) & u_referenced) &&
                !(bindstg_(b) & bitofstg_(s_typedef))) {
              if (bindstg_(b) & b_fnconst) {
                cc_warn(bind_warn_function_not_used, b);
              } else {
                cc_warn(bind_warn_variable_not_used, b);
              }
            }
        }
    }
    topbindingchain = 0;             /* just for tidyness */
}

#ifdef __powerc
typedef Symstr *((*PPCC_CHOKES_ON_CAST)[BIND_HASHSIZE]);
#endif

void bind_init(void)
{   int i;
    topbindingchain = 0, toptagbindchain = 0, labelchain = 0;
    freeScopes = local_scope = NULL;
    tag_found_in_local_scope = NO;
    scope_level = 0;
    tentative_defs = 0;
#ifndef TARGET_IS_INTERPRETER
    saved_vg_state.size = 0;
#endif
    gensymline = gensymgen = 0;
#ifdef __powerc
    hashvec = (PPCC_CHOKES_ON_CAST)
#else
    hashvec = (Symstr *((*)[BIND_HASHSIZE]))
#endif
        GlobAlloc(SU_Other, sizeof(*hashvec));
    for (i = 0; i < BIND_HASHSIZE; i++) (*hashvec)[i] = NULL;
}

/* end of bind.c */
