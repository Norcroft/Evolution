        /*
 * C compiler file mip/dwarf.c
 * Copyright:   (C) 1995, Advanced RISC Machines Limited. All rights reserved.
 */

/*
 * RCS $Revision: 1.13 $
 * Checkin $Date: 1995/11/07 17:40:55 $
 * Revising $Author: fwai $
 */

/* The debug table formatter for DWARF debug tables, for embedding in ELF
   or other object files.
 */

#ifdef __STDC__
#  include <string.h>
#else
#  include <strings.h>
#endif
#include <stddef.h>

#include "globals.h"

#ifdef TARGET_HAS_MULTIPLE_DEBUG_FORMATS

#define dbg_tableindex dwarf_tableindex
#define dbg_notefileline dwarf_notefileline
#define dbg_addcodep dwarf_addcodep
#define dbg_scope dwarf_scope
#define dbg_topvar dwarf_topvar
#define dbg_type dwarf_type
#define dbg_proc dwarf_proc
#define dbg_locvar dwarf_locvar
#define dbg_locvar1 dwarf_locvar1
#define dbg_commblock dwarf_commblock
#define dbg_enterproc dwarf_enterproc
#define dbg_bodyproc dwarf_bodyproc
#define dbg_return dwarf_return
#define dbg_xendproc dwarf_xendproc
#define dbg_define dwarf_define
#define dbg_undef dwarf_undef
#define dbg_init dwarf_init
#define dbg_finalise dwarf_finalise
#define dbg_setformat dwarf_setformat
#define dbg_final_src_codeaddr dwarf_final_src_codeaddr

#define obj_notefpdesc dwarf_notefpdesc
#define dbg_debugareaexists dwarf_debugareaexists
#define dbg_writedebug dwarf_writedebug

#endif

#include "mcdep.h"
#include "mcdpriv.h"
#include "aeops.h"
#include "aetree.h"      /* evaluate */
#include "errors.h"
#include "xrefs.h"
#include "store.h"
#include "codebuf.h"
#include "regalloc.h"
#include "util.h"
#include "sem.h"       /* alignoftype, sizeoftype, structfield */
#include "builtin.h"   /* te_xxx, xxxsegment */
#include "bind.h"
#include "simplify.h"  /* mcrep */
#include "version.h"
#include "dwarf.h"
#include "unmangle.h"  /* unmangle() */

/* Private tag values used to mark DbgList items. They don't make it out */
/* into the debug tables.                                                */

#define DW_TAG_endproc (DW_TAG_lo_user)
#define DW_TAG_end_lexical_block (DW_TAG_lo_user+1)
#define DW_TAG_proctype_formal (DW_TAG_lo_user+2)
#define DW_TAG_fref (DW_TAG_lo_user+3)
#define DW_TAG_ignore (DW_TAG_lo_user+4)

#ifdef TARGET_HAS_DEBUGGER

#ifdef TARGET_HAS_DWARF

static Symstr *debug_sym, *lineinfo_sym;

static int dwarf_version;

#ifndef TARGET_HAS_MULTIPLE_DEBUG_FORMATS

char dbg_name[] = "DWARF";
int usrdbgmask;

#endif

#define DEBUGAREANAME (dwarf_version == 2 ? ".debug_info" : ".debug")
#define LINEINFOAREANAME (dwarf_version == 2 ? ".debug_line" : ".line")
#define NAMEINDEXAREANAME ".debug_pubnames"
#define RANGEINDEXAREANAME ".debug_arange"

static void dbg_sub_init(void);
static bool dbg_init_done, dbg_sub_init_done;

#define DbgAlloc(n) GlobAlloc(SU_Dbg, n)
#define DbgNew(type) ((type *)DbgAlloc(sizeof(type)))

typedef struct DbgList Dw_TypeRep;

typedef struct Dbg_Structelt Dbg_Structelt;
typedef struct Dbg_Enumelt Dbg_Enumelt;
typedef struct Dbg_Return Dbg_Return;

/* The next macro seems to indicate a lack to do this portably in ANSI-C */
/* Are discriminated unions second class objects?                        */
#define DbgListAlloc(variant) \
    ((DbgList *)DbgAlloc((size_t)(sizeof(dbglist->car.variant)+offsetof(DbgList,car))))

/* The following is the *internal* data structure in which debug info. */
/* is buffered.                                                        */
typedef struct DbgList DbgList;
typedef enum {
    Stg_Reg,
    Stg_Auto,
    Stg_Static,
    Stg_Extern,
    Stg_ArgReg,
    Stg_ArgAuto
} StgClass;

typedef struct {
    Symstr *sym;
    RealRegister r;
} SymOrReg;

struct DbgList {
    DbgList *cdr;
    int debsort;
    DbgList *sibling;
    unsigned32 dbgloc;
    union
    {  struct { DbgList *children;
                char *name;
                Symstr *codeseg;
                int32 codesize;
              } DEB_SECTION;
       struct { DbgList *children;
                Dw_TypeRep *type;
                int32 sourcepos;
                int32 entryaddr, bodyaddr;   /* see dbg_bodyproc */
                DbgList *endproc;
                int global;
                char *fileentry;
                char *name;
                Symstr *codeseg;
                bool variadic;
                int32 virtuality;
                int32 vtable_offset;
                bool inlined;
                Dw_TypeRep *mem;
              } DEB_PROC;
       struct { DbgList *qualtypes;
                DbgList *children;
                Dw_TypeRep *type;
              } DEB_PROCTYPE;
       struct { DbgList *qualtypes;
                Dw_TypeRep *container;
                Dw_TypeRep *type;
              } DEB_PTRTOMEMTYPE;
       struct { Symstr *name;
                Dw_TypeRep *type;
              } DEB_FORMAL;
       struct { int32 dummy;
              } DEB_REST;
       struct { int32 endaddr;
/* a (Deb_filecoord *) next line may subsume sourcepos too */
              } DEB_ENDPROC;
       struct { Dw_TypeRep *type;
                int32 sourcepos;
                StgClass stgclass;
                int32 location;
                Symstr *sym;
                SymOrReg base;
                Dw_TypeRep *mem;
              } DEB_VAR;
       struct { DbgList *qualtypes;
                char *name;
                Dw_TypeRep *type;
                TypeExpr *typex;
              } DEB_TYPE;
       struct { DbgList *qualtypes;
                int typecode;
              } DEB_BASETYPE;
       struct { Dw_TypeRep *type;
                Dw_TypeRep *next;  /* qualified type of the same type */
                Dw_TypeRep *basetype;
                int n;
                char *qualmap;
              } DEB_QUALTYPE;
       struct { DbgList *qualtypes;
                char *name;
                DbgList *children;
                Dw_TypeRep *container;
                char size;
              } DEB_ENUM;
       struct { char *name;
                int32 val;
              } DEB_ENUMERATOR;
       struct { DbgList *qualtypes;
                int32 size;
                int32 open;
                Dw_TypeRep *basetype;
                int32 lowerbound;
                int32 upperbound;
              } DEB_ARRAY;
       struct { DbgList *qualtypes;
                char *name;
                DbgList *children;
                int undefsort;
                  /* A not-yet-defined (but referenced) struct, class or   */
                  /* union is given sort TAG_fref, and its real sort is    */
                  /* stored in undefsort.                                  */
                int32 size;
                Friend *friends;
              } DEB_STRUCT;
       struct { char *name;                     /* source form */
                int32 offset;
                Dw_TypeRep *type;
                unsigned8 bsize, boffset;
                bool decl;
              } DEB_MEMBER;
       struct { Dw_TypeRep *type;
                int32 offset;
                int32 virtuality;
              } DEB_INHERIT;
       struct { union { DbgList *next;
                        int32 codeaddr;
                      } s;
                DbgList *children;
                DbgList *end;
                Symstr *codeseg;
              } DEB_STARTSCOPE;
       struct { union { DbgList *next;
                        int32 codeaddr;
                      } s;
              } DEB_ENDSCOPE;
       struct { int32 space;
                char const *name;
                int32 argcount;
                char const *body;
                dbg_ArgList const *arglist;
                FileLine fl;
              } DEB_DEFINE;
       struct { char const *name;
                FileLine fl;
              } DEB_UNDEF;
       struct { DbgList **parent;
              } DEB_NULL;
    } car;
};

static DbgList *dbglist, *dbglistproc;
static DbgList *basetypes;

typedef struct Dbg_LocList Dbg_LocList;
struct Dbg_LocList
{   Dbg_LocList *cdr;
    Binder *name;
    int32 pos;
    Dw_TypeRep *typeref;
};

static Dbg_LocList *dbg_loclist;

static struct {
    Symstr *sym;
    int32 len;
} baseseg;

typedef struct Dw_Scope Dw_Scope;
struct Dw_Scope {
    Dw_Scope *cdr;
    DbgList *item;
    DbgList **childlist;
    DbgList **childp;
};

static Dw_Scope *freescopes,
                *scopestack;

static void PushScope(DbgList *item, DbgList **childp) {
  Dw_Scope *p = freescopes;
  if (p != NULL)
    freescopes = cdr_(freescopes);
  else
    p = DbgNew(Dw_Scope);
  cdr_(p) = scopestack; p->item = item; p->childp = childp; p->childlist = childp;
  *childp = NULL;
  scopestack = p;
}

static void PopScope(void) {
  Dw_Scope *p = scopestack;
  scopestack = cdr_(p);
  cdr_(p) = freescopes;
  freescopes = p;
  if (*p->childlist != NULL) {
    DbgList *terminator = DbgListAlloc(DEB_NULL);
    cdr_(terminator) = dbglist; dbglist = terminator;
    terminator->debsort = TAG_padding;
    terminator->sibling = NULL;
    terminator->car.DEB_NULL.parent = p->childlist;
    *p->childp = terminator;
  }
}

static DataXref *dw_relocate(DataXref *xrefs, int32 where, Symstr *symbol) {
  return (DataXref*)global_list3(SU_Xref, xrefs, where, symbol);
}

/* First structs and code for buffering file/line co-ordinates.         */
/* We have one of these for every file we see.  BUT because a file may  */
/* validly be included more than once in a C program, we had better do  */
/* pointer, not string, equality on names, and rely on pp.c behaviour.  */

typedef struct Deb_filecoord Deb_filecoord;
typedef struct Deb_filelist Deb_filelist;

struct Deb_filelist
{   Deb_filelist *nextfile;
    char *filename;
    unsigned lastline;
    Deb_filecoord *linelist;
};

struct Deb_filecoord
{   Deb_filecoord *cdr;
    Deb_filelist *file; Deb_filecoord *nextinfile;
    unsigned16 line, col;
    int32 codeaddr;
    Symstr *codeseg;
};

static Deb_filelist *dbg_filelist;
/* The next two vars are (code order) list & tail pointer */
static Deb_filecoord *dbg_coord_p, **dbg_coord_q;
static Deb_filecoord dbg_coord_sentinel =
  {   0,                   /* cdr                                      */
      0, 0,                /* file, nextinfile                         */
      0, 0xffff, 0         /* line, col, codeaddr(set by dbg_fileinfo).*/
  };

int32 dbg_tableindex(int32 dt_number)
{
  IGNORE(dt_number);
  return 0;
}

void *dbg_notefileline(FileLine fl) {
  Deb_filelist *x;
  if (!dbg_sub_init_done) dbg_sub_init();
  x = dbg_filelist;
  while (x != NULL && x->filename != fl.f) x = x->nextfile;
  if (x == NULL) {
    x = DbgNew(Deb_filelist);
    x->nextfile = dbg_filelist, x->filename = fl.f,
    x->linelist = 0;
    dbg_filelist = x;
  }
  if (usrdbg(DBG_LINE)) {
    Deb_filecoord *l = x->linelist;
    /* There used to be a syserr here if (l != NULL && l->line > fl.l),
       but it can be triggered by #line (though not #line n file, which
       won't give equality of filename): the CVS has an example. Also, it
       fails if functions are taken out of file order, as in the
       out-of-line expansion of inline functions.
     */
    l = DbgNew(Deb_filecoord);
      cdr_(l) = NULL,
      l->nextinfile = x->linelist, x->linelist = l,
      l->file = x, l->line = fl.l, l->col = fl.column,
      l->codeaddr = -1;
      l->codeseg = dbg_init_done ? bindsym_(codesegment) : 0;
    x->lastline = fl.l;
    return (void *)l;
  }
  return DUFF_ADDR;
}

static DbgList *dbglistscope;

/* The 'dbgaddr' arg has type 'void *' to keep the debugger types local to */
/* this file.  This does not make it any less of a (ANSI approved) hack.   */
void dbg_addcodep(void *dbgaddr, int32 codeaddr) {
  if (dbgaddr == NULL) { /* J_INFOSCOPE */
    /* c.flowgraf outputs a J_INFOSCOPE immediately after calling
     * dbg_scope, to mark the relevant code address.
     */
    if (debugging(DEBUG_Q)) cc_msg("-- scope at 0x%lx\n", codeaddr);
    { DbgList *p = dbglistscope, *next;
      for (; p != NULL; p = next) {
        next = p->car.DEB_STARTSCOPE.s.next;
        p->car.DEB_STARTSCOPE.s.codeaddr = codeaddr;
      }
      dbglistscope = NULL;
    }
  } else if (usrdbg(DBG_LINE)) {
    Deb_filecoord *p = (Deb_filecoord *)dbgaddr;
    if (debugging(DEBUG_Q))
      cc_msg("%p ('%s' line %u/%u) @ %.6lx\n", (VoidStar )p,
             p->file->filename, p->line, p->col, (long)codeaddr);
    /* The following test avoids setting nextincode/codeaddr twice */
    /* This is currently needed in case FileLine's are duplicated. */
    if (p->codeaddr == -1) {
      p->codeaddr = codeaddr;
      *dbg_coord_q = p;
      dbg_coord_q = &cdr_(p);
    }
  }
}

static DataXref *xrefs;

typedef union {
  unsigned32 u;
  char const *s;
  struct {
    unsigned32 n;
    Symstr *sym;
  } ref;
  unsigned32 d[2];
} AttribArg;

static unsigned32 dw_inlinestring(char const *s, unsigned32 offset) {
  size_t n = strlen(s);
  obj_writedebug(s, n+1);
  return offset + n + 1;
}

static unsigned32 dw_write_b(unsigned u, unsigned32 offset) {
  char b[1];
  b[0] = u; obj_writedebug(b, 1);
  return offset + 1;
}

static unsigned32 dw_write_h(unsigned32 u, unsigned32 offset) {
  unsigned16 h[1];
  h[0] = (unsigned16)u; obj_writedebug(h, 1+DBG_SHORTFLAG);
  return offset + 2;
}

static unsigned32 dw_write_w(unsigned32 u, unsigned32 offset) {
  obj_writedebug(&u, 1+DBG_INTFLAG);
  return offset + 4;
}

/* End of file/line co-ordinate code */

static Dw_TypeRep *Dw_PrimType(int typecode) {
  Dw_TypeRep *p = basetypes;
  for (; p != NULL; p = cdr_(p))
    if (p->car.DEB_BASETYPE.typecode == typecode)
      return p;
  p = DbgListAlloc(DEB_BASETYPE);
  cdr_(p) = basetypes; basetypes = p;
  p->debsort = DW_TAG_base_type;
  p->car.DEB_BASETYPE.typecode = typecode;
  p->car.DEB_BASETYPE.qualtypes = NULL;
  return p;
}

#define dw_additem(sort, tag) \
  dw_additem_s((size_t)(sizeof(dbglist->car.sort)+offsetof(DbgList,car)), tag)

static DbgList *dw_additem_s(size_t size, unsigned tag) {
  DbgList *p = (DbgList *)DbgAlloc(size);
  p->debsort = tag;
  p->sibling = NULL; *scopestack->childp = p; scopestack->childp = &p->sibling;
  cdr_(p) = dbglist; dbglist = p;
  return p;
}

static void dw_typerep(TypeExpr *, Dw_TypeRep **typep);

static Dw_TypeRep *dw_arrayrep(TypeExpr *t, Expr *e)
{ /* e is the array size. Since C arrays start at 0, the upper bound is */
  /* one less                                                           */
  DbgList *p = DbgListAlloc(DEB_ARRAY);
  p->debsort = DW_TAG_array_type;
  p->car.DEB_ARRAY.open = e == NULL;
  dw_typerep(t, &p->car.DEB_ARRAY.basetype);
  p->car.DEB_ARRAY.lowerbound = 0;
  p->car.DEB_ARRAY.upperbound = e ? evaluate(e)-1:0;
  p->car.DEB_ARRAY.size = sizeoftype(t);
  p->car.DEB_ARRAY.qualtypes = NULL;
  p->sibling = NULL; *scopestack->childp = p; scopestack->childp = &p->sibling;
  p->cdr = dbglist;              /* do this last (typerep above) */
  dbglist = p;
  return p;
}

static void move_children(DbgList *p, DbgList *prevlist) {
  /* Move the child entries of p, currently at the head of dbglist (up to   */
  /* but not including prevlist) to immediately before p (after it when the */
  /* list is reversed)                                                      */
  DbgList *end = dbglist,
          *before;
  for (; cdr_(end) != prevlist; end = cdr_(end)) continue;
  for (before = prevlist; cdr_(before) != p; before = cdr_(before)) continue;
  cdr_(before) = dbglist; cdr_(end) = p;
  dbglist = prevlist;
}

static void dbg_proc_i(Symstr *name, TypeExpr *t, bool ext, FileLine fl, bool virt, int32 voffset, bool inlined);
void dbg_xendproc(FileLine fl);
FileLine dbg_invented_fl = {0, 0, 0};

static Dw_TypeRep *dw_structentry(Dw_TypeRep *p, TagBinder *b, TypeExpr *x) {
  SET_BITMAP sort = tagbindbits_(b) & CLASSBITS;
  int itemsort = sort == bitoftype_(s_struct) ? DW_TAG_structure_type :
                 sort == bitoftype_(s_union)  ? DW_TAG_union_type :
                                                DW_TAG_class_type;
  DbgList *prev_dbglist = NULL;
  if (p == NULL) {
    p = dw_additem(DEB_STRUCT, itemsort);
    p->car.DEB_STRUCT.size = 0;    /* filled in later */
    p->car.DEB_STRUCT.name = isgensym(tagbindsym_(b)) ? NULL : symname_(tagbindsym_(b));
    p->car.DEB_STRUCT.children = NULL;
    p->car.DEB_STRUCT.qualtypes = NULL;
    p->car.DEB_STRUCT.friends = NULL;
    if (b != NULL) b->tagbinddbg = (IPtr)p;
    if (!(tagbindbits_(b) & TB_DEFD)) {
      p->debsort = DW_TAG_fref;
      p->car.DEB_STRUCT.undefsort = itemsort;
      p->car.DEB_STRUCT.size = 0;
      return p;
    }
  } else {
    p->debsort = itemsort;
    if (LanguageIsCPlusPlus) p->car.DEB_STRUCT.friends = b->friends;
    if (p != dbglist) prev_dbglist = dbglist;
  }
  p->car.DEB_STRUCT.size = sizeoftype(x);

  { StructPos sp;
    DbgList **pp = &p->car.DEB_STRUCT.children;
    ClassMember *l;
    PushScope(p, pp);
    structpos_init(&sp, b);
    for (l = tagbindmems_(b); l != 0; l = memcdr_(l))
      if (memsv_(l) != NULL && 
            (structfield(l, sort, &sp) || LanguageIsCPlusPlus)) {
        if (LanguageIsCPlusPlus && attributes_(l) & (CB_BASE|CB_VBASE)) {
            DbgList *base = dw_additem(DEB_INHERIT, DW_TAG_inheritance);
            TagBinder *tb_base = typespectagbind_(princtype(memtype_(l)));
            base->car.DEB_INHERIT.type = (Dw_TypeRep *)tb_base->tagbinddbg;
            base->car.DEB_INHERIT.offset = sp.woffset;
            base->car.DEB_INHERIT.virtuality = attributes_(l) & CB_VBASE;
        } else if (h0_(memtype_(l)) != t_ovld) 
            if (isfntype(memtype_(l)))
            {   dbg_proc_i(memsv_(l), princtype(memtype_(l)), NO, dbg_invented_fl,
                        bindstg_(l) & bitofstg_(s_virtual), bindxx_(l),
                        bindstg_(l) & bitofstg_(s_inline));
                dbg_xendproc(dbg_invented_fl);
                PushScope(cdr_(dbglist), &(cdr_(dbglist))->car.DEB_PROC.children);
                dw_typerep(memtype_(l), &(cdr_(dbglist))->car.DEB_PROC.type);
                PopScope();
            } else {
              /* note that memsv is 0 for padding bit fields */
              DbgList *mem = dw_additem(DEB_MEMBER, DW_TAG_member);
              mem->car.DEB_MEMBER.offset = sp.woffset;
              mem->car.DEB_MEMBER.name = symname_(memsv_(l));
              mem->car.DEB_MEMBER.boffset = (unsigned8)sp.boffset;
              mem->car.DEB_MEMBER.bsize = (unsigned8)sp.bsize;
              mem->car.DEB_MEMBER.decl = bindstg_(l) & bitofstg_(s_extern); 
              if (sp.bsize == 0) {
                dw_typerep(memtype_(l), &mem->car.DEB_MEMBER.type);
              } else {
                TypeExpr te; te = *memtype_(l);
                typespecmap_(&te) &= ~BITFIELD;
                dw_typerep(&te, &mem->car.DEB_MEMBER.type);
              }
            }
      }
    PopScope();
  }
  if (prev_dbglist != NULL) move_children(p, prev_dbglist);
  return p;
}

static Dw_TypeRep *dw_enumentry(Dw_TypeRep *p, TagBinder *b) {
  static unsigned const c[] =
  {   FT_signed_char,   FT_signed_short,   FT_signed_integer,   FT_signed_integer,
      FT_unsigned_char, FT_unsigned_short, FT_unsigned_integer, FT_unsigned_integer
  };
  static char const s[] = { 1, 2, 4, 4, 1, 2, 4, 4};
  DbgList *prev_dbglist = NULL;
  if (p == NULL) {
    p = dw_additem(DEB_ENUM, DW_TAG_enumeration_type);
    p->car.DEB_ENUM.children = NULL;
    p->car.DEB_ENUM.name = isgensym(tagbindsym_(b)) ? NULL : symname_(tagbindsym_(b));
    p->car.DEB_ENUM.qualtypes = NULL;
    if (b != NULL) b->tagbinddbg = (IPtr)p;
    if (!(tagbindbits_(b) & TB_DEFD)) {
      p->debsort = DW_TAG_fref;
      return p;
    }
  } else {
    p->debsort = DW_TAG_enumeration_type;
    prev_dbglist = dbglist;
  }

  { int32 container = (tagbindbits_(b) & TB_CONTAINER) >> TB_CONTAINER_SHIFT;
    p->car.DEB_ENUM.container = Dw_PrimType(c[container]);
    p->car.DEB_ENUM.size = s[container];
  }
  PushScope(p, &p->car.DEB_ENUM.children);
  { BindList *members = tagbindenums_(b);
    for (; members != 0; members = members->bindlistcdr) {
      DbgList *mem = dw_additem(DEB_ENUMERATOR, DW_TAG_enumerator);
      Binder *elt = members->bindlistcar;
      mem->car.DEB_ENUMERATOR.name = symname_(bindsym_(elt));
      mem->car.DEB_ENUMERATOR.val = bindenumval_(elt);
    }
  }
  PopScope();
  if (prev_dbglist != NULL) move_children(p, prev_dbglist);
  return p;
}

static Dw_TypeRep *struct_typerep(TypeExpr *x) {
  TagBinder *b = typespectagbind_(x);
  Dw_TypeRep *t = (Dw_TypeRep *)b->tagbinddbg;
  if (t == NULL ||                 /* not yet seen */
      (t->debsort == DW_TAG_fref && (tagbindbits_(b) & TB_DEFD))
                           /* previously seen, (undefined), now defined */
      ) {
    if (tagbindbits_(b) & bitoftype_(s_enum))
      t = dw_enumentry(t, b);
    else
      t = dw_structentry(t, b, x);
  }
  return t;
}

static int typename_match(char *mangled, char *generic)
{ int l = strlen(generic);
  if (mangled == generic) return 1;
  if (strncmp(mangled, generic, l) == 0 &&
      strncmp(mangled+l, "__", 2) == 0) return 1;
  return 0;
}

typedef struct Dw_QualType Dw_QualType;
struct Dw_QualType {
  Dw_QualType *cdr;
  int qual;
};
#define dw_mk_qualtype(t, p) ((Dw_QualType *)syn_list2(p, t))

static Dw_QualType *CVQualType(SET_BITMAP m, Dw_QualType *p) {
  if (m & bitoftype_(s_const)) p = dw_mk_qualtype(DW_TAG_const_type, p);
  if (m & bitoftype_(s_volatile)) p = dw_mk_qualtype(DW_TAG_volatile_type, p);
  return p;
}

static Dw_TypeRep *dw_fnrep(TypeExpr *x) {
  DbgList *t = dw_additem(DEB_PROCTYPE, DW_TAG_subroutine_type);
  dw_typerep(typearg_(x), &t->car.DEB_PROCTYPE.type);
  t->car.DEB_PROCTYPE.qualtypes = NULL;
  PushScope(t, &t->car.DEB_PROCTYPE.children);
  { FormTypeList *ft = typefnargs_(x);
    for (; ft != NULL; ft = ft->ftcdr) {
      DbgList *p = dw_additem(DEB_FORMAL, DW_TAG_proctype_formal);
      p->car.DEB_FORMAL.name = ft->ftname == NULL ? NULL : ft->ftname;
      dw_typerep(ft->fttype, &p->car.DEB_FORMAL.type);
    }
  }
  if (fntypeisvariadic(x)) dw_additem(DEB_REST, DW_TAG_unspecified_parameters);
  PopScope();
  return t;
}

static Dw_TypeRep *dw_ptrtomemrep(TypeExpr *x)
{   DbgList *t = dw_additem(DEB_PTRTOMEMTYPE, DW_TAG_ptr_to_member_type);
    t->car.DEB_PTRTOMEMTYPE.container = (Dw_TypeRep *)typespectagbind_(x)->tagbinddbg;
    dw_typerep(typearg_(x), &t->car.DEB_PTRTOMEMTYPE.type);
    t->car.DEB_PTRTOMEMTYPE.qualtypes = NULL;
    return t;
}

static void dw_typerep(TypeExpr *x, Dw_TypeRep **typep)
{   /* note that we do NOT call prunetype() here so we still see typedefs */
  Dw_QualType *quals = NULL;
  Dw_TypeRep *restype = NULL;
  for (;;) {
    switch (h0_(x)) {
    case t_content:
      quals = CVQualType(typeptrmap_(x), quals);
      quals = dw_mk_qualtype(DW_TAG_pointer_type, quals);
      x = typearg_(x);
      continue;
    case t_ref:
      quals = CVQualType(typeptrmap_(x), quals);
      quals = dw_mk_qualtype(DW_TAG_reference_type, quals);
      x = typearg_(x);
      continue;
    case t_coloncolon:
      restype = dw_ptrtomemrep(x);
      break;
    case t_subscript:
      restype = dw_arrayrep(typearg_(x), typesubsize_(x));
      break;
    case t_fnap:
      restype = dw_fnrep(x);
      break;
    case s_typespec:
      { SET_BITMAP m = typespecmap_(x);
        quals = CVQualType(m, quals);
        switch (m & -m) {   /* LSB - unsigned32/long etc. are higher */
        case bitoftype_(s_enum):
          restype = struct_typerep(x);
          break;
        case bitoftype_(s_struct):
        case bitoftype_(s_class):
        case bitoftype_(s_union):
          restype = struct_typerep(x);
          break;
        case bitoftype_(s_typedefname):
          { Binder *b = typespecbind_(x);
            /* is there already a table entry for it ? */
            { DbgList *l;
              for ( l = dbglist ; l != NULL ; l = cdr_(l))
                if ( l->debsort==DW_TAG_typedef &&
                     l->car.DEB_TYPE.typex==bindtype_(b) &&
                     typename_match(l->car.DEB_TYPE.name, symname_(bindsym_(b)))) {
                  restype = l;
                  break;
                }
              if (l == NULL) syserr("typerep $b", b);
            }
            break;
          }
        case bitoftype_(s_char):
          { int32 mcr = mcrepoftype(x);
            restype = Dw_PrimType((mcr & MCR_SORT_MASK) == MCR_SORT_SIGNED ?
                                                    FT_signed_char : FT_unsigned_char);
            break;
          }
        case bitoftype_(s_int):
          if (m & BITFIELD) syserr(syserr_dbg_bitfield);
          { int32 mcr = mcrepoftype(x);
            int tc = (mcr & MCR_SORT_MASK) == MCR_SORT_SIGNED ?
                       (((mcr & MCR_SIZE_MASK) == 2) ? FT_signed_short : FT_signed_integer) :
                       (((mcr & MCR_SIZE_MASK) == 2) ? FT_unsigned_short : FT_unsigned_integer);
            restype = Dw_PrimType(tc);
            break;
          }
        case bitoftype_(s_double):
          restype = Dw_PrimType((m & bitoftype_(s_short)) ? FT_float : FT_dbl_prec_float);
          break;
        case bitoftype_(s_bool):
          restype = Dw_PrimType(FT_boolean);
          break;
        case bitoftype_(s_void):
          restype = Dw_PrimType(FT_void);
          break;
        default:
          syserr(syserr_dbg_typerep, x, (long)m);
          restype = NULL;
          return;
        }
        break;
      }
        /* drop through for now */
    default:
      syserr(syserr_dbg_typerep, x, (long)typespecmap_(x));
      restype = NULL;
      return;
    }
    break;
  }
  if (quals != NULL) {
    Dw_TypeRep *basetype = restype;
    Dw_TypeRep *q, **qp;
    int n = 1;
    int maxn = length((List *)quals);
    char *qualmap = (char *)SynAlloc(maxn);
    char *qualp = &qualmap[maxn];

    switch (restype->debsort) {
    case DW_TAG_ptr_to_member_type:
        qp = &restype->car.DEB_PTRTOMEMTYPE.qualtypes;
        break;
    case DW_TAG_array_type:
        qp = &restype->car.DEB_ARRAY.qualtypes;
        break;
    case DW_TAG_subroutine_type:
        qp = &restype->car.DEB_PROCTYPE.qualtypes;
        break;
    case DW_TAG_enumeration_type:
        qp = &restype->car.DEB_ENUM.qualtypes;
        break;
    case DW_TAG_class_type:
    case DW_TAG_union_type:
    case DW_TAG_structure_type:
        qp = &restype->car.DEB_STRUCT.qualtypes;
        break;
    case DW_TAG_typedef:
    case DW_TAG_base_type:
        qp = &restype->car.DEB_TYPE.qualtypes;
        break;
    default:
        syserr("dw_typerep: %d", restype->debsort);
        qp = NULL;
    }

    for (; quals != NULL; quals = cdr_(quals), n++) {
      *--qualp = quals->qual == DW_TAG_pointer_type ? MOD_pointer_to :
               quals->qual == DW_TAG_reference_type ? MOD_reference_to :
                   quals->qual == DW_TAG_const_type ? MOD_const :
                                                      MOD_volatile;
      for (; (q = *qp) != NULL; qp = &q->car.DEB_QUALTYPE.next)
        if (q->car.DEB_QUALTYPE.n > n) {
          q = NULL;
          break;
        } else if (q->car.DEB_QUALTYPE.n == n &&
                   memcmp(q->car.DEB_QUALTYPE.qualmap, qualp, n) == 0) {
          restype = q;
          break;
        }
      if (q == NULL) {
        DbgList *newt = dw_additem(DEB_QUALTYPE, quals->qual);
        newt->car.DEB_QUALTYPE.type = restype;
        newt->car.DEB_QUALTYPE.n = n;
        newt->car.DEB_QUALTYPE.qualmap = (char *)DbgAlloc(n);
        memcpy(newt->car.DEB_QUALTYPE.qualmap, qualp, n);
        newt->car.DEB_QUALTYPE.next = *qp; *qp = newt;
        newt->car.DEB_QUALTYPE.basetype = basetype;
        restype = newt;
      }
    }
  }
  if (typep != NULL) *typep = restype;
}

#if 0

static bool typename_fuzzy_match(char *mangled, char *generic)
{   int l = strlen(mangled);
    while ((*mangled == *generic) && l) mangled++, generic++, l--;
    if (l && isdigit(*mangled)) {
        int len = atoi(mangled);
        while (isdigit(*mangled)) mangled++;
        mangled += len;
    }
    while ((*mangled == *generic) && l) mangled++, generic++, l--;
    return (l == 0);
}

static void dw_spec(Binder *b, Dw_TypeRep **spec)
{   TagBinder *parent = bindparent_(b);
    DbgList *p = ((DbgList *)parent->tagbinddbg)->car.DEB_STRUCT.children;
    for (; p->debsort != TAG_padding; p = p->sibling)
    {   if (p->debsort == DW_TAG_member && 
                typename_match(symname_(bindsym_(b)), p->car.DEB_MEMBER.name) ||
            p->debsort == DW_TAG_subprogram &&
                typename_fuzzy_match(symname_(bindsym_(b)), p->car.DEB_PROC.name))
        {   *spec = p;
            return;
        }
    }
    syserr("dw_spec: member not found");
}
#endif

static void dw_addvar(Symstr *name, Dw_TypeRep *t, int32 sourcepos,
                      StgClass stgclass, SymOrReg base, int32 addr, TypeExpr *type)
{ unsigned tag = stgclass >= Stg_ArgReg ? DW_TAG_formal_parameter: DW_TAG_variable;
  DbgList *p = dw_additem(DEB_VAR, tag);
  Binder *b = bind_global_(name);
  p->car.DEB_VAR.type = t;
  p->car.DEB_VAR.sourcepos = sourcepos;
  p->car.DEB_VAR.stgclass = stgclass;
  p->car.DEB_VAR.location = addr;
  p->car.DEB_VAR.sym = name;
  p->car.DEB_VAR.base = base;
  if (type != NULL) dw_typerep(type, &p->car.DEB_VAR.type);
  p->car.DEB_VAR.mem = (b && bindparent_(b)) ? (Dw_TypeRep *)bindparent_(b)->tagbinddbg : NULL;
}

void dbg_topvar(Symstr *name, int32 addr, TypeExpr *t, int stgclass,
                FileLine fl)
/* For scoping reasons this only gets called on top-level variables (which */
/* are known to be held in global store).  (Does this matter?)             */
{ if (usrdbg(DBG_PROC))
  { /* nb bss => external here.  The effect is only to cause the table item
       to be 0+symbol, rather than addr+data seg
     */
    DbgList *p;
    SymOrReg base;
    StgClass stg = (stgclass & DS_REG) ? Stg_Reg :
                   (stgclass & DS_EXT) ? Stg_Extern :
                                         Stg_Static;
    base.sym = NULL;
    if (stgclass & (DS_EXT|DS_BSS))
      base.sym = name, addr = 0;
#ifdef CONST_DATA_IN_CODE
    else if (stgclass & DS_CODE)
      base.sym = bindsym_(constdatasegment);
#endif
    else if (!(stgclass & DS_REG))
      base.sym = bindsym_(datasegment);

    if (debugging(DEBUG_Q))
      cc_msg("top var $r @ %.6lx\n", name, (long)addr);
    if (stgclass != 0 && stg != Stg_Reg)
      for ( p = dbglist ; p!=NULL ; p = p->cdr )
        if ( p->debsort == DW_TAG_variable &&
             (p->car.DEB_VAR.stgclass == Stg_Extern ||
              p->car.DEB_VAR.stgclass == Stg_Static) &&
             p->car.DEB_VAR.location==0 &&
             p->car.DEB_VAR.sym == name) {
          p->car.DEB_VAR.sourcepos = fl.l;
          p->car.DEB_VAR.location = addr;
          p->car.DEB_VAR.base = base;
          return;
        }
    dw_addvar(name, 0, fl.l, stg, base, addr, t);
  }
}

static void dw_typeinternal(Symstr *name, Dw_TypeRep *t, TypeExpr *type)
/* This procedure is called on a type-declaration internal to a procedure
 * (from dbg_scope, after the syntax tree has evaporated), and on a global
 * one, with the syntax tree in place.  The latter therefore goes through
 * dbg_type, which internalises the type.
 */
{ if (isgensym(name))
    dw_typerep(type, NULL);
  else {
    DbgList *p = dw_additem(DEB_TYPE, DW_TAG_typedef);
    if (debugging(DEBUG_Q))
        cc_msg("type $r\n", name);
    p->car.DEB_TYPE.type = t;
    p->car.DEB_TYPE.typex = type;
    p->car.DEB_TYPE.name = symname_(name);
    p->car.DEB_TYPE.qualtypes = NULL;
    if (type != NULL) dw_typerep(type, &p->car.DEB_TYPE.type);
  }
}

void dbg_type(Symstr *name, TypeExpr *t, FileLine fl)
/* This only gets called on top-level types (which are known to be held in
 * global store).
 */
{ IGNORE(fl);
  dw_typeinternal(name, 0, t);
}

static Deb_filecoord *cur_proc_coord;

static void dbg_proc_i(Symstr *name, TypeExpr *t, bool ext, FileLine fl, bool virt, int32 voffset, bool inlined)
{ if (usrdbg(DBG_PROC))
  { DbgList *p = DbgListAlloc(DEB_PROC);
    char *s = symname_(name);
    Binder *b = bind_global_(name);
    if (debugging(DEBUG_Q)) cc_msg("startproc $r\n", name);
    t = princtype(t);
    p->debsort = DW_TAG_subprogram;
    if (h0_(t) != t_fnap) syserr(syserr_dbg_proc);
    dw_typerep(typearg_(t), &p->car.DEB_PROC.type);
    p->car.DEB_PROC.sourcepos = fl.l;
    p->car.DEB_PROC.entryaddr = 0;       /* fill in at dbg_enterproc */
    p->car.DEB_PROC.bodyaddr = 0;        /* fill in at dbg_bodyproc */
    p->car.DEB_PROC.endproc = 0;         /* fill in at dbg_xendproc   */
    p->car.DEB_PROC.fileentry = fl.f;
    p->car.DEB_PROC.name = s;
    p->car.DEB_PROC.codeseg = bindsym_(codesegment);
    p->car.DEB_PROC.global = ext;
    p->car.DEB_PROC.variadic = fntypeisvariadic(t);
    p->car.DEB_PROC.virtuality = virt;
    p->car.DEB_PROC.vtable_offset = voffset;
    p->car.DEB_PROC.inlined = inlined;
    p->car.DEB_PROC.mem = (b && bindparent_(b)) ? 
                (Dw_TypeRep *)bindparent_(b)->tagbinddbg : NULL;
    p->sibling = NULL; *scopestack->childp = p; scopestack->childp = &p->sibling;
    p->cdr = dbglist;              /* do this last (typerep above) */
    dbglistproc = dbglist = p;       /* so can be filled in */
    PushScope(p, &p->car.DEB_PROC.children);
  }
  if (usrdbg(DBG_LINE))
    cur_proc_coord = (Deb_filecoord *)fl.p;
  dbg_loclist = 0;
}

void dbg_proc(Symstr *name, TypeExpr *t, bool ext, FileLine fl)
{   dbg_proc_i(name, t, ext, fl, NO, 0, 
        bind_global_(name) ? bindstg_(bind_global_(name)) & bitofstg_(s_inline) : NO);
}

void dbg_enterproc(void)
{ if (usrdbg(DBG_PROC))
  { DbgList *p = dbglistproc;

    if (p == 0 || p->debsort != DW_TAG_subprogram || p->car.DEB_PROC.entryaddr != 0)
      syserr(syserr_dbg_proc1);
    if (debugging(DEBUG_Q))
      cc_msg("enter '%s' @ %.6lx\n",
             p->car.DEB_PROC.name, (long)codebase);
    p->car.DEB_PROC.entryaddr = codebase;
  }
  if (usrdbg(DBG_LINE))
    dbg_addcodep(cur_proc_coord, codebase);
}

/* The following routine records the post-entry codeaddr of a proc */
void dbg_bodyproc(void)
{ if (usrdbg(DBG_PROC))
  { DbgList *p = dbglistproc;
    if (p == 0 || p->debsort != DW_TAG_subprogram || p->car.DEB_PROC.bodyaddr != 0)
      syserr(syserr_dbg_proc1);
    if (debugging(DEBUG_Q))
      cc_msg("body '%s' @ %.6lx\n",
             p->car.DEB_PROC.name, (long)(codebase+codep));
    p->car.DEB_PROC.bodyaddr = codebase+codep;
  }
}

void dbg_return(int32 addr)
{ if (usrdbg(DBG_PROC))
  { if (debugging(DEBUG_Q))
        cc_msg("return @ %.6lx\n", addr);
    /* No way to represent this in DWARF */
  }
}

void dbg_xendproc(FileLine fl)
{ IGNORE(fl);
  if (bindsym_(codesegment) == baseseg.sym) baseseg.len = codebase+codep;
  if (usrdbg(DBG_PROC))
  { DbgList *q = dbglistproc;
    DbgList *p = DbgListAlloc(DEB_ENDPROC);
    if (q == 0 || q->debsort != DW_TAG_subprogram || q->car.DEB_PROC.endproc != 0)
      syserr(syserr_dbg_proc1);
    if (debugging(DEBUG_Q))
      cc_msg("endproc '%s' @ %.6lx\n",
             q->car.DEB_PROC.name, (long)(codebase+codep));
    q->car.DEB_PROC.endproc = p;
    p->debsort = DW_TAG_endproc;
    p->car.DEB_ENDPROC.endaddr = codebase+codep;
    cdr_(p) = dbglist; dbglist = p;
    dbg_loclist = 0;
    PopScope();
  }
}

/* dbg_locvar() registers the name and line of a declaration, and internalises
 * the type.  Location info cannot be added until after register allocation.
 * See also dbg_scope which completes.
 * (Type internalisation cannot be done then, because by that time the tree
 * has evaporated).
 * Also remember that dead code elimination may remove some decls.
 */
void dbg_locvar(Binder *name, FileLine fl)
{ if (usrdbg(DBG_VAR) && !isgensym(bindsym_(name))) {
    /* local to a proc */
    Dbg_LocList *p = (Dbg_LocList*) BindAlloc(sizeof(Dbg_LocList));
    if (debugging(DEBUG_Q))
      cc_msg("note loc var $b\n", name);
    cdr_(p) = dbg_loclist;
    p->name = name;
    p->pos = fl.l;
    dw_typerep(bindtype_(name), &p->typeref);
    dbg_loclist = p;
    if (bindstg_(name) & bitofstg_(s_typedef))
      dw_typeinternal(bindsym_(name), p->typeref, NULL);
  }
}

static Dbg_LocList *dbg_findloclist(Binder *b)
{ Dbg_LocList *p;
  for (p = dbg_loclist; p != NULL; p = p->cdr)
    if (p->name == b) return p;
  return NULL;
}

void dbg_locvar1(Binder *b) {
  Symstr *name = bindsym_(b);
  SymOrReg base;
  Dbg_LocList *p = dbg_findloclist(b);
  StgClass stgclass;
  int stgclassname;
  int32 addr = bindaddr_(b);
  base.sym = NULL;
  if (p == NULL || p->pos == -1) {
    if (debugging(DEBUG_Q)) cc_msg(" omitted");
    return;   /* invented variable name (e.g. s_let) */
  }
  switch (bindstg_(b) & PRINCSTGBITS) {
  default:
  defolt:
    syserr(syserr_dbg_table, name, (long)bindstg_(b), (long)addr);
    return;
  case bitofstg_(s_typedef):
    if (debugging(DEBUG_Q)) cc_msg(" <typedef>");
    return;                   /* dbg_type deals with s_typedef vars  */
  case bitofstg_(s_extern):
    if (debugging(DEBUG_Q)) cc_msg(" <extern>");
    return;                   /* local externs do not allocate store */
  case bitofstg_(s_static):
    stgclass = Stg_Static, stgclassname = 'S';
    base.sym =
#ifdef TARGET_HAS_BSS
           (bindstg_(b) & u_bss) ? bindsym_(bsssegment) :
#endif
#ifdef CONST_DATA_IN_CODE
     (bindstg_(b) & u_constdata) ? bindsym_(constdatasegment) :
#endif
                                   bindsym_(datasegment);
    break;
  case bitofstg_(s_auto):
    if (bindxx_(b) != GAP) {
      stgclass = (addr & BINDADDR_MASK) == BINDADDR_ARG ? Stg_ArgReg : Stg_Reg;
      stgclassname = 'R', addr = register_number(bindxx_(b));
    } else switch (addr & BINDADDR_MASK) {
    case BINDADDR_ARG:
      stgclass = Stg_ArgAuto, stgclassname = 'A', addr = local_fpaddress(b);
      base.r = local_fpbase(b);
      break;
    case BINDADDR_LOC:
      stgclass = Stg_Auto, stgclassname = 'P', addr = local_fpaddress(b);
      base.r = local_fpbase(b);
      break;
    case 0:
      /* probably declared but not used case (where addr is still a bindlist) */
      if ((bindstg_(b) & b_bindaddrlist) != 0) {
        if (debugging(DEBUG_Q)) cc_msg(" unused - omitted");
        return;
      }
      /* otherwise, fall into internal error case */
    default:
      goto defolt;
    }
    break;
  }
  if (debugging(DEBUG_Q)) cc_msg(" %c %lx", stgclassname, (long)addr);
  dw_addvar(name, p->typeref, p->pos, stgclass, base, addr, NULL);
}

bool dbg_scope(BindListList *newbll, BindListList *oldbll)
{ int32 entering = length((List *)newbll) - length((List *)oldbll);
  if (entering == 0) return NO;
  if (entering < 0)
  { BindListList *t = newbll;
    newbll = oldbll, oldbll = t;
  }
  if (length((List *)oldbll) > 0) {
    BindListList *bll = newbll;
    DbgList *last = NULL;
    for (bll = newbll; bll != oldbll; bll = bll->bllcdr) {
      if (bll == 0) syserr(syserr_dbg_scope);
      if (bll->bllcar != 0) {
        DbgList *p;
        if (entering > 0) {
          p = dw_additem(DEB_STARTSCOPE, DW_TAG_lexical_block);
          p->car.DEB_STARTSCOPE.s.next = last; /* filled in soon by INFOSCOPE */
          p->car.DEB_STARTSCOPE.codeseg = bindsym_(codesegment);
          PushScope(p, &p->car.DEB_STARTSCOPE.children);
        } else {
          p = DbgListAlloc(DEB_ENDSCOPE);
          p->debsort = DW_TAG_end_lexical_block;
          p->car.DEB_STARTSCOPE.s.next = last; /* filled in soon by INFOSCOPE */
          scopestack->item->car.DEB_STARTSCOPE.end = p;
          PopScope();
          cdr_(p) = dbglist; dbglist = p;
        }
        last = p;
        dbglistscope = p;
      }
    }
  }
  if (debugging(DEBUG_Q)) cc_msg("scope %ld\n", entering);
  for (; newbll != oldbll; newbll = newbll->bllcdr)
  { SynBindList *bl;
    if (newbll == 0) syserr(syserr_dbg_scope);
    for (bl = newbll->bllcar; bl; bl = bl->bindlistcdr)
    { Binder *b = bl->bindlistcar;
      if (bindstg_(b) & b_dbgbit) continue; /* for this and next line */
      bindstg_(b) |= b_dbgbit;              /* see end of routine cmt */
      if (debugging(DEBUG_Q))
        cc_msg("  %s $b",
                entering>=0 ? "binding" : "unbinding",
                b);
      if (entering > 0)
        dbg_locvar1(b);
      if (debugging(DEBUG_Q))
        cc_msg("\n");
    }
  }
  return YES;
  /* Ask for INFOSCOPE item to get called back more or less immediately */
  /* from the local cg (INFOSCOPE item) to fill in the codeaddr         */
}

/* Dummy procedure not yet properly implemented, included here to keep in */
/* step with dbx.c */
void dbg_commblock(Binder *b, SynBindList *members, FileLine fl) {
  IGNORE(b); IGNORE(members); IGNORE(fl);
}

void dbg_define(char const *name, bool objectmacro, char const *body,
                dbg_ArgList const *args, FileLine fl) {
    /* Only representable in DWARF version 2 */
  IGNORE(name); IGNORE(objectmacro); IGNORE(body); IGNORE(args); IGNORE(fl);
  return;
}

void dbg_undef(char const *name, FileLine fl) {
    /* Only representable in DWARF version 2 */
  IGNORE(name); IGNORE(fl);
  return;
}

static int32 dw_1_lineinfo_size(void) {
  return dbg_coord_p == NULL ? 0 :
                               (length((List *)dbg_coord_p)+1) * 10 + 8;
}

static int32 src_mapped_codebase, src_mapped_codep;

void dbg_final_src_codeaddr(int32 code_base, int32 code_p)
{   src_mapped_codebase = code_base,
    src_mapped_codep = code_p;
}

static void dw_1_write_lineinfo(void) {
  Deb_filecoord *p = dbg_coord_p;
  DataXref *xrefs = NULL;
  int32 size = dw_1_lineinfo_size();
  int32 roundup = (size & 2) ? 2 : 0;
  dbg_coord_sentinel.codeseg = bindsym_(codesegment);
  *dbg_coord_q = &dbg_coord_sentinel;
  dbg_coord_sentinel.codeaddr = 
        (LanguageIsCPlusPlus) ? src_mapped_codebase+src_mapped_codep : codebase+codep;
  obj_startdebugarea(LINEINFOAREANAME);
  dw_write_w(size + roundup, 0);
  dw_write_w(p->codeaddr, 0);
  xrefs = dw_relocate(xrefs, 4, p->codeseg);
  for (; p != NULL; p = cdr_(p)) {
  /* Include the sentinel (to mark the end of info, and to hold
     the address of end of code-segment)
     Note that, despite the DWARF spec talking about deltas for
     the code addresses, it appears to mean from the section start,
     not from the previous statement.
   */
    dw_write_w(p->line, 0);
    dw_write_h(p->col, 0);
    dw_write_w(p->codeaddr, 0);
  }
  /* round up to 4-byte multiple */
  if (roundup) dw_write_h(0, 0);
  obj_enddebugarea(LINEINFOAREANAME, xrefs);
}

/* armobj.c calls writedebug to generate the debugging tables.   */
/* It must format them and then call obj_writedebug()            */
static unsigned32 dw_1_writeattribute(int attr, unsigned32 offset, AttribArg arg) {
  offset = dw_write_h(attr, offset);
  switch (attr & 15) {
  case FORM_ADDR:
  case FORM_REF:
    xrefs = dw_relocate(xrefs, offset, arg.ref.sym);
    return dw_write_w(arg.ref.n, offset);

  case FORM_BLOCK2:
  case FORM_DATA2:
    return dw_write_h(arg.ref.n, offset);

  case FORM_DATA4:
    if (arg.ref.sym != NULL) xrefs = dw_relocate(xrefs, offset, arg.ref.sym);
    return dw_write_w(arg.ref.n, offset);

  case FORM_BLOCK4:
    return dw_write_w(arg.ref.n, offset);

  case FORM_DATA8:
    { unsigned32 w[2];
      w[0] = arg.d[0]; w[1] = arg.d[1]; obj_writedebug(w, 2+DBG_INTFLAG);
      return offset + 8;
    }
  case FORM_STRING:
    return dw_inlinestring(arg.s, offset);

  default:
    syserr("dw_1_writeattribute(%x)", attr);
    return offset;
  }
}

static unsigned32 dw_1_writeattrib_u(int attr, unsigned32 offset, unsigned32 u) {
  AttribArg arg;
  arg.ref.n = u;
  arg.ref.sym = ((attr & 15) == FORM_REF) ? debug_sym : NULL;
  return dw_1_writeattribute(attr, offset, arg);
}

static unsigned32 dw_1_writeattrib_str(int attr, unsigned32 offset, char const *s) {
  AttribArg arg;
  arg.s = s;
  return dw_1_writeattribute(attr, offset, arg);
}

static unsigned32 dw_1_writeattrib_ref(int attr, unsigned32 offset, unsigned32 n, Symstr *sym) {
  AttribArg arg;
  arg.ref.n = n; arg.ref.sym = sym;
  return dw_1_writeattribute(attr, offset, arg);
}

static unsigned32 dw_1_attrib_size(int attr) {
  switch (attr & 15) {
  case FORM_ADDR:
  case FORM_REF:
  case FORM_DATA4:
  case FORM_BLOCK4: return 6;

  case FORM_BLOCK2:
  case FORM_DATA2:  return 4;

  case FORM_DATA8:  return 10;

  default:          syserr("dw_1_attrib_size(%x)", attr);
                    return 0;
  }
}

static unsigned32 dw_1_attrib_str_size(char const *s) {
  return (unsigned32)strlen(s) + 2 + 1;
}

static unsigned32 dw_1_hdr_size(void) {
  return 6 + dw_1_attrib_size(AT_sibling);
}

static unsigned32 dw_1_attrib_friend_size(Friend *amigos)
{ int32 n = 0;
  for (; amigos != NULL; amigos = amigos->friendcdr)
    if (h0_(amigos->u.friendfn) == s_binder)
      n += (bindstg_(amigos->u.friendfn) & b_undef) ? 0 : 1;
    else
      n++;
  return 4 * n + dw_1_attrib_size(AT_friends);
}

static unsigned32 dw_1_typeref_size(DbgList *typep) {
/* For DWARF version 1, qualified types do not have information items, nor do
   base types (the qualifiers and distinction between base and user types are
   encoded in the reference). It's uncertain whether pointer and reference
   types should be separate types or not: there are items for them, but also
   there are modifier values. For now, we make them items.
  */
  switch (typep->debsort) {
  case DW_TAG_base_type:
      return dw_1_attrib_size(AT_fund_type);

  case DW_TAG_fref:
  case DW_TAG_structure_type:
  case DW_TAG_union_type:
  case DW_TAG_class_type:
  case DW_TAG_enumeration_type:
  case DW_TAG_subroutine_type:
  case DW_TAG_array_type:
  case DW_TAG_typedef:
    return dw_1_attrib_size(AT_user_def_type);

  case DW_TAG_reference_type:
  case DW_TAG_pointer_type:
  case DW_TAG_const_type:
  case DW_TAG_volatile_type:
    { unsigned32 n = typep->car.DEB_QUALTYPE.n;
      DbgList *basetype = typep->car.DEB_QUALTYPE.basetype;
      if (basetype->debsort == DW_TAG_base_type) {
        if (basetype->car.DEB_BASETYPE.typecode == FT_void && n == 1 &&
            typep->car.DEB_QUALTYPE.qualmap[0] == MOD_pointer_to)
          /* special representation for void * */
          return dw_1_attrib_size(AT_fund_type);
        else
          return dw_1_attrib_size(AT_mod_fund_type) + n + 2; /* fundamental type code */
      } else
        return dw_1_attrib_size(AT_mod_u_d_type) + n + 4;  /* type ref */
    }

  default:
    syserr("dw_1_typeref_size %d", typep->debsort);
    return 0;
  }
}

static char unmangle_buf[256];

static char *dbg_unmangle(char *s)
{ if (strstr(s, "ct__F") == NULL &&
      strstr(s, "dt__F") == NULL &&
      strstr(s, "as__F") == NULL &&
      strstr(s, "__C") == NULL)
  { unmangle(s, unmangle_buf, 256);
    if (strcmp(s, unmangle_buf) != NULL)
    { char *t = strchr(unmangle_buf, '(');
      if (t != NULL && t < &unmangle_buf[256]) *t = '\0';
      return unmangle_buf;
    }
  }
  return s;
}

#define low_pc_(p)      (p->car.DEB_PROC.bodyaddr)
#define high_pc_(p)     (p->car.DEB_PROC.endproc->car.DEB_ENDPROC.endaddr)
static unsigned32 dw_1_itemsize(DbgList *p) {
/* must be kept in step with dw_1_write_info */
  unsigned32 n;
  switch (p->debsort) {
  case DW_TAG_compile_unit:
    n = dw_1_hdr_size() +
        dw_1_attrib_size(AT_language) +
        dw_1_attrib_str_size(p->car.DEB_SECTION.name);
    if (baseseg.len > 0) {
      n += dw_1_attrib_size(AT_low_pc) +
           dw_1_attrib_size(AT_high_pc);
    }
    if (dw_1_lineinfo_size() != 0) {
      n += dw_1_attrib_size(AT_stmt_list);
    }
    return n + dw_1_attrib_str_size(version_banner());

  case DW_TAG_subprogram:
    n = dw_1_hdr_size() +
        dw_1_attrib_str_size(dbg_unmangle(p->car.DEB_PROC.name));
    { DbgList *restype = p->car.DEB_PROC.type;
      if (restype->debsort != DW_TAG_base_type ||
          restype->car.DEB_BASETYPE.typecode != FT_void)
        n += dw_1_typeref_size(restype);
    }
    if (p->car.DEB_PROC.mem != NULL)
    { n += dw_1_typeref_size(p->car.DEB_PROC.mem);
      if (p->car.DEB_PROC.virtuality) n += dw_1_attrib_str_size("");
        /* /* and vtable offset?? */
    }
    if (p->car.DEB_PROC.inlined)
      n += dw_1_attrib_str_size("");
    return n + ((low_pc_(p) != 0 && high_pc_(p) != 0) ? dw_1_attrib_size(AT_low_pc) +
               dw_1_attrib_size(AT_high_pc) : 0);

  case DW_TAG_endproc:
    return 0;

  case DW_TAG_proctype_formal:
    n = dw_1_hdr_size() +
        dw_1_typeref_size(p->car.DEB_FORMAL.type);
    if (p->car.DEB_FORMAL.name!= NULL)
      n += dw_1_attrib_str_size(symname_(p->car.DEB_FORMAL.name));
    return n;

  case DW_TAG_subroutine_type:
    n = dw_1_hdr_size();
    { DbgList *restype = p->car.DEB_PROCTYPE.type;
      if (restype->debsort != DW_TAG_base_type ||
          restype->car.DEB_BASETYPE.typecode != FT_void)
        n += dw_1_typeref_size(restype);
    }
    return n;

  case DW_TAG_variable:
  case DW_TAG_formal_parameter:
    n = dw_1_hdr_size() +
        dw_1_attrib_str_size(dbg_unmangle(symname_(p->car.DEB_VAR.sym))) +
        dw_1_typeref_size(p->car.DEB_VAR.type) +
        dw_1_attrib_size(AT_location);
    if (p->car.DEB_VAR.mem != NULL)
        n += dw_1_typeref_size(p->car.DEB_VAR.mem);
    switch (p->car.DEB_VAR.stgclass) {
    case Stg_Extern:
    case Stg_Static:  return n + 5;
    case Stg_Reg:
    case Stg_ArgReg:  return n + 5;
    case Stg_Auto:
    case Stg_ArgAuto: return n + 11;
    default:          syserr("dw_1_itemsize: stg %p = %d", p, p->car.DEB_VAR.stgclass);
    }

  case DW_TAG_unspecified_parameters:
    return dw_1_hdr_size();

  case DW_TAG_typedef:
    return dw_1_hdr_size() +
           dw_1_attrib_str_size(p->car.DEB_TYPE.name) +
           dw_1_typeref_size(p->car.DEB_TYPE.type);

  case DW_TAG_fref:
    p->debsort = p->car.DEB_STRUCT.undefsort;
  case DW_TAG_class_type:
  case DW_TAG_union_type:
  case DW_TAG_structure_type:
    n = dw_1_hdr_size();
    if (p->car.DEB_STRUCT.name != NULL) 
        n += dw_1_attrib_str_size(p->car.DEB_STRUCT.name);
    if (p->car.DEB_STRUCT.size != 0)  n += dw_1_attrib_size(AT_byte_size);
    if (p->car.DEB_STRUCT.friends != NULL)
        n += dw_1_attrib_friend_size(p->car.DEB_STRUCT.friends);
    return n;

  case DW_TAG_volatile_type:
  case DW_TAG_const_type:
  case DW_TAG_pointer_type:
  case DW_TAG_reference_type:
    return 0;

  case DW_TAG_ptr_to_member_type:
    n = dw_1_hdr_size() +
        dw_1_typeref_size(p->car.DEB_PTRTOMEMTYPE.container) +
        dw_1_typeref_size(p->car.DEB_PTRTOMEMTYPE.type);
    return n;

  case DW_TAG_member:
    n = dw_1_hdr_size() +
        dw_1_attrib_str_size(p->car.DEB_MEMBER.name) +
        dw_1_typeref_size(p->car.DEB_MEMBER.type);

    if (!p->car.DEB_MEMBER.decl)                /* static member, no AT_location */
      n += dw_1_attrib_size(AT_location) +
        6  /* op_const(n) op_add */;
    if (p->car.DEB_MEMBER.bsize != 0) {
      n += dw_1_attrib_size(AT_bit_size) +
           dw_1_attrib_size(AT_bit_offset);
    }
    return n;

  case DW_TAG_inheritance:
    n = dw_1_hdr_size() +
        dw_1_typeref_size(p->car.DEB_INHERIT.type) +
        dw_1_attrib_size(AT_location) +
        6;
    if (p->car.DEB_INHERIT.virtuality)
      n += dw_1_attrib_str_size("");
    return n;

  case DW_TAG_enumeration_type:
    n = dw_1_hdr_size();
    if (p->car.DEB_ENUM.name != NULL)
      n += dw_1_attrib_str_size(p->car.DEB_ENUM.name);
    n += dw_1_attrib_size(AT_byte_size);
    { DbgList *elts = p->car.DEB_ENUM.children;
      n += dw_1_attrib_size(AT_element_list);
      for (; elts != NULL; elts = elts->sibling)
        if (elts->debsort == DW_TAG_enumerator) {
          n += 4 + strlen(elts->car.DEB_ENUMERATOR.name) + 1;
        }
    }
    return n;

  case DW_TAG_enumerator:
    return 0;   /* DWARF version 2 only */

  case TAG_padding: /* null entry to terminate sibling chains */
    { DbgList *l = *p->car.DEB_NULL.parent;
      for (; l != p; l = l->sibling)
        if (l->dbgloc != 0) return 4;
      p->debsort = DW_TAG_ignore;
      return 0;
    }

  case DW_TAG_array_type:
    n = dw_1_hdr_size() +
        dw_1_attrib_size(AT_subscr_data);
    n += p->car.DEB_ARRAY.open ? 10 : 12;
    return n + dw_1_typeref_size(p->car.DEB_ARRAY.basetype);

  case DW_TAG_lexical_block:
    return dw_1_hdr_size() +
           dw_1_attrib_size(AT_low_pc) +
           dw_1_attrib_size(AT_high_pc);

  case DW_TAG_ignore:
  case DW_TAG_end_lexical_block:
    return 0;

  default:
    syserr("dw_1_itemsize %d", p->debsort);
    return 0;
  }
}

static unsigned32 dw_this_itemsize;

static unsigned32 dw_1_hdr(DbgList *p, int itemsort, unsigned32 offset) {
  dw_this_itemsize = dw_1_itemsize(p);
  offset = dw_write_w(dw_this_itemsize, offset);
  offset = dw_write_h(itemsort, offset);
  { DbgList *sib = p->sibling;
    for (; sib->sibling != 0; sib = sib->sibling)
      if (sib->dbgloc != 0) break;
    return dw_1_writeattrib_u(AT_sibling, offset, sib->dbgloc);
  }
}

static unsigned32 dw_1_writetyperef(DbgList *typep, unsigned32 offset) {
/* For DWARF version 1, qualified types do not have information items, nor do
   base types (the qualifiers and distinction between base and user types are
   encoded in the reference). It's uncertain whether pointer and reference
   types should be separate types or not: there are items for them, but also
   there are modifier values. For now, we use modifiers.
  */
  switch (typep->debsort) {
  case DW_TAG_base_type:
    return dw_1_writeattrib_u(AT_fund_type, offset, typep->car.DEB_BASETYPE.typecode);

  case DW_TAG_structure_type:
  case DW_TAG_union_type:
  case DW_TAG_class_type:
  case DW_TAG_enumeration_type:
  case DW_TAG_subroutine_type:
  case DW_TAG_array_type:
  case DW_TAG_typedef:
    return dw_1_writeattrib_u(AT_user_def_type, offset, typep->dbgloc);

  case DW_TAG_reference_type:
  case DW_TAG_pointer_type:
  case DW_TAG_const_type:
  case DW_TAG_volatile_type:
    { int n = typep->car.DEB_QUALTYPE.n;
      Dw_TypeRep *basetype = typep->car.DEB_QUALTYPE.basetype;
      if (basetype->debsort == DW_TAG_base_type) {
        if (basetype->car.DEB_BASETYPE.typecode == FT_void && n == 1 &&
            typep->car.DEB_QUALTYPE.qualmap[0] == MOD_pointer_to)
          /* special representation for void * */
          return dw_1_writeattrib_u(AT_fund_type, offset, FT_pointer);
        else {
          offset = dw_1_writeattrib_u(AT_mod_fund_type, offset, n+2);
          obj_writedebug(typep->car.DEB_QUALTYPE.qualmap, n);
          return dw_write_h(basetype->car.DEB_BASETYPE.typecode, offset+n);
        }
      } else {
        offset = dw_1_writeattrib_u(AT_mod_u_d_type, offset, n+4);
        obj_writedebug(typep->car.DEB_QUALTYPE.qualmap, n);
        offset += n;
        xrefs = dw_relocate(xrefs, offset, debug_sym);
        return dw_write_w(basetype->dbgloc, offset);
      }
    }

  default:
    syserr("dw_1_writetype %d", typep->debsort);
    return 0;
  }
}

static unsigned32 dw_1_write_friends(unsigned32 offset, Friend *amigos)
{ DbgList *p = dbglist;
  offset = dw_write_h(AT_friends, offset);
  offset = dw_write_h(dw_1_attrib_friend_size(amigos)-dw_1_attrib_size(AT_friends), offset);
  for (; amigos != NULL ; amigos = amigos->friendcdr)
  { xrefs = dw_relocate(xrefs, offset, debug_sym);
    if (h0_(amigos->u.friendclass) == s_tagbind)
      offset = dw_write_w(((Dw_TypeRep *)((TagBinder *)amigos->u.friendclass)->tagbinddbg)->dbgloc, offset);
    else
      for (; p != NULL; p = cdr_(p))
        if (!(bindstg_(amigos->u.friendfn) & b_undef) &&
            p->debsort == DW_TAG_subprogram &&
            p->car.DEB_PROC.name == symname_(bindsym_(amigos->u.friendfn)))
          { offset = dw_write_w(p->dbgloc, offset); break;}
  }
  return offset;
}

static void dw_1_write_info(void)
{ DbgList *p;
  unsigned32 infosize, offset, roundup;
  xrefs = NULL;
  /* produce structure descriptors for forward referenced structures for
   * which there hasn't been a real declaration
   */
  obj_startdebugarea(DEBUGAREANAME);
  PopScope();
  dbglist = (DbgList *)dreverse((List *)dbglist);

  for (p = dbglist; p != NULL; p = cdr_(p))
    if (p->debsort == DW_TAG_subprogram && p->car.DEB_PROC.variadic) {
      DbgList *q,
              **prevp = &cdr_(p),
              **prevsibling = &p->car.DEB_PROC.children;
      for (q = p->car.DEB_PROC.children; q != NULL; q = q->sibling)
        if (q->debsort == DW_TAG_formal_parameter) {
          prevsibling = &q->sibling;
          prevp = &cdr_(q);
        }

      q = (DbgList *)DbgAlloc(sizeof(dbglist->car.DEB_REST)+offsetof(DbgList,car));
      q->debsort = DW_TAG_unspecified_parameters;
      q->sibling = *prevsibling; cdr_(q) = *prevp;
      *prevsibling = q; *prevp = q;
    }

  for (infosize = 0, p = dbglist; p != NULL; p = cdr_(p)) {
    unsigned32 n = dw_1_itemsize(p);
    p->dbgloc = n == 0 ? 0 : infosize;
    infosize += n;
  }

  if (infosize & 3)
    roundup = 8 - (infosize & 3);
  else
    roundup = 0;

  for (offset = 0, p = dbglist; p != NULL; p = cdr_(p)) {
    int sort = p->debsort;
    unsigned32 start = offset;
    switch (sort) {
    default:
      syserr(syserr_dbg_write, (long)sort);
      break;

    case DW_TAG_compile_unit:
      { DbgList dummy;
        dummy.dbgloc = infosize + roundup;
        p->sibling = &dummy;
        offset = dw_1_hdr(p, TAG_compile_unit, offset);
      }
      offset = dw_1_writeattrib_u(AT_language, offset, 
                (LanguageIsCPlusPlus) ? LANG_C_PLUS_PLUS : LANG_C89);
      offset = dw_1_writeattrib_str(AT_name, offset, p->car.DEB_SECTION.name);
      if (baseseg.len > 0) {
        offset = dw_1_writeattrib_ref(AT_low_pc, offset, 0, p->car.DEB_SECTION.codeseg);
        offset = dw_1_writeattrib_ref(AT_high_pc, offset, baseseg.len, p->car.DEB_SECTION.codeseg);
      }
      if (dw_1_lineinfo_size() != 0) {
        offset = dw_1_writeattrib_ref(AT_stmt_list, offset, 0, lineinfo_sym);
      }
      offset = dw_1_writeattrib_str(AT_producer, offset, version_banner());
      break;

    case DW_TAG_subprogram:
      offset = dw_1_hdr(p, p->car.DEB_PROC.global ? TAG_global_subroutine : TAG_subroutine, offset);
      offset = dw_1_writeattrib_str(AT_name, offset, dbg_unmangle(p->car.DEB_PROC.name));
      { DbgList *restype = p->car.DEB_PROC.type;
        if (restype->debsort != DW_TAG_base_type ||
            restype->car.DEB_BASETYPE.typecode != FT_void)
          offset = dw_1_writetyperef(restype, offset);
      }
      if (low_pc_(p) != 0 && high_pc_(p) != 0)
      {
      offset = dw_1_writeattrib_ref(AT_low_pc, offset, p->car.DEB_PROC.bodyaddr, 
                        p->car.DEB_PROC.codeseg);
      offset = dw_1_writeattrib_ref(AT_high_pc, offset, 
                p->car.DEB_PROC.endproc->car.DEB_ENDPROC.endaddr, p->car.DEB_PROC.codeseg);
      }
      if (p->car.DEB_PROC.mem != NULL)
      { offset = dw_1_writeattrib_ref(AT_member, offset, p->car.DEB_PROC.mem->dbgloc, 
                        debug_sym);
        if (p->car.DEB_PROC.virtuality)
          offset = dw_1_writeattrib_str(AT_virtual, offset, "");
      }
      if (p->car.DEB_PROC.inlined)
        offset = dw_1_writeattrib_str(AT_inline, offset, "");
      break;

    case DW_TAG_endproc:
      break;

    case DW_TAG_unspecified_parameters:
      offset = dw_1_hdr(p, TAG_unspecified_parameters, offset);
      break;

    case DW_TAG_proctype_formal:
      offset = dw_1_hdr(p, TAG_formal_parameter, offset);
      if (p->car.DEB_FORMAL.name != NULL)
        offset = dw_1_writeattrib_str(AT_name, offset, symname_(p->car.DEB_FORMAL.name));
      offset = dw_1_writetyperef(p->car.DEB_FORMAL.type, offset);
      break;

    case DW_TAG_subroutine_type:
      offset = dw_1_hdr(p, TAG_subroutine_type, offset);
      { DbgList *restype = p->car.DEB_PROCTYPE.type;
        if (restype->debsort != DW_TAG_base_type ||
            restype->car.DEB_BASETYPE.typecode != FT_void)
          offset = dw_1_writetyperef(restype, offset);
      }
      break;

    case DW_TAG_ptr_to_member_type:
      offset = dw_1_hdr(p, TAG_ptr_to_member_type, offset);
      offset = dw_1_writetyperef(p->car.DEB_PTRTOMEMTYPE.container, offset);
      offset = dw_1_writetyperef(p->car.DEB_PTRTOMEMTYPE.type, offset);
      break;

    case DW_TAG_variable:
      sort = p->car.DEB_VAR.stgclass == Stg_Extern ? TAG_global_variable : TAG_local_variable;
    case DW_TAG_formal_parameter:
      offset = dw_1_hdr(p, sort, offset);
      offset = dw_1_writeattrib_str(AT_name, offset, dbg_unmangle(symname_(p->car.DEB_VAR.sym)));
      offset = dw_1_writetyperef(p->car.DEB_VAR.type, offset);
      if (p->car.DEB_VAR.mem != NULL)
        offset = dw_1_writeattrib_ref(AT_member, offset, p->car.DEB_VAR.mem->dbgloc, debug_sym);
      offset = dw_write_h(AT_location, offset);
      switch (p->car.DEB_VAR.stgclass) {
      case Stg_Extern:
      case Stg_Static:
        offset = dw_write_h(5, offset);
        offset = dw_write_b(OP_ADDR, offset);
        { Symstr *s = p->car.DEB_VAR.base.sym;
          obj_symref(s, symext_(s) == NULL ? xr_data|xr_weak : xr_data, 0);
          xrefs = dw_relocate(xrefs, offset, s);
        }
        offset = dw_write_w(p->car.DEB_VAR.location, offset);
        break;

      case Stg_Reg:
      case Stg_ArgReg:
        offset = dw_write_h(5, offset);
        offset = dw_write_b(OP_REG, offset);
        offset = dw_write_w(p->car.DEB_VAR.location, offset);
        break;

      case Stg_Auto:
      case Stg_ArgAuto:
        offset = dw_write_h(11, offset);
        offset = dw_write_b(OP_BASEREG, offset);
        offset = dw_write_w(p->car.DEB_VAR.base.r, offset);
        offset = dw_write_b(OP_CONST, offset);
        offset = dw_write_w(p->car.DEB_VAR.location, offset);
        offset = dw_write_b(OP_ADD, offset);
        break;
      }
      break;

    case DW_TAG_typedef:
      offset = dw_1_hdr(p, TAG_typedef, offset);
      offset = dw_1_writeattrib_str(AT_name, offset, p->car.DEB_TYPE.name);
      offset = dw_1_writetyperef(p->car.DEB_TYPE.type, offset);
      break;

    case DW_TAG_class_type:
    case DW_TAG_union_type:
    case DW_TAG_structure_type:
      offset = dw_1_hdr(p, p->debsort, offset);
      if (p->car.DEB_STRUCT.name != NULL)
        offset = dw_1_writeattrib_str(AT_name, offset, p->car.DEB_STRUCT.name);
      if (p->car.DEB_STRUCT.size != 0)
        offset = dw_1_writeattrib_u(AT_byte_size, offset, p->car.DEB_STRUCT.size);
      if (p->car.DEB_STRUCT.friends != NULL)
        offset = dw_1_write_friends(offset, p->car.DEB_STRUCT.friends);
      break;

    case DW_TAG_volatile_type:
    case DW_TAG_const_type:
    case DW_TAG_pointer_type:
    case DW_TAG_reference_type:
      break;

    case DW_TAG_member:
      offset = dw_1_hdr(p, TAG_member, offset);
      offset = dw_1_writeattrib_str(AT_name, offset, p->car.DEB_MEMBER.name);
      offset = dw_1_writetyperef(p->car.DEB_MEMBER.type, offset);
      if (p->car.DEB_MEMBER.bsize != 0) {
        offset = dw_1_writeattrib_u(AT_bit_size, offset, p->car.DEB_MEMBER.bsize);
        offset = dw_1_writeattrib_u(AT_bit_offset, offset, p->car.DEB_MEMBER.boffset);
      }
      if (!p->car.DEB_MEMBER.decl)
      { offset = dw_1_writeattrib_u(AT_location, offset, 6);
        offset = dw_write_b(OP_CONST, offset);
        offset = dw_write_w(p->car.DEB_MEMBER.offset, offset);
        offset = dw_write_b(OP_ADD, offset);
      }
      break;

    case DW_TAG_inheritance:
      offset = dw_1_hdr(p, TAG_inheritance, offset);
      offset = dw_1_writetyperef(p->car.DEB_INHERIT.type, offset);
      offset = dw_1_writeattrib_u(AT_location, offset, 6);
      offset = dw_write_b(OP_CONST, offset);
      offset = dw_write_w(p->car.DEB_INHERIT.offset, offset);
      offset = dw_write_b(OP_ADD, offset);
      if (p->car.DEB_INHERIT.virtuality)
        offset = dw_1_writeattrib_str(AT_virtual, offset, "");
      break;

    case DW_TAG_enumeration_type:
      offset = dw_1_hdr(p, TAG_enumeration_type, offset);
      if (p->car.DEB_ENUM.name != NULL)
        offset = dw_1_writeattrib_str(AT_name, offset, p->car.DEB_ENUM.name);
      offset = dw_1_writeattrib_u(AT_byte_size, offset, p->car.DEB_ENUM.size);
      { DbgList *elts = p->car.DEB_ENUM.children;
        DbgList *prev = NULL, *next;
        /* DWARF version 1 requires the enumeration members in reverse order.
           Pre-reverse them. (And reverse back on writing)
         */
        for (; elts != NULL; prev = elts, elts = next) {
          next = elts->sibling;
          elts->sibling = prev;
        }
        elts = prev;
        offset = dw_1_writeattrib_u(AT_element_list, offset,
                                    dw_this_itemsize - (offset + 6 - start));
        prev = NULL;
        for (; elts != NULL; prev = elts, elts = next) {
          if (elts->debsort == DW_TAG_enumerator) {
          /* This just ignores the sibling list terminator */
            offset = dw_write_w(elts->car.DEB_ENUMERATOR.val, offset);
            offset = dw_inlinestring(elts->car.DEB_ENUMERATOR.name, offset);
          }
          next = elts->sibling;
          elts->sibling = prev;
        }
      }
      break;

    case DW_TAG_enumerator:
      break;   /* DWARF version 2 only */

    case TAG_padding: /* null entry to terminate sibling chains */
      offset = dw_write_w(4, offset);
      break;

    case DW_TAG_array_type:
      offset = dw_1_hdr(p, TAG_array_type, offset);
      offset = dw_1_writeattrib_u(AT_subscr_data, offset,
                                  dw_1_typeref_size(p->car.DEB_ARRAY.basetype) + (p->car.DEB_ARRAY.open ? 10 : 12));
      offset = dw_write_b(p->car.DEB_ARRAY.open ? FMT_FT_C_X : FMT_FT_C_C, offset);
      offset = dw_write_h(FT_signed_integer, offset);
      offset = dw_write_w(p->car.DEB_ARRAY.lowerbound, offset);
      if (p->car.DEB_ARRAY.open)
        offset = dw_write_h(0, offset);
      else
        offset = dw_write_w(p->car.DEB_ARRAY.upperbound, offset);
      offset = dw_write_b(FMT_ET, offset);
      offset = dw_1_writetyperef(p->car.DEB_ARRAY.basetype, offset);
      break;

    case DW_TAG_lexical_block:
      offset = dw_1_hdr(p, TAG_lexical_block, offset);
      offset = dw_1_writeattrib_ref(AT_low_pc, offset, p->car.DEB_STARTSCOPE.s.codeaddr, p->car.DEB_STARTSCOPE.codeseg);
      offset = dw_1_writeattrib_ref(AT_high_pc, offset, p->car.DEB_STARTSCOPE.end->car.DEB_ENDSCOPE.s.codeaddr, p->car.DEB_STARTSCOPE.codeseg);
      break;

    case DW_TAG_ignore:
    case DW_TAG_end_lexical_block:
      break;
    }
  }
  if (roundup != 0) {
    unsigned32 w = 0;
    dw_write_w(roundup, offset);
    obj_writedebug(&w, roundup - 4);
  }
  obj_enddebugarea(DEBUGAREANAME, xrefs);
}

static unsigned32 dw_nameindex_size;
static DbgList *dw_section;

static unsigned32 dw_1or2_nameindex_size(void) {
  DbgList const *p = dw_section->car.DEB_SECTION.children;
  unsigned32 n = 0;
  for (; p != NULL; p = p->sibling)
    switch (p->debsort) {
    case DW_TAG_subprogram:
      n += strlen(dbg_unmangle(p->car.DEB_PROC.name)) + 5;
      break;

    case DW_TAG_variable:
      n += strlen(dbg_unmangle(symname_(p->car.DEB_VAR.sym))) + 5;
      break;

    case DW_TAG_typedef:
      n += strlen(p->car.DEB_TYPE.name) + 5;
      break;

    case DW_TAG_fref:
    case DW_TAG_class_type:
    case DW_TAG_union_type:
    case DW_TAG_structure_type:
      if (p->car.DEB_STRUCT.name != NULL)
        n += strlen(p->car.DEB_STRUCT.name) + 5;
      break;

    case DW_TAG_enumeration_type:
      if (p->car.DEB_ENUM.name != NULL)
        n += strlen(p->car.DEB_ENUM.name) + 5;
      { DbgList *elts = p->car.DEB_ENUM.children;
        for (; elts != NULL; elts = elts->sibling)
          if (elts->debsort == DW_TAG_enumerator)
            n += strlen(elts->car.DEB_ENUMERATOR.name) + 5;
      }
      break;

    default:
      break;
    }
  return n;
}

static void dw_1or2_write_nameindex_entry(DbgList const *p, char const *s) {
  dw_write_w(p->dbgloc, 0);
  dw_inlinestring(s, 0);
}

static void dw_1_write_nameindex(void) {
  DbgList const *p = dw_section->car.DEB_SECTION.children;
  DataXref *xrefs = NULL;
  unsigned32 size = dw_nameindex_size + 9 + /* header (excluding length word) */
                                        4;  /* terminator */
  unsigned32 roundup = (size & 3) ? 4 - (size & 3) : 0;
  obj_startdebugarea(NAMEINDEXAREANAME);
  dw_write_w(size + roundup, 0);
  dw_write_b(1, 0);
  dw_write_w(0, 0);
  dw_write_w(baseseg.len, 0);
  xrefs = dw_relocate(xrefs, 5, debug_sym);
  for (; p != NULL; p = p->sibling)
    switch (p->debsort) {
    case DW_TAG_subprogram:
      dw_1or2_write_nameindex_entry(p, dbg_unmangle(p->car.DEB_PROC.name));
      break;

    case DW_TAG_variable:
      dw_1or2_write_nameindex_entry(p, dbg_unmangle(symname_(p->car.DEB_VAR.sym)));
      break;

    case DW_TAG_typedef:
      dw_1or2_write_nameindex_entry(p, p->car.DEB_TYPE.name);
      break;

    case DW_TAG_class_type:
    case DW_TAG_union_type:
    case DW_TAG_structure_type:
      if (p->car.DEB_STRUCT.name != NULL)
        dw_1or2_write_nameindex_entry(p, p->car.DEB_STRUCT.name);
      break;

    case DW_TAG_enumeration_type:
      if (p->car.DEB_ENUM.name != NULL)
        dw_1or2_write_nameindex_entry(p, p->car.DEB_ENUM.name);
      { DbgList *elts = p->car.DEB_ENUM.children;
        for (; elts != NULL; elts = elts->sibling)
          if (elts->debsort == DW_TAG_enumerator)
            dw_1or2_write_nameindex_entry(p, elts->car.DEB_ENUMERATOR.name);
      }
      break;

    default:
      break;
    }
  dw_write_w(0, 0);
  if (roundup) {
    unsigned32 w = 0;
    obj_writedebug(&w, roundup);
  }
  obj_enddebugarea(NAMEINDEXAREANAME, xrefs);
}

#ifdef TARGET_HAS_FP_OFFSET_TABLES

void obj_notefpdesc(ProcFPDesc const *fpd) {
  /* Only representable in DWARF version 2 */
  IGNORE(fpd);
  return;
}

#endif

void dbg_finalise(void) {
  dbg_init_done = dbg_sub_init_done = NO;
}

bool dbg_debugareaexists(char const *name) {
  if (strcmp(name, DEBUGAREANAME) == 0)
    return dbglist != NULL;
  else if (strcmp(name, LINEINFOAREANAME) == 0)
    return dw_1_lineinfo_size() != 0;
  else if (strcmp(name, NAMEINDEXAREANAME) == 0)
    return (dw_nameindex_size = dw_1or2_nameindex_size()) != 0;
  else if (strcmp(name, RANGEINDEXAREANAME) == 0)
    return NO;
  return NO;
}

void dbg_writedebug(void) {
  if (dbglist != NULL) dw_1_write_info();
  if (dw_1_lineinfo_size() != 0) dw_1_write_lineinfo();
  if (dw_nameindex_size != 0) dw_1_write_nameindex();
}

static void dbg_sub_init(void) {
  dbglist = NULL; basetypes = NULL;
  baseseg.len = 0;
  dbglistproc = 0;
  dbglistscope = 0;
  dbg_filelist = 0, dbg_coord_p = 0; dbg_coord_q = &dbg_coord_p;
  dbg_loclist = 0;
  dbg_sub_init_done = YES;
  dw_nameindex_size =0;
}

void dbg_setformat(int format) {
  if (format == 0) format = 1;  /* -dwarf on its own defaults to dwarf version 1 */
  dwarf_version = format;
}

void dbg_init(void) {
  if (!dbg_sub_init_done) dbg_sub_init();
  baseseg.sym = bindsym_(codesegment);
  if (usrdbg(DBG_ANY))
  { DbgList *p = DbgListAlloc(DEB_SECTION);
    dw_section = p;
    debug_sym = obj_notedebugarea(DEBUGAREANAME);
    lineinfo_sym = obj_notedebugarea(LINEINFOAREANAME);
    obj_notedebugarea(NAMEINDEXAREANAME);
    obj_notedebugarea(RANGEINDEXAREANAME);
    p->debsort = DW_TAG_compile_unit;
    p->car.DEB_SECTION.name = sourcefile;
    p->car.DEB_SECTION.codeseg = bindsym_(codesegment);
    p->cdr = NULL;
    freescopes = NULL;
    scopestack = NULL; PushScope(p, &p->car.DEB_SECTION.children);
    { DbgList *q, **pp = &dbglist;
      for (; (q = *pp) != NULL; pp = &q->cdr) continue;
      *pp = p;
    }
    if (usrdbg(DBG_LINE))
    { Deb_filelist *x = dbg_filelist;
      for (; x != NULL; x = x->nextfile) {
        Deb_filecoord *l = x->linelist;
        for (; l != NULL; l = l->nextinfile)
          l->codeseg = bindsym_(codesegment);
      }
    }
  }
  dbg_init_done = YES;
}

#endif /* TARGET_HAS_DWARF */

#endif /* TARGET_HAS_DEBUGGER */

/* End of mip/dwarf.c */
