/*
 * codebuf.h : target-independent part of object code generation, version 23
 * Copyright (C) Acorn Computers Ltd., 1988-1990.
 * Copyright (C) Codemist Ltd, 1988-1992.
 * Copyright (C) Advanced Risc Machines Ltd., 1991-1992.
 */

/*
 * RCS $Revision: 1.18 $
 * Checkin $Date: 1996/01/10 14:54:25 $
 * Revising $Author: hmeeking $
 */

#ifndef _codebuf_LOADED
#define _codebuf_LOADED 1

#ifndef _defs_LOADED
#  include "defs.h"
#endif
#include "xrefs.h"

#ifdef TARGET_ASM_NAMES_LITERALS
#  define genlitbinder(typ) \
       global_mk_binder(0, gensymval(2), bitofstg_(s_static), typ)
#  define lit_keepname(b) 1
#else
#  define genlitbinder(typ) \
       mk_binder(gensymval(0), bitofstg_(s_static), typ)
#  define lit_keepname(b) 0
#endif

typedef struct DataDesc {
    DataInit *head, *tail;
    int32 size;
    DataXref *xrefs;
    int xrarea;

    union { int32 w32[1]; int16 w16[2]; int8 w8[4]; } wbuff;
    uint8 wpos;
    uint8 wtype;
} DataDesc;

#ifndef TARGET_IS_INTERPRETER
extern DataDesc data;           /* hard for fgrep to find!              */
#define vardata data            /* easier for fgrep to find!            */
#ifdef CONST_DATA_IN_CODE
extern DataDesc constdata;
#else
#define dataloc   (data.size)   /* compatibility with other back ends.  */
#define datainitp (data.head)
#define dataxrefs (data.xrefs)
#endif
extern DataDesc *datap;

extern int32 code_area_idx;     /* for armcc -S -ZO ... */

extern int32 codebase;
extern struct LabelNumber *litlab;
extern int32 litpoolp;
extern int32 codep;

#ifdef TARGET_HAS_BSS
extern int32 bss_size;
#endif
#endif

/* xxx/gen.c obj.c and asm.c should now only access 'codeandflagvec'    */
/* via the macros code_xxx_() below.                                    */

/* ECN: Modified to allow non-aligned words if TARGET_HAS_HALFWORD_INSTRUCTIONS
 *      Unfortunately there are large scale assumptions elsewhere that
 *      codeandflagvec addresses an array of 32 bit words.
 */
#ifdef TARGET_HAS_HALFWORD_INSTRUCTIONS
#  define CodeFlag_t unsigned char
extern struct CodeAndFlag { unsigned16 code[CODEVECSEGSIZE*2];
                            CodeFlag_t flag[CODEVECSEGSIZE*2]; }
                   *codeandflagvec[CODEVECSEGMAX];
#define code_byte_(q) ((unsigned8 *)(codeandflagvec[(q) >> CODEVECSEGBITS+2]->code)) \
                                    [(q) & (CODEVECSEGSIZE*4-1)]
#define code_hword_(q) (codeandflagvec[(q) >> (CODEVECSEGBITS+2)])-> \
                                code[((q) >> 1) & (CODEVECSEGSIZE*2-1)]
#define code_flag_(q) (codeandflagvec[(q) >> (CODEVECSEGBITS+2)])-> \
                                flag[((q) >> 1) & (CODEVECSEGSIZE*2-1)]
#define code_inst_(q) (host_lsbytefirst ? \
                        (code_hword_(q) | (code_hword_(q+2) << 16)) : \
                        ((code_hword_(q) << 16) | code_hword_(q+2)))
#define set_code_inst_(q,v) (host_lsbytefirst ? \
        ((code_hword_(q) = (unsigned16)(v)), (code_hword_((q)+2) = (v) >> 16)) : \
        ((code_hword_(q) = (v) >> 16), (code_hword_((q)+2) = (unsigned16)(v))))
#else
#ifdef TARGET_HAS_BYTE_INSTRUCTIONS
#  define CodeFlag_t int32      /* dying?  */
#else
#  define CodeFlag_t unsigned char
#endif
extern struct CodeAndFlag { int32 code[CODEVECSEGSIZE];
                            CodeFlag_t flag[CODEVECSEGSIZE]; }
                   *codeandflagvec[CODEVECSEGMAX];
#define code_inst_(q) (codeandflagvec[(q) >> CODEVECSEGBITS+2])-> \
                                      code[(q)>>2 & CODEVECSEGSIZE-1]
#define set_code_inst_(q,v) (code_inst_(q)=v)
#define code_flag_(q) (codeandflagvec[(q)>>CODEVECSEGBITS+2])-> \
                                      flag[(q)>>2 & CODEVECSEGSIZE-1]
#endif
#ifndef NO_ASSEMBLER_OUTPUT     /* i.e. lay off otherwise */
  extern VoidStar (*(codeasmauxvec[CODEVECSEGMAX]))[CODEVECSEGSIZE];
# define code_aux_(q)  (*codeasmauxvec[(q) >> CODEVECSEGBITS+2]) \
                                      [(q)>>2 & CODEVECSEGSIZE-1]
#endif

/* The following macros provide easy access to blocks for xxx/obj.c     */
#define code_instvec_(i)   (&(codeandflagvec[i]->code)[0])
#define code_flagvec_(i)   (&(codeandflagvec[i]->flag)[0])

#ifdef TARGET_HAS_BYTE_INSTRUCTIONS
#define code_byte_(q) \
   ((unsigned char *)(codeandflagvec[(q) >> CODEVECSEGBITS+2]->code)) \
                            [(q) & 4*CODEVECSEGSIZE-1]
/* miserable macro to interpret int32 flags as 4 byte flags.  Hmm.      */
#define flag_byte_(q) \
   ((unsigned char *)(codeandflagvec[(q) >> CODEVECSEGBITS+2]->flag)) \
                            [(q) & 4*CODEVECSEGSIZE-1]
#endif
extern struct LabList *asm_lablist; /* exported to xxxgen.c */

extern struct LabelNumber *nextlabel(void);

extern void labeldata(Symstr *b);
extern int32 trydeletezerodata(DataInit *previous, int32 minsize);

extern void gendlabel(Binder *b, bool topflag, DataDesc *seg);
extern void gendcAX(Symstr *sv, int32 offset, int xrflavour);
 /* (possibly external) name + offset, flavour is xr_data or xr_code */
#ifdef TARGET_IS_INTERPRETER
#define gendc0(n) 0
#else
extern void gendc0(int32 nbytes);
#endif
extern void gendcI_a(int32 len, int32 val, bool aligned);
#define gendcI(len, val) gendcI_a(len, val, YES)
extern void gendcE(int32 len, FloatCon *val);
extern Binder *gendcSlit(StringSegList *s);
#ifdef TARGET_CALL_USES_DESCRIPTOR
extern void gendcF(Symstr *sv, int32 offset);
extern int32 genfncon(Symstr* sv);
#else /* TARGET_CALL_USES_DESCRIPTOR */
#  define gendcF(sym, off, strength) gendcAX(sym, off, xr_code|(strength))
#endif /* TARGET_CALL_USES_DESCRIPTOR */
#define gendcA(sym, off, strength) gendcAX(sym, off, xr_data|(strength))

extern void vg_genstring(StringSegList *s, int32 size, int pad);

extern void padstatic(int32 align);

#ifdef TARGET_HAS_BSS
extern void padbss(int32 align);
#endif

int32 totargetsex(int32 w, int flag);

extern void codeseg_stringsegs(StringSegList *x, bool incode);
extern void codeseg_flush(Symstr *strlitname);
extern int32 codeseg_function_name(Symstr *name, int32 argn);
extern void show_entry(Symstr *name, int flags);
extern void show_code(Symstr *name);
extern void outcodeword(int32 w, int32 f);
extern void outcodewordaux(int32 w, int32 f, VoidStar aux);
extern void outlitword(int32 w, int32 flavour, Symstr *sym, VoidStar aux,
                       int32 flag);
extern int32 codeloc(void);
extern int32 lit_findadcon(Symstr *name, int32 offset, int32 wherefrom);
extern void dumplits2(bool needsjump);
extern int lit_of_count_name(char *s);
extern void dump_count_names(void);
extern int32 lit_findword(int32 w, int32 flavour, Symstr *sym, int32 flag);
extern int32 lit_findwordaux(int32 w, int32 flavour, VoidStar aux, int32 flag);
extern int32 lit_findwordsincurpool(int32 w[], int32 count, int32 flavour);
extern int32 lit_findwordsinprevpools(int32 w[], int32 count,
                                      int32 flavour, int32 earliest);
extern int32 lit_findstringincurpool(StringSegList *s);
extern int32 lit_findstringinprevpools(StringSegList *s, int32 earliest);

extern int32 stringlength(StringSegList *s);

extern int32 addbsssym(Symstr *sym, int32 size, int32 align, bool statik, bool local);

extern void codebuf_init(void);
extern void codebuf_reinit(void);
extern void codebuf_reinit1(char * codeseg_name);   /* for C++ dynamic inits */
extern void codebuf_reinit2(void);
extern void codebuf_tidy(void);

#ifdef TARGET_IS_ACW
extern void outbytef(int32 w, int f);
#endif

#endif

/* end of codebuf.h */
