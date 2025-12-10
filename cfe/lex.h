/*
 * C compiler file cfe/lex.h:
 * Copyright (C) Acorn Computers Ltd., 1988-1990.
 * Copyright (C) Codemist Ltd., 1988-1992.
 * Copyright (C) Advanced RISC Machines Limited, 1990-1992.
 */

/*
 * RCS $Revision: 1.13 $
 * Checkin $Date: 1995/09/13 12:13:51 $
 * Revising $Author: amycroft $
 */

#ifndef _lex_h
#define _lex_h

#ifndef _defs_LOADED
#  include "defs.h"
#endif

typedef struct SymInfo {
    AEop sym;
    union { char *s; int32 i; Symstr *sv; FloatCon *fc; } a1;
    union { int32 len, flag; } a2;
    FileLine fl;
} SymInfo;

extern SymInfo curlex;          /* Current token and aux. info. */
#ifdef EXTENSION_VALOF
extern bool inside_valof_block;
#endif

extern AEop nextsym(void);
extern void ungetsym(void);     /* right inverse of nextsym */

extern int errs_on_this_sym;

extern AEop nextsym_for_hashif(void);

extern void lex_init(void);

extern void lex_beware_reinit(void);

extern void lex_reinit(void);

#ifdef CPLUSPLUS

/* for C++ or ANSI C; harmless here */
extern int lex_bodybegin(void);         /* start save template def      */
extern int lex_bodyend(void);           /* end   save template def      */
extern int lex_savebody(void);          /* save member fn def           */
extern int lex_saveexpr(void);
extern void lex_openbody(int h, bool dup);      /* re-read saved text   */
extern void lex_closebody(void);                /* end read + lose text */
/* for C++ ONLY; not used in ANSI C */
extern AEop lex_buffersym(void);
extern void lex_endbuffering(void);

#else

#define lex_bodybegin() 0
#define lex_bodyend() 0
#define lex_savebody() 0
#define lex_saveexpr() 0
#define lex_openbody(h,dup) 0
#define lex_closebody() 0

#endif

#endif

/* end of cfe/lex.h */
