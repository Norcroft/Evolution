/*
 * mip/cg.h
 * Copyright (C) Acorn Computers Ltd., 1988-1990.
 * Copyright (C) Codemist Ltd., 1987-1992.
 * Copyright (C) Advanced RISC Machines Limited, 1991-1992.
 */

/*
 * RCS $Revision: 1.13 $
 * Checkin $Date: 1995/09/18 14:59:23 $
 * Revising $Author: enevill $
 */

#ifndef _cg_h
#define _cg_h 1

#ifndef _defs_LOADED
#  include "defs.h"
#endif
#ifndef _cgdefs_LOADED
#  include "cgdefs.h"
#endif
#ifndef _jopcode_LOADED
#include "jopcode.h"
#endif

extern bool has_main;

extern J_OPCODE Q_swap(J_OPCODE);
/* alter a condition mask as required after interchanging the two operands
   of a comparison
 */

extern Binder *gentempvarofsort(RegSort sort);

extern Binder *gentempvarofsortwithname(RegSort sort, char *name);

#ifdef TARGET_IS_INTERPRETER
#define cg_topdecl(x, fl) 0
#define cg_reinit() 0
#define cg_tidy() 0
#else
extern void cg_topdecl(TopDecl *x, FileLine fl);
extern void cg_reinit(void);
extern void cg_tidy(void);
#endif

extern void cg_topdecl2(BindList *local_binders, BindList *regvar_binders);

extern void cg_init(void);

extern void cg_sub_reinit(void);

#endif

/* end of mip/cg.h */
