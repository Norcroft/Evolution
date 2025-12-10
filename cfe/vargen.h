/*
 * cfe/vargen.h:
 * Copyright (C) Acorn Computers Ltd., 1988
 * Copyright (C) Codemist Ltd., 1988.
 */

/*
 * RCS $Revision: 1.15 $ Codemist 4
 * Checkin $Date: 1995/09/18 14:46:49 $
 * Revising $Author: enevill $
 */

#ifndef _vargen_h
#define _vargen_h

#ifndef _defs_LOADED
#  include "defs.h"
#endif

/*
 * ****** NASTY EXPORT - RECONSIDER ******
 * Should be static except for initstaticvar(datasegment) in compiler.c
 */
extern void initstaticvar(Binder *b, bool topflag);

/* The following routine generates statics, which MUST have been instated
   with instate_declaration().  Dynamic initialistions are turned into
   assignments for rd_block() by returning the expression tree; NULL means
   no dynamic initialization. Top-level dynamic initialization code (for C++)
   is also generated in the module initialization function.

   Ensure type errors are noticed here (for line numbers etc.)
*/
extern Expr *genstaticparts(DeclRhsList * const d, bool topflag,
        bool dummy_call, Expr *dyninit);
/* @@@ since the 'const' isn't part of the function type in the line    */
/* @@@ above, AM wonders why it has been added.  C++ namemunge oddity!  */

#if defined CPLUSPLUS && !defined TARGET_IS_INTERPRETER

extern void vg_note_vtable(TagBinder *cl, int32 sz, Symstr *name);
extern Binder *generate_wrapper(Binder *a);
extern TopDecl *vg_dynamic_init(void);
extern void vg_ref_dynamic_init(void);
extern void vargen_init(void);
extern int32 ddtor_vecsize(void);
#else

#ifdef TARGET_IS_INTERPRETER
#define generate_wrapper(a) ((a),0)
#define vg_generate_deferred_const(a) ((void)(a))
#endif
#define vg_note_vtable(cl, sz, name) 0
#define vg_dynamic_init() 0
#define vg_ref_dynamic_init() 0
#define vargen_init() 0
#define vg_currentdecl_inits 0
#define ddtor_vecsize() 0
#endif

#endif

/* end of cfe/vargen.h */
