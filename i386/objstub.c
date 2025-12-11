/*
 * objstub.c: a stub file to define objstream, obj_codewrite and so
 * on, for the i386 target which doesn't _have_ obj output as such
 * because it does everything through nasm.
 */

#ifndef __STDC__
#  include <strings.h>
#  define  SEEK_SET 0
#else
#  include <string.h>
#endif
#include <time.h>              /* see time() below */

#include "globals.h"    /* loads host.h,options.h,target.h,defaults.h   */
#include "mcdep.h"
/* #include "mcdpriv.h" */
#include "store.h"
#include "codebuf.h"
#include "builtin.h"
#include "xrefs.h"
#include "errors.h"
#include "sem.h"		/* experiment: for sizeoftype */
#include "version.h"		/* for CC_BANNER */

#ifdef foo

FILE *objstream;

CodeXref *codexrefs;

void obj_init() {
    				       /* stub */
}

void obj_header() {
				       /* stub */
}

void obj_trailer() {
    				       /* stub */
}

int32 obj_symref(Symstr *s, int flags, int32 loc) {
    return 0;			       /* utter rubbish, made-up */
}

void obj_codewrite(Symstr *name) {
    				       /* stub */
}
#else
void obj_header() {
				       /* stub */
}

void obj_trailer() {
    				       /* stub */
}

void obj_codewrite(Symstr *name) {
    				       /* stub */
}
#endif
