/*
 * C compiler file compiler.h
 * Copyright (C) Acorn Computers Ltd., 1988-1990.
 * Copyright (C) Advanced RISC Machines Limited, 1991-92.
 */

/*
 * RCS $Revision: 1.6 $
 * Checkin $Date: 1995/05/10 16:03:48 $
 * Revising $Author: lsmith $
 */

#ifndef __compiler_h
#define __compiler_h

/* The filename suffixes subject to inversion under RISC OS/MVS etc.  */
#ifdef FORTRAN
#  define FNAME_INCLUDE_SUFFIXES "f F h H"
#  define FNAME_SUFFIXES "a A f F h H o O s S"
#  define LANG_EXTN 'f'
#  define LANG_UC_EXTN 'F'
#  define LANG_EXTN_STRING "f"
#else
#ifdef PASCAL
#  define FNAME_INCLUDE_SUFFIXES "p P h H"
#  define FNAME_SUFFIXES "a A p P h H o O s S"
#  define LANG_EXTN 'p'
#  define LANG_UC_EXTN 'P'
#  define LANG_EXTN_STRING "p"
#else
#ifdef BCPL
#  define FNAME_INCLUDE_SUFFIXES "b B h H"
#  define FNAME_SUFFIXES "a A b B h H o O s S"
#  define LANG_EXTN 'b'
#  define LANG_UC_EXTN 'B'
#  define LANG_EXTN_STRING "b"
#else
#  define FNAME_INCLUDE_SUFFIXES "c C h H c++ C++ cpp CPP cp CP i I"
#  define FNAME_SUFFIXES "a A c C h H o O s S c++ C++ cpp CPP cp CP"
#  define LANG_EXTN 'c'
#  define LANG_UC_EXTN 'C'
#  define LANG_EXTN_STRING "c"
#endif
#endif
#endif

#include <time.h>    /* for time_t */

extern time_t tmuse_front, tmuse_back;

extern int ccom(int argc, char *argv[]);  /* must match spec for main */

extern void driver_abort(char *message);

extern bool cistreq(const char *s1, const char *s2);

#ifdef FOR_ACORN
extern bool cplusplus_preprocessing(void);
#endif

#endif
