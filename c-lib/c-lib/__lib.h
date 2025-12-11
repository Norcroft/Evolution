
/* Copyright (C) Codemist Ltd, 1988-1999 */

#ifndef __sys_lib_h
#define __sys_lib_h 1

#include "hostsys.h"

#ifndef int32
#define int32 int
#define unsigned32 unsigned
#endif

extern unsigned32 __remain;

extern int32 __div(int32 a1, int32 a2);
extern unsigned32 __udiv(unsigned32 a1, unsigned32 a2);
extern int32 __rem(int32 a1, int32 a2);
extern unsigned32 __urem(unsigned32 a1, unsigned32 a2);
extern int32 __mul(int32 a1, int32 a2) ;
unsigned32 __shiftru(unsigned32 a1, int32 a2);
int32 __shiftrs(int32 a1, int32 a2);
int32 __shiftl(int32 a1, int32 a2);

#ifdef CMsim
#include <signal.h>
#define __ovfl() (raise(SIGFPE),0)
#else
extern int __ovfl(void);      /* floating point exception routine */
#endif

#endif
/* end of __lib.h */
