#include "lib-base.c"       /* needs to be first */

#include "simsys.c"
#include "stdio.c"

#include "alloc.c"
#include "ctype.c"
#include "error.c"
#include "getenv.c"
#include "locale.c"
#include "raise.c"
#include "remove.c"
#include "sbrk.c"
#include "signal.c"
#include "stdlib.c"
#include "string.c"
#include "system.c"

#ifndef NO_FLOATING_LIBRARY
#include "sort.c"            /* because not often used */
#include "time.c"            /* because of (double)    */
#include "fpprintf.c"
#include "scanf.c"
#include "math.c"
#include "softfp.c"
#endif

#include "printf.c"          /* after fpprintf.c */

#include "__mul.c"
#include "__div.c"
#include "__rem.c"
#include "__udiv.c"
#include "__urem.c"
#include "__remain.c"
#include "__shift.c"
#include "__fixup.c"
