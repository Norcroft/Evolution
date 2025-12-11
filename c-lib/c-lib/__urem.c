
/* Copyright (C) Codemist Ltd, 1988-1999 */

#include "__lib.h"

unsigned32 __urem(unsigned32 a1, unsigned32 a2)
{   (void) __udiv(a1, a2);
    return __remain;
}
