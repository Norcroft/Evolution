
/* Copyright (C) Codemist Ltd, 1988-1999 */

#include "__lib.h"

int32 __rem(int32 a1, int32 a2)
{   (void) __div(a1, a2);
    return (int32) __remain;
}
