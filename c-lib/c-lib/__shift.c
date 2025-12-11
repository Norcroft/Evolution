
/* C4P run-time library.                  */
/* __shift.c: provide shifts by variable. */
/* Copyright (C) Codemist Ltd, 1988-1999. */

#include "__lib.h"

unsigned32 __shiftru(unsigned32 a1, int32 a2)
{   if (a2 & 16) a1 >>= 16;
    if (a2 & 8) a1 >>= 8;
    if (a2 & 4) a1 >>= 4;
    if (a2 & 2) a1 >>= 2;
    if (a2 & 1) a1 >>= 1;
    return a1;
}

int32 __shiftrs(int32 a1, int32 a2)
{   if (a2 & 16) a1 >>= 16;
    if (a2 & 8) a1 >>= 8;
    if (a2 & 4) a1 >>= 4;
    if (a2 & 2) a1 >>= 2;
    if (a2 & 1) a1 >>= 1;
    return a1;
}

int32 __shiftl(int32 a1, int32 a2)
{   if (a2 & 16) a1 <<= 16;
    if (a2 & 8) a1 <<= 8;
    if (a2 & 4) a1 <<= 4;
    if (a2 & 2) a1 <<= 2;
    if (a2 & 1) a1 <<= 1;
    return a1;
}
