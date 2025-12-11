
/* Copyright (C) Codemist Ltd, 1988-1999 */

#include "__lib.h"

int32 __div(int32 a1, int32 a2)
/*
 * Divide a2 by a1 and return quotient.  Leave remainder in __remain.
 * Note, by the way, that the variable "__remain" is of type unsigned int,
 * even though my remainder is a signed quantity here.. but I do not
 * consider it worth having a second signed variable to hand back
 * a signed remainder, especially since I probably expect that the
 * said result will be handed back in a register rather than in memory.
 * The quotient is truncated towards zero (and I have signed values).
 * Code is similar to that for unsigned division, but has to fiddle
 * with signs, and can be very slightly quicker on the pre-normalization
 * since signed values are 1 bit shorter.
 */
{
    int32 a4, ip;
    unsigned32 a3, a2u;
    ip = (a2 & 0x80000000) | ((unsigned32)(a1 ^ a2) >> 1);
/*
 * ip bit 31  sign of dividend (= sign of remainder)
 *    bit 30  sign of dividend EOR sign of divisor (= sign of quotient)
 * these are packed into one register with a view to avoiding the
 * need for more work registers and the potential for extra register
 * save overhead.
 */
    if (a2 < 0) a2 = -a2;
    if (a1 < 0) a1 = -a1;
    if (a1 == 0) return __ovfl();
    a2u = a2;
    a3 = a1;
    a4 = 0;
s_loop:
    if      (a2u <= a3)        goto s_sh0mod8;
    else if (a2u <= (a3 << 1)) goto s_sh1mod8;
    else if (a2u <= (a3 << 2)) goto s_sh2mod8;
    else if (a2u <= (a3 << 3)) goto s_sh3mod8;
    else if (a2u <= (a3 << 4)) goto s_sh4mod8;
    else if (a2u <= (a3 << 5)) goto s_sh5mod8;
    else if (a2u <= (a3 << 6)) goto s_sh6mod8;
    else if (a2u >  (a3 << 7))
    {   a3 = a3 << 8;
        goto s_loop;
    }

s_loop2:
    if (a2u >= (a3 << 7)) a2u -= (a3 << 7), a4++;
    a4 = a4 << 1;
s_sh6mod8:
    if (a2u >= (a3 << 6)) a2u -= (a3 << 6), a4++;
    a4 = a4 << 1;
s_sh5mod8:
    if (a2u >= (a3 << 5)) a2u -= (a3 << 5), a4++;
    a4 = a4 << 1;
s_sh4mod8:
    if (a2u >= (a3 << 4)) a2u -= (a3 << 4), a4++;
    a4 = a4 << 1;
s_sh3mod8:
    if (a2u >= (a3 << 3)) a2u -= (a3 << 3), a4++;
    a4 = a4 << 1;
s_sh2mod8:
    if (a2u >= (a3 << 2)) a2u -= (a3 << 2), a4++;
    a4 = a4 << 1;
s_sh1mod8:
    if (a2u >= (a3 << 1)) a2u -= (a3 << 1), a4++;
    a4 = a4 << 1;
s_sh0mod8:
    if (a2u >= a3) a2u -= a3, a4++;
    if ((unsigned32)a1 <= (a3 >> 1))
    {   a3 = a3 >> 8;
        a4 = a4 << 1;
        goto s_loop2;
    }

    if ((ip & 0x40000000) != 0) a4 = -a4;
    if (ip < 0) a2u = -a2u;
    __remain = a2u;
    return a4;
}
