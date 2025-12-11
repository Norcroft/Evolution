
/* Copyright (C) Codemist Ltd, 1988-1999 */

#include "__lib.h"

unsigned32 __udiv(unsigned32 a1, unsigned32 a2)
/*
 *Unsigned divide of a2 by a1:returns quotient, leaves remainder in __remain
 */
{
    unsigned32 a3, a4, ip;

    if (a1 == 0) return __ovfl();
    a3 = a1;
    a4 = 0;
    ip = 0x80000000u;
    if (a2 < ip) ip = a2;
u_loop:
    if      (ip <= a3)        goto u_sh0mod8;
    else if (ip <= (a3 << 1)) goto u_sh1mod8;
    else if (ip <= (a3 << 2)) goto u_sh2mod8;
    else if (ip <= (a3 << 3)) goto u_sh3mod8;
    else if (ip <= (a3 << 4)) goto u_sh4mod8;
    else if (ip <= (a3 << 5)) goto u_sh5mod8;
    else if (ip <= (a3 << 6)) goto u_sh6mod8;
    else if (ip >  (a3 << 7))
    {   a3 = a3 << 8;
        goto u_loop;
    }
u_loop2:
    if (a2 >= (a3 << 7)) a2 -= (a3 << 7), a4++;
    a4 = a4 << 1;
u_sh6mod8:
    if (a2 >= (a3 << 6)) a2 -= (a3 << 6), a4++;
    a4 = a4 << 1;
u_sh5mod8:
    if (a2 >= (a3 << 5)) a2 -= (a3 << 5), a4++;
    a4 = a4 << 1;
u_sh4mod8:
    if (a2 >= (a3 << 4)) a2 -= (a3 << 4), a4++;
    a4 = a4 << 1;
u_sh3mod8:
    if (a2 >= (a3 << 3)) a2 -= (a3 << 3), a4++;
    a4 = a4 << 1;
u_sh2mod8:
    if (a2 >= (a3 << 2)) a2 -= (a3 << 2), a4++;
    a4 = a4 << 1;
u_sh1mod8:
    if (a2 >= (a3 << 1)) a2 -= (a3 << 1), a4++;
    a4 = a4 << 1;
u_sh0mod8:
    if (a2 >= a3) a2 -= a3, a4++;
    if (a1 <= a3 >> 1)
    {   a3 = a3 >> 8;
        a4 = a4 << 1;
        goto u_loop2;
    }
    __remain = a2;
    return a4;
}
