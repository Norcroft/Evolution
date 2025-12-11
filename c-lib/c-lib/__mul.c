
/* C4P run-time library.                  */
/* __mul.c: multiply 32x32 -> 32.         */
/* Copyright (C) Codemist Ltd, 1988-1999. */

#include "__lib.h"

int32 __mul(int32 a1, int32 a2)
{
    _word(0xdc05);      /*  push    r4          */
    _word(0xdd05);      /*  push    r5          */
    _word(0xdc27);      /*  mov     r4,r1       */
    _word(0xdd47);      /*  mov     r5,r2       */
    _word(0x6800);      /*  movi.l  r0,32768    */
      _word(0x8000);    /*    (ditto)           */
    _word(0x9400);      /*  add     r4,r0       */
    _word(0xbcee);      /*  sra     r4,8        */
    _word(0xbcee);      /*  sra     r4,8        */
    _word(0xd000);      /*  mul     ; (ms*ls)   */
    _word(0xdc27);      /*  mov     r4,r1       */
    _word(0xd9a7);      /*  mov     r1,r5       */
    _word(0x9500);      /*  add     r5,r0       */
    _word(0xbdee);      /*  sra     r5,8        */
    _word(0xbdee);      /*  sra     r5,8        */
    _word(0xd100);      /*  macc    ; (ls*ms)   */
    _word(0xbae8);      /*  sla     r2,8        */
    _word(0xbae8);      /*  sla     r2,8        */
    _word(0xdd27);      /*  mov     r5,r1       */
    _word(0xd100);      /*  macc    ; (ls*ls)   */
    _word(0xd947);      /*  mov     r1,r2       */
    _word(0xdd25);      /*  pop     r5          */
    _word(0xdc25);      /*  pop     r4          */
    return a1;          /*  ret                 */
}

#ifdef NEVER
/* Algorithm: The 'MUL' instruction is encoded by the call to           */
/*            int32 __smulx2(int a1, int a2);                           */

int32 __mul(int32 a1, int32 a2)
{   int32 rm1 = __smulx2((int)a1, (int)(a2+0x8000 >> 16));
    int32 rm2 = __smulx2((int)(a1+0x8000 >> 16), (int)a2);
    int32 rl = __smulx2((int)a1, (int)a2);
    return ((rm1 + rm2) << 16) + rl;
}
#endif

