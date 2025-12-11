
/* C4P run-time library.                  */
/* __fixup.c: condition code N := not C.  */
/* Copyright (C) Codemist Ltd, 1988-1999. */

#include "__lib.h"

void __fixup()
{   _word(0x60ff);      /* mov r0,-1 */
    _word(0xa001);      /* adc r0,0  */
}
void __callr0()
{   _word(0xf809);      /* mov pc,r0 */
}
