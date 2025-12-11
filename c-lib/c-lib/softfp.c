/* softfp.c: Software implementation of floating point arithmetic.       */
/* Copyright (C) Codemist Ltd., 1987-1997.                               */

/*
 * Compiling this code is expected to lead to lots of warning messages
 * about argument passing type error. This is because I have to deal
 * with the interface between FP values and representations.
 * Either ignore these errors or compile with option -zpe1
 * These go away when compiled as separate routines.

 * This code assumes various NASTY things about the way in which FP
 * arguments & results are processed.  It is ONLY for use with the
 * SOFTWARE_FLOATING_POINT option in the Norcroft C compiler.
 * Note that it must itself be compiled with SOFTWARE_FLOATING_POINT
 * enabled so that 'float' and 'int' (or 'long') have the same internal
 * representation (32 bits in an int register).
 * Little point: 'float' args are written 'int32' so arg. widening
 * (to double) does not take place.  Is this widening sensible on XAP?
 * The XAP now has 'TARGET_PASS_FLOAT_AS_FLOAT, so does not do this.
 *
 * If __sizeof_double==4 then sizeof(double)=4 and the compiler
 * should call __fadd etc rather than __dadd. The 8-byte float
 * support remains here in case it is wanted for long double.
 */

#include <signal.h>

#ifdef __xap
#define int32 long
#define unsigned32 unsigned long
#define EXTENSION_FRAC 1
#else
#define int32 int
#define unsigned32 unsigned
#endif

#define bool int
#define signed32_rightshift_(a,n) ((int32)(a) >> (n))

/*
 * This code NORMALLY uses a floating point representation based on
 * the IEEE standard, but note well that I do not implement arithmetic
 * on infinities or denormalised numbers and I do not support the
 * concept on a NaN. Complete and proper support for all those (and the
 * various directed rounding options) would make this code rather
 * larger and slower, and anybody who needs that should obtain specialist
 * help and should create a definitive floating point emulator for their
 * particular computer architecture.
 *
 * If IBMFLOAT is defined I will use the old IBM mainframe layout, but
 * this is not expected to be very useful these days!
 */

#ifndef IBMFLOAT
#  define IEEE 1
#endif

/*
 * The magic rules about when and how to round are implemented here.
 * g is guard word representing the 32-bits of precision just below
 * the value w (which is the lowest word of the value being rounded).
 * If g = 0.5 I round only if w is odd. If g > 0.5 I round always.
 */

#define _needcarry(g, w) ((g & 0x80000000) && ((w & 1)!=0 || g!=0x80000000))

#ifdef IBMFLOAT
/*
 * I have only implemented the faster 32-bit float code in IEEE-like
 * mode.
 */
#define OLD_SLOW_CODE_BUT_A_BIT_SHORTER 1
#endif

#ifdef OLD_SLOW_CODE_BUT_A_BIT_SHORTER

/* I perform single precision arithmetic by widening to double,          */
/* performing the operation and narrowing back.                          */
/* This is slow but easy to code!                                        */

float __fadd(int32 a, int32 b)
{
    return (float)(__f2d(a) + __f2d(b));
}

float __fsub(int32 a, int32 b)
{
    return (float)(__f2d(a) - __f2d(b));
}

float __fmul(int32 a, int32 b)
{
    return (float)(__f2d(a) * __f2d(b));
}

float __fdiv(int32 a, int32 b)
{
    return (float)(__f2d(a) / __f2d(b));
}

int32 __fneg(int32 a)       /* type is really float, but here who cares!     */
{
    return a==0 ? 0 : a ^ 0x80000000;
}


bool __fgr(int32 a, int32 b)
{
    return __f2d(a) > __f2d(b);
}

bool __fgeq(int32 a, int32 b)
{
    return __f2d(b) >= __f2d(a);
}

bool __fls(int32 a, int32 b)
{
    return __f2d(b) < __f2d(a);
}

bool __fleq(int32 a, int32 b)
{
    return __f2d(a) <= __f2d(b);
}

bool __feq(int32 a, int32 b)
{
    return __f2d(a) == __f2d(b);
}

bool __fneq(int32 a, int32 b)
{
    return __f2d(a) != __f2d(b);
}

float __fflt(int32 a)
{
    return (float)__dflt(a);
}

float __ffltu(unsigned32 a)
{
    return (float)__dfltu(a);
}

int32 __ffix(int32 a)
{
    return (int32)__f2d(a);
}

unsigned32 __ffixu(int32 a)
{
    return (unsigned32)__f2d(a);
}

#else /* OLD_SLOW_CODE_BUT_A_BIT_SHORTER */

bool fflt_sum(unsigned32 *a, unsigned32 b, unsigned32 c)
{
    unsigned32 guard=0;
    int bx = (int)(b >> 23) & 0xff;
    int cx = (int)(c >> 23) & 0xff;
    int shift = bx - cx;
    if (shift < -24 || bx == 0)
    {   *a = c & 0x7fffffff;
        return 1;
    }
    if (shift > 24 || cx == 0)
    {   *a = b & 0x7fffffff;
        return 1;
    }
    b = (b & 0x7fffff) | 0x800000;
    c = (c & 0x7fffff) | 0x800000;
/* Now I need to align the operands */
    if (shift > 0)
    {   guard = c << (32 - shift);
        c = c >> shift;
    }
    else if (shift < 0)
    {   shift = -shift;
        guard = b << (32 - shift);
        b = b >> shift;
        bx = cx;
    }
    else guard = 0;
/*
 * Now b and c are similarly aligned, and bx holds the exponent
 * that I want to end up with. If any shifting was done then guard
 * stores the bits that were shifted away, with the top such bit
 * in the 0x80000000 position.
 */
    b += c;
    if (b & 0x01000000)
    {   guard = (b << 31) | (guard >> 1);
        b = b >> 1;
        bx += 1;
        if (bx >= 0xff) return 0;          /* Overflow case             */
    }
    if (_needcarry(guard, b))
    {   b++;
        if (b & 0x01000000)
        {   b = b >> 1;
            bx += 1;
            if (bx >= 0xff) return 0;          /* Overflow case             */
        }
    }
    *a = (b & ~0x00800000) | ((int32)bx << 23);
    return 1;
}

bool fflt_difference(unsigned32 *a, unsigned32 b, unsigned32 c)
{
    unsigned32 bg, resultsign;
    int bx = (int)(b >> 23) & 0xff;
    int cx = (int)(c >> 23) & 0xff;
    int shift = bx - cx;
    if (shift < -24 || bx == 0)
    {   *a = c | 0x80000000;
        return 1;
    }
    if (shift > 24 || cx == 0)
    {   *a = b & 0x7fffffff;
        return 1;
    }
    b = (b & 0x7fffff) | 0x800000;
    c = (c & 0x7fffff) | 0x800000;
/* Now I need to align the operands                                      */
    if (shift > 0)
    {   bg = -(c << (32 - shift));
        if (bg != 0) b--;
        c = c >> shift;
    }
    else if (shift < 0)
    {   shift = -shift;
        bg = b << (32 - shift);
        b = b >> shift;
        bx = cx;
    }
    else bg = 0;
/*
 * In this case I have guard information for each of b and c. Do a
 * double length subtraction (b,bg) - (c,cg) given that at most one
 * of bg, cg can be non-zero. Well actually I have put all the guard
 * information into bg by now...
 */
    b = b - c;
/*
 * Subtraction complete in 2s complement form. I now argue that if b ends up
 * negative then I must have had the (shifted) b < c, and so since both b and
 * c were normalised to start with c can NOT have been shifted right. So if
 * anything was it was b. So I can negate the doubleword value (b, bg) to
 * get the absolute value of my result ready, and record that the result
 * will be negative.
 */
    if (b & 0x80000000)
    {   if (bg == 0) b = -b;
        else
        {   bg = -bg;
            b = ~b;
        }
        resultsign = 0x80000000;
    }
    else resultsign = 0;
/* Subtraction now complete in sign & magnitude form */
    if (b == 0 && bg == 0)
    {   *a = 0;                    /* Result is absolutely zero          */
        return 1;
    }
/* Must have a normalized result.                                        */
    while ((b & 0x00f80000) == 0)
    {   b = (b << 5) | (bg >> 27);
        bg = bg << 5;
        bx -= 5;
    }
    while ((b & 0x00800000) == 0)
    {   b = (b << 1) | (bg >> 31); /* NB bg is unsigned in this shift */
        bg = bg << 1;
        bx -= 1;
    }
    if (bx <= 0)
    {   *a = resultsign;
        return 1;
    }
    if (_needcarry(bg, b)) b++;
    *a = (b & ~0x00800000) | ((int32)bx << 23) | resultsign;
    return 1;
}

int32 __fadd(unsigned32 b, unsigned32 c)
{
    bool ok;
    unsigned32 a;
/* Do a case analysis on signs to end up with arithmetic on unsigned floats */
    if (b & 0x80000000)
    {   if (c & 0x80000000)
        {   ok = fflt_sum(&a, b, c);
            a ^= 0x80000000;
        }
        else ok = fflt_difference(&a, c, b);
    }
    else if (c & 0x80000000)
        ok = fflt_difference(&a, b, c);
    else ok = fflt_sum(&a, b, c);
    if (!ok) raise(SIGFPE);
    return a;
}

int32 __fsub(unsigned32 b, unsigned32 c)
{
    bool ok;
    unsigned32 a;
    if (b & 0x80000000)
    {   if (c & 0x80000000)
            ok = fflt_difference(&a, c, b);
        else
        {   ok = fflt_sum(&a, b, c);
            a ^= 0x80000000;
        }
    }
    else if (c & 0x80000000)
        ok = fflt_sum(&a, b, c);
    else ok = fflt_difference(&a, b, c);
    if (!ok) raise(SIGFPE);
    return a;
}

int32 __fmul(unsigned32 b, unsigned32 c)
{
    unsigned32 a0, a1, a2, b0, b1, c0, c1;
    int32 as = (b ^ c) & 0x80000000;    /* sign for result */
    int ax;
    int bx = (int)(b >> 23) & 0xff;
    int cx = (int)(c >> 23) & 0xff;
    if (bx==0 || cx==0)
    {   /* multiplication by 0.0         */
        /* note treatment of sign.       */
        return as;
    }
    b = (b & 0x7fffff) | 0x800000;
    c = (c & 0x7fffff) | 0x800000;
/*
 * Here b and c are 24-bit numbers. I split them into 12 bit chunks and form
 * a 48-bit product so that I can round carefully.
 */
    b0 = b & 0xfff;
    b1 = b >> 12;
    c0 = c & 0xfff;
    c1 = c >> 12;
    a0 = b0*c0;
    a1 = b0*c1 + c0*b1;
    a2 = b1*c1;
    a1 += (a0 >> 12); a0 &= 0xfff;
    a2 += (a1 >> 12); a1 &= 0xfff;
    a1 = (a1 << 12) | a0;
    a1 = a1 << 8;  /* Position at the top of the word */
/* Now (a2,a1) is the 48-bit product */
    ax = bx + cx - 0x7e;
    if ((a2 & 0x00800000) == 0)
    {   a2 = (a2 << 1) | ((a1 >> 31) & 1);
        a1 = a1 << 1;
        ax--;
    }
    if (_needcarry(a1, a2)) 
    {   a2++;
        if (a2 & 0x01000000)
        {   a2 = a2 >> 1;
            ax++;
        }
    }
    if (ax >= 0xff) raise(SIGFPE);      /* Overflow */
    else if (ax <= 0)
    {   /* N.B. keep sign on underflow     */
        return as;
    }
    return (a2 & 0x007fffff) | (ax << 23) | as;
}

int32 __fdiv(unsigned32 b, unsigned32 c)
{
    unsigned32 a, as = (b ^ c) & 0x80000000;
    int i;
    int ax;
    int bx = (int)(b >> 23) & 0xff;
    int cx = (int)(c >> 23) & 0xff;
    if (cx == 0) raise(SIGFPE);         /* division by zero              */
    if (bx == 0)
    {   /*  0.0 / anything  = 0.0        */
        return as;
    }
    b = (b & 0x7fffff) | 0x00800000;
    c = (c & 0x7fffff) | 0x00800000;
    ax = bx - cx + 0x7e;
    a = 0;
/*
 * Do the division by test-and-subtract. I compute a 26-bit quotient
 * which will SOMETIMES have its top bit zero. Thus I can be certain
 * of getting 25 good bits. The first 24 will be part of the answer. The
 * next will be the most significant bit of the guard word (used for rounding)
 * and the remainder then just needs to know if it is zero or not.
 */
    for (i = 0; i<26; i++)
    {   if (b >= c)
        {   b = b - c;
            a = (a << 1) | 1;
        }
        else a = a << 1;
        b = b << 1;
    }
    if (a & 0x02000000)    /* Shift to get 25 good bits */
    {   b |= (a & 1);      /* Only care here about zero vs non-zero */
        a = a >> 1;
        ax++;
    }
    b |= (a << 31);        /* guard word with 0.5 bit in top position */
    a = a >> 1;
    if (_needcarry(b, a))
    {   a++;
        if (a & 0x01000000)
        {   a = a >> 1;
            ax++;
        }
    }
    if (ax >= 0xff) raise(SIGFPE); /* Overflow on the division          */
    else if (ax <= 0)
    {   /* N.B. keep sign on underflow     */
        return as;
    }
    return (a & 0x007fffff) | (ax << 23) | as;
}

int32 __fneg(int32 b)
{
    return b ^ 0x80000000;
}


bool __fgr(int32 b, int32 c)
{
/* +0.0 is equal to -0.0                                                 */
    if ((b & 0x7f000000)==0 && (c & 0x7f000000)==0) return 0;
    if (b < 0 && c >= 0) return 0;
    else if (b >= 0 && c < 0) return 1;
    else if (b < 0)
    {   int32 temp = b;
        b = c & 0x7fffffff;
        c = temp & 0x7fffffff;
    }
    if (b > c) return 1;
    else return 0;
}

bool __fgeq(int32 a, int32 b)
{
    return !__fgr(b, a);
}

bool __fls(int32 a, int32 b)
{
    return __fgr(b, a);
}

bool __fleq(int32 a, int32 b)
{
    return !__fgr(a, b);
}

bool __feq(unsigned32 b, unsigned32 c)
{
    if (b == c) return 1;
/* I ensure that +0 == -0 here.                                          */
    if (b != 0 && b != 0x80000000) return 0;
    if (c != 0 && c != 0x80000000) return 0;
    else return 1;
}

bool __fneq(unsigned32 a, unsigned32 b)
{
    return !__feq(a, b);
}


int32 __fflt(int32 n)
{
    unsigned32 a;
    int32 as;
    int ax;
    if (n==0) return 0;
    else if (n>0) as = 0;
    else
    {   as = 0x80000000;
        n = -n;             /* from here on n is thought of as unsigned  */
    }
    a = n;
    ax = 0x7e + 24;
    if ((a & 0xff000000) != 0)
    {   unsigned al = 0;
        while ((a & 0xff000000) != 0)
        {   al = (al >> 1) | (a << 31);
            a = a >> 1;
            ax++;
        }
        if (_needcarry(al, a))
        {   a++;
            if (a & 0x01000000)
            {   a = a >> 1;
                ax++;
            }
        }
    }
    else while ((a & 0x00800000)==0)
    {   a = a << 1;
        ax -= 1;
    }
    return (a & ~0x00800000) | (ax << 23) | as;
}

int32 __ffltu(unsigned32 n)
{
    unsigned32 a;
    int ax;
    if (n==0) return 0;
    a = n;
    ax = 0x7e + 24;
    if ((a & 0xff000000) != 0)
    {   unsigned al = 0;
        while ((a & 0xff000000) != 0)
        {   al = (al >> 1) | (a << 31);
            a = a >> 1;
            ax++;
        }
        if (_needcarry(al, a))
        {   a++;
            if (a & 0x01000000)
            {   a = a >> 1;
                ax++;
            }
        }
    }
    else while ((a & 0x008000000)==0)
    {   a = a << 1;
        ax -= 1;
    }
    return (a & ~0x00800000) | (ax << 23);
}

int32 __ffix(unsigned32 a)
{
    int sign, ax;
    if (a & 0x80000000)
    {   a &= ~0x80000000;
        sign = 1;
    }
    else sign = 0;
    ax = (int)(a >> 23) - 0x7e - 24;
    a = (a & 0x007fffff) | 0x00800000;
    if (ax < 0)
    {   ax = -ax;
        if (ax >= 24) return 0;
        a = a >> ax;
    }
    else if (ax >= 8)
    {   if (sign && ax == 8 && a == 0x00800000) a = 0x80000000;
        else return raise(SIGFPE); /* overflow */
    }
    else a = a << ax;
    if (sign) a = -a;
    return a;
}

unsigned32 __ffixu(unsigned32 a)
{
    int ax = (int)(a >> 23) - 0x7e - 24;
    if (a & 0x80000000)
    {   if (a == 0x80000000) return 0;
        else return raise(SIGFPE);     /* negative input */
    }
    a = (a & 0x007fffff) | 0x00800000;
    if (ax < 0)
    {   ax = -ax;
        if (ax >= 24) return 0;
        a = a >> ax;
    }
    else if (ax > 8) return raise(SIGFPE); /* overflow */
    else a = a << ax;
    return a;
}


#endif /* OLD_SLOW_CODE_BUT_A_BIT_SHORTER */

#ifdef EXTENSION_FRAC
/*
 * I will leave the fraction extension using the slow scheme at
 * least for now.
 */
float __ffltr(int32 a) { return (float)(_dfltr(a)); }
int32 __ffixr(int32 a) { return __dfixr(__f2d(a)); }
#endif

/* Now the functions that deal with double precision args. They can have */
/* funny types for their formals now since all references to them have   */
/* already been processed under the shelter of proper extern             */
/* declarations that tell the official story re types.                   */

/*
 * Note a big oddity here. I use "long double". This is because I really
 * wany a 64-bit float, and with some options the compiler would make
 * a plain "double" just 32-bits.
 */

typedef union dblrep { long double d; unsigned32 ua[2]; } dblrep;
#define i2dbl_(a,u0,u1) (a.ua[0] = u0, a.ua[1] = u1, a.d)


#ifdef IBMFLOAT
/*
 * For the IBM representation conversions between single and double
 * floats are VERY easy - I just discard a word or pad with zero bits.
 * IEEE makes it harder since the layout of bits in the words has to be
 * changed.
 */

int32 __d2f(int32 a, int32 b)   /* value is really a float, arg a double     */
{
    return a;
}

long double __f2d(int32 a)
{   dblrep r;
    r.ua[0] = a;
    r.ua[1] = 0;
    return r.d;
}

#else /* IBMFLOAT */


int32 __d2f(int32 hi, int32 lo)
{
    int x = (int)(hi>>20) & 0x7ff;
    int32 m = hi & 0x000fffff;
    if (x==0)                   /* value is zero - treat specially       */
        return 0;               /* OK */
    else if (x==0x7ff)          /* value is 'infinity'.                  */
        return raise(SIGFPE);
    m = (m << 3) | ((lo >> 29) & 0x7);
    lo = lo << 3;
    if (lo < 0 && (lo!=0x80000000 || (m & 1)!=0))
        if ((m += 1) == 0x00800000)     /* round up & renormalize        */
        {   m = 0;
            x += 1;
        }
    x = x - 0x3ff + 0x7f;
    if (x >= 0xff) return raise(SIGFPE);
    else if (x <= 0)
        return (hi & 0x80000000);       /* underflow: preserve sign     */
    return (hi & 0x80000000) | ((int32)x << 23) | m;
}

long double __f2d(int32 e0)
{   dblrep r;
    int x = (int)(e0>>23) & 0xff;
    int32 s = e0 & 0x80000000;
    if (x==0)                   /* value is zero - preserve sign, why?   */
        r.ua[0] = s, r.ua[1] = 0;
    else if (x==0xff)           /* value is 'infinity' - preserve sign.  */
        r.ua[0] = s | 0x7ff00000, r.ua[1] = 0;
    else
        x = x - 0x7f + 0x3ff,
        r.ua[0] = s | ((int32)x << 20) | ((e0 & 0x7fffff) >> 3),
        r.ua[1] = e0 << 29;
    return r.d;
}

#endif /* IBMFLOAT */

/* Coded without concern about speed, and so quite dreadfully slow.      */
/* Also coded without concern about code density, so rather repetitive   */
/* and bulky. Memory is supposed to be cheap these days.                 */
/* N.B. flt_sum and flt_difference logically take unsigned flt operands  */
/* and thus must IGNORE their top bit.                                   */

#ifdef IBMFLOAT

bool flt_sum(unsigned32 *a, unsigned32 bh, unsigned32 bl,
                             unsigned32 ch, unsigned32 cl)
{
    unsigned32 w;
    int bx = (int)(bh >> 24) & 0x7f;
    int cx = (int)(ch >> 24) & 0x7f;
    int shift = bx - cx;
    if (shift < -(56/4))
    {   a[0] = ch & 0x7fffffff;
        a[1] = cl;
        return 1;
    }
    if (shift > (56/4))
    {   a[0] = bh & 0x7fffffff;
        a[1] = bl;
        return 1;
    }
    bh = bh & 0xffffff;
    ch = ch & 0xffffff;
/* Now I need to align the operands                                      */
    if (shift > 0)
    {   int rshift;
        if (shift > 8)
        {   cl = ch;
            ch = 0;
            shift -= 8;
        }
        rshift = 8 - shift;
        shift *= 4;
        rshift *= 4;
        cl = (ch << rshift) | (cl >> shift);
        ch = ch >> shift;
    }
    else if (shift < 0)
    {   int rshift;
        if (shift < -8)
        {   bl = bh;
            bh = 0;
            shift += 8;
        }
        shift = -shift;
        rshift = 8 - shift;
        shift *= 4;
        rshift *= 4;
        bl = (bh << rshift) | (bl >> shift);
        bh = bh >> shift;
        bx = cx;
    }
/* Now I just need to do a two-word addition                             */
    w = (bl & 0xff) + (cl & 0xff);
    bl = (bl >> 8) + (cl >> 8) + (w >> 8);
    bh += ch + (bl >> 24);
    bl = (bl << 8) + (w & 0xff);
    if (bh & 0xff000000)
/* Maybe I need to renormalize?                                          */
    {   bl = (bh << 28) | (bl >> 4);
        bh = bh >> 4;
        bx += 1;
        if (bx > 0x7f) return 0;            /* Overflow case             */
    }
/* I truncated rather than rounded.                                      */
    a[0] = bh | ((int32)bx << 24);
    a[1] = bl;
    return 1;
}

bool flt_difference(unsigned32 *a, unsigned32 bh, unsigned32 bl,
                                    unsigned32 ch, unsigned32 cl)
{
    unsigned32 bg=0, cg=0, w, resultsign;
    int bx = (int)(bh >> 24) & 0x7f;
    int cx = (int)(ch >> 24) & 0x7f;
    int shift = bx - cx;
    if (shift < -(56/4) || (bh == 0 && bl == 0))
    {   a[0] = ch | 0x80000000;
        a[1] = cl;
        return 1;
    }
    if (shift > (56/4) || (ch == 0 && cl == 0))
    {   a[0] = bh & 0x7fffffff;
        a[1] = bl;
        return 1;
    }
    bh = bh & 0xffffff;
    ch = ch & 0xffffff;
/* Now I need to align the operands                                      */
    if (shift > 0)
    {   int rshift;
        if (shift > 8)
        {   cg = cl;
            cl = ch;
            ch = 0;
            shift -= 8;
        }
        rshift = 8 - shift;
        shift *= 4;
        rshift *= 4;
        if (cg == 0) cg = cl << rshift;
        else cg = (cl << rshift) | 1;
        cl = (ch << rshift) | (cl >> shift);
        ch = ch >> shift;
        bg = 0;
    }
    else if (shift < 0)
    {   int rshift;
        if (shift < -8)
        {   bg = bl;
            bl = bh;
            bh = 0;
            shift += 8;
        }
        shift = -shift;
        rshift = 8 - shift;
        shift *= 4;
        rshift *= 4;
        if (bg == 0) bg = bl << rshift;
        else bg = (bl << rshift) | 1;
        bl = (bh << rshift) | (bl >> shift);
        bh = bh >> shift;
        cg = 0;
        bx = cx;
    }
    else bg = cg = 0;
/* Now for a subtraction, taking account of signs. Ugh.                  */
/* I rely on right shifts on signed types being arithmetic in struggles  */
/* to implement multiple precision arithmetic without a proper add-carry */
/* operator in my language.                                              */
    w = (bg & 0xff) - (cg & 0xff);
    bg = (bg >> 8) - (cg >> 8) + signed32_rightshift_(w, 8);
    cg = signed32_rightshift_(bg, 24);
    bg = (bg << 8) | (w & 0xff);
    w = (bl & 0xff) - (cl & 0xff) + cg;
    bl = (bl >> 8) - (cl >> 8) + signed32_rightshift_(w, 8);
    cg = signed32_rightshift_(bl, 24);
    bl = (bl << 8) | (w & 0xff);
    bh = bh - ch + cg;
/* Subtraction complete in 2s complement form.                           */
    if (bh & 0x80000000)  /* Sign of result must be negative             */
    {   if (bg == 0)
        {   if (bl == 0) bh = -bh;
            else
            {   bl = -bl;
                bh = ~bh;
            }
        }
        else
        {   bg = -bg;
            bl = ~bl;
            bh = ~bh;
        }
        resultsign = 0x80000000;
    }
    else resultsign = 0;
/* Subtraction now complete in sign & magnitude form                     */
    if (bh == 0 && bl == 0 && bg == 0)
    {   a[0] = a[1] = 0;           /* Result is absolutely zero          */
        return 1;
    }
/* Must have a normalized result.                                        */
    if ((bh & 0x00f00000)==0)       /* need to renormalize?              */
    {   while (bh == 0)
        {   bh = bl >> 8;
            bl = (bl << 24) | (bg >> 8);
            bg = bg << 24;
            bx -= 6;
            if (bx < 0)
            {   a[0] = a[1] = 0;
                return 1;
            }
        }
        while ((bh & 0x00f00000) == 0)
        {   bh = (bh << 4) | (bl >> 28);
            bl = (bl << 4) | (bg >> 28);
            bg = bg << 4;
            bx -= 1;
            if (bx < 0)
            {   a[0] = a[1] = 0;
                return 1;
            }
        }
    }
    else if (bh & 0x0f000000)
    {/* bg = (bl << 28) | (bg >> 4); */ /* guard word not needed any more */
        bl = (bh << 28) | (bl >> 4);
        bh = bh >> 4;
        bx += 1;
        if (bx > 0x7f) return 0;        /* Overflow case                 */
    }
    a[0] = bh | ((int32)bx << 24) | resultsign;
    a[1] = bl;
    return 1;
}

#else /* IBMFLOAT */

bool flt_sum(unsigned32 *a, unsigned32 bh, unsigned32 bl,
                             unsigned32 ch, unsigned32 cl)
{
    unsigned32 guard=0, w;
    int bx = (int)(bh >> 20) & 0x7ff;
    int cx = (int)(ch >> 20) & 0x7ff;
    int shift = bx - cx;
    if (shift < -54 || bx == 0)
    {   a[0] = ch & 0x7fffffff;
        a[1] = cl;
        return 1;
    }
    if (shift > 54 || cx == 0)
    {   a[0] = bh & 0x7fffffff;
        a[1] = bl;
        return 1;
    }
    bh = (bh & 0xfffff) | 0x100000;
    ch = (ch & 0xfffff) | 0x100000;
/* Now I need to align the operands                                      */
    if (shift > 0)
    {   int rshift;
        if (shift > 32)
        {   guard = cl;
            cl = ch;
            ch = 0;
            shift -= 32;
        }
        rshift = 32 - shift;
        if (guard == 0) guard = cl << rshift;
        else guard = (cl << rshift) | 1;
        cl = (ch << rshift) | (cl >> shift);
        ch = ch >> shift;
    }
    else if (shift < 0)
    {   int rshift;
        if (shift < -32)
        {   guard = bl;
            bl = bh;
            bh = 0;
            shift += 32;
        }
        shift = -shift;
        rshift = 32 - shift;
        if (guard == 0) guard = bl << rshift;
        else guard = (bl << rshift) | 1;
        bl = (bh << rshift) | (bl >> shift);
        bh = bh >> shift;
        bx = cx;
    }
    else guard = 0;
    w = (bl & 0xff) + (cl & 0xff);
    bl = (bl >> 8) + (cl >> 8) + (w >> 8);
    bh += ch + (bl >> 24);
    bl = (bl << 8) + (w & 0xff);
    if (bh & 0x00200000)
    {   guard = (bl << 31) | (guard >> 1);
        bl = (bh << 31) | (bl >> 1);
        bh = bh >> 1;
        bx += 1;
        if (bx >= 0x7ff) return 0;          /* Overflow case             */
    }
    if (_needcarry(guard, bl))
    {   if (bl == 0xffffffff)
        {   bl = 0;
            bh += 1;
            if (bh & 0x00200000)
            {   bh = 0x00100000;
                bx += 1;
                if (bx >= 0x7ff) return 0;      /* Overflow case         */
            }
        }
        else bl += 1;
    }
    a[0] = (bh & ~0x00100000) | ((int32)bx << 20);
    a[1] = bl;
    return 1;
}

bool flt_difference(unsigned32 *a, unsigned32 bh, unsigned32 bl,
                                    unsigned32 ch, unsigned32 cl)
{
    unsigned32 bg=0, cg=0, w, resultsign;
    int bx = (int)(bh >> 20) & 0x7ff;
    int cx = (int)(ch >> 20) & 0x7ff;
    int shift = bx - cx;
    if (shift < -54 || bx == 0)
    {   a[0] = ch | 0x80000000;
        a[1] = cl;
        return 1;
    }
    if (shift > 54 || cx == 0)
    {   a[0] = bh & 0x7fffffff;
        a[1] = bl;
        return 1;
    }
    bh = (bh & 0xfffff) | 0x100000;
    ch = (ch & 0xfffff) | 0x100000;
/* Now I need to align the operands                                      */
    if (shift > 0)
    {   int rshift;
        if (shift > 32)
        {   cg = cl;
            cl = ch;
            ch = 0;
            shift -= 32;
        }
        rshift = 32 - shift;
        if (cg == 0) cg = cl << rshift;
        else cg = (cl << rshift) | 1;
        cl = (ch << rshift) | (cl >> shift);
        ch = ch >> shift;
        bg = 0;
    }
    else if (shift < 0)
    {   int rshift;
        if (shift < -32)
        {   bg = bl;
            bl = bh;
            bh = 0;
            shift += 32;
        }
        shift = -shift;
        rshift = 32 - shift;
        if (bg == 0) bg = bl << rshift;
        else bg = (bl << rshift) | 1;
        bl = (bh << rshift) | (bl >> shift);
        bh = bh >> shift;
        cg = 0;
        bx = cx;
    }
    else bg = cg = 0;
/* Now for a subtraction, taking account of signs. Ugh.                  */
/* I rely on right shifts on signed types being arithmetic in struggles  */
/* to implement multiple precision arithmetic without a proper add-carry */
/* operator in my language.                                              */
    w = (bg & 0xff) - (cg & 0xff);
    bg = (bg >> 8) - (cg >> 8) + signed32_rightshift_(w, 8);
    cg = signed32_rightshift_(bg, 24);
    bg = (bg << 8) | (w & 0xff);
    w = (bl & 0xff) - (cl & 0xff) + cg;
    bl = (bl >> 8) - (cl >> 8) + signed32_rightshift_(w, 8);
    cg = signed32_rightshift_(bl, 24);
    bl = (bl << 8) | (w & 0xff);
    bh = bh - ch + cg;
/* Subtraction complete in 2s complement form.                           */
    if (bh & 0x80000000)  /* Sign of result must be negative             */
    {   if (bg == 0)
        {   if (bl == 0) bh = -bh;
            else
            {   bl = -bl;
                bh = ~bh;
            }
        }
        else
        {   bg = -bg;
            bl = ~bl;
            bh = ~bh;
        }
        resultsign = 0x80000000;
    }
    else resultsign = 0;
/* Subtraction now complete in sign & magnitude form                     */
    if (bh == 0 && bl == 0 && bg == 0)
    {   a[0] = a[1] = 0;           /* Result is absolutely zero          */
        return 1;
    }
/* Must have a normalized result.                                        */
    if ((bh & 0x00300000)==0)       /* need to renormalize?              */
    {   while (bh == 0)
        {   bh = bl >> 11;
            bl = (bl << 21) | (bg >> 11);
            bg = bg << 21;
            bx -= 21;
            if (bx <= 0)
            {   a[0] = a[1] = 0;
                return 1;
            }
        }
        while ((bh & 0x001fe000) == 0)
        {   bh = (bh << 8) | (bl >> 24);
            bl = (bl << 8) | (bg >> 24);
            bg = bg << 8;
            bx -= 8;
            if (bx <= 0)
            {   a[0] = a[1] = 0;
                return 1;
            }
        }
        while ((bh & 0x001c0000) == 0)
        {   bh = (bh << 3) | (bl >> 29);
            bl = (bl << 3) | (bg >> 29);
            bg = bg << 3;
            bx -= 3;
            if (bx <= 0)
            {   a[0] = a[1] = 0;
                return 1;
            }
        }
        while ((bh & 0x00100000) == 0)
        {   bh = (bh << 1) | (bl >> 31);
            bl = (bl << 1) | (bg >> 31);
            bg = bg << 1;
            bx -= 1;
            if (bx <= 0)
            {   a[0] = a[1] = 0;
                return 1;
            }
        }
    }
    else if (bh & 0x00200000)
    {   bg = (bl << 31) | (bg >> 1);
        bl = (bh << 31) | (bl >> 1);
        bh = bh >> 1;
        bx += 1;
        if (bx >= 0x7ff) return 0;      /* Overflow case                 */
    }
/* The magic rules about when and how to round are implemented here      */
    if (_needcarry(bg, bl))
    {   if (bl == 0xffffffff)
        {   bl = 0;
            bh += 1;
            if (bh & 0x00200000)
            {   bh = 0x00100000;
                bx += 1;
                if (bx >= 0x7ff) return 0;      /* Overflow case         */
            }
        }
        else bl += 1;
    }
    a[0] = (bh & ~0x00100000) | ((int32)bx << 20) | resultsign;
    a[1] = bl;
    return 1;
}

#endif /* IBMFLOAT */

long double __dadd(unsigned32 bh, unsigned32 bl,
                   unsigned32 ch, unsigned32 cl)
{
    bool ok;
    dblrep a;
/* Do a case analysis on signs to end up with arithmetic on unsigned floats */
    if (bh & 0x80000000)
    {   if (ch & 0x80000000)
        {   ok = flt_sum(a.ua, bh, bl, ch, cl);
            a.ua[0] ^= 0x80000000;
        }
        else ok = flt_difference(a.ua, ch, cl, bh, bl);
    }
    else if (ch & 0x80000000)
        ok = flt_difference(a.ua, bh, bl, ch, cl);
    else ok = flt_sum(a.ua, bh, bl, ch, cl);
    if (!ok) raise(SIGFPE);
    return a.d;
}

long double __dsub(unsigned32 bh, unsigned32 bl,
                   unsigned32 ch, unsigned32 cl)
{
    bool ok;
    dblrep a;
    if (bh & 0x80000000)
    {   if (ch & 0x80000000)
            ok = flt_difference(a.ua, ch, cl, bh, bl);
        else
        {   ok = flt_sum(a.ua, bh, bl, ch, cl);
            a.ua[0] ^= 0x80000000;
        }
    }
    else if (ch & 0x80000000)
        ok = flt_sum(a.ua, bh, bl, ch, cl);
    else ok = flt_difference(a.ua, bh, bl, ch, cl);
    if (!ok) raise(SIGFPE);
    return a.d;
}


#ifdef IBMFLOAT

long double __dmul(unsigned32 bh, unsigned32 bl,
                   unsigned32 ch, unsigned32 cl)
{
    dblrep a;
    unsigned32 ah, al, carry;
    unsigned32 aa[7], bb[4], cc[4];
    int i, j;
    int32 as = (bh ^ ch) & 0x80000000;    /* sign for result */
    int ax;
    int bx = (int)(bh >> 24) & 0x7f;
    int cx = (int)(ch >> 24) & 0x7f;
    if ((bh==0 && bl==0) || (ch==0 && cl==0))
    {   /* multiplication by 0.0         */
        /* note treatment of sign.       */
        return i2dbl_(a, as, 0);
    }
    bh = bh & 0xffffff;
    ch = ch & 0xffffff;
/* I split the operands into 14-bit chunks and do a long multiplication. */
/* As coded here I put more effort than might really be needed into the  */
/* low order bits of the product, but for now I am more concerned with   */
/* ease of coding and accuracy of results than with absolute speed.      */
    bb[0] = bh >> 10;
    bb[1] = ((bh & 0x3ff) << 4) | (bl >> 28);
    bb[2] = (bl >> 14) & 0x3fff;
    bb[3] = bl & 0x3fff;
    cc[0] = ch >> 10;
    cc[1] = ((ch & 0x3ff) << 4) | (cl >> 28);
    cc[2] = (cl >> 14) & 0x3fff;
    cc[3] = cl & 0x3fff;
    aa[0] = aa[1] = aa[2] = aa[3] = aa[4] = aa[5] = aa[6] = 0;
    for (i=0; i<4; i++)
        for (j=0; j<4; j++)
            aa[i+j] += bb[i] * cc[j];
    carry = 0;
    for (i=6; i!=0; i--)
    {   unsigned32 w = aa[i] + carry;
        aa[i] = w & 0x3fff;
        carry = w >> 14;
    }
    carry = aa[0] + carry;
    ax = bx + cx - 0x40;
    if ((carry & 0x0f000000) == 0)
    {   carry = (carry << 4) | (aa[1] >> 10);
        aa[1] = ((aa[1] << 4) & 0x3fff) | (aa[2] >> 10);
        aa[2] = ((aa[2] << 4) & 0x3fff) | (aa[3] >> 10);
        ax -= 1;
    }
    ah = carry >> 4;
    al = ((carry & 0xf) << 28) | (aa[1] << 14) | aa[2];
    if (ax > 0x7f) raise(SIGFPE); /* Overflow */
    else if (ax < 0)
    {   /* N.B. keep sign on underflow     */
        return i2dbl_(a, as, 0);
    }
    return i2dbl_(a, ah | ((int32)ax << 24) | as, al);
}

long double __ddiv(unsigned32 bh, unsigned32 bl,
                   unsigned32 ch, unsigned32 cl)
{
    dblrep a;
    unsigned32 ah, al, as = (bh ^ ch) & 0x80000000;
    int i;
    int ax;
    int bx = (int)(bh >> 24) & 0x7f;
    int cx = (int)(ch >> 24) & 0x7f;
    if ((ch & 0x7fffffff) == 0 && cl == 0) raise(SIGFPE); /* .. by zero  */
    if ((bh & 0x7fffffff)==0 && bl == 0)
    {   /*  0.0 / anything  = 0.0        */
        return i2dbl_(a, as, 0);
    }
    bh = bh & 0xffffff;
    ch = ch & 0xffffff;
    ax = bx - cx + 0x40;
    ah = al = 0;
/* Do the division by test-and-subtract                                  */
    for (i = 0; i<=(56/4); i++)
    {   int nxt = 0;
        while (bh > ch || (bh == ch && bl >= cl))
        {   unsigned32 w = (bl & 0xff) - (cl & 0xff);
/* Do a double length subtraction (oh the carry is a misery)             */
            bl = (bl >> 8) - (cl >> 8) + signed32_rightshift_(w,8);
            bh = bh - ch + signed32_rightshift_(bl,24);
            bl = (bl << 8) | (w & 0xff);
            nxt++;
        }
        ah = (ah << 4) | (al >> 28);
        al = (al << 4) | nxt;
        bh = (bh << 4) | (bl >> 28);
        bl = bl << 4;
    }
    if (ah & 0xff000000)
    {   al = (al >> 4) | (ah << 28);
        ah = ah >> 4;
        ax += 1;
    }
    if (ax > 0x7f) raise(SIGFPE);     /* Overflow on the division        */
    else if (ax < 0)
    {   /* N.B. keep sign on underflow     */
        return i2dbl_(a, as, 0);
    }
    return i2dbl_(a, ah | ((int32)ax << 24) | as, al);
}

#else /* IBMFLOAT */

long double __dmul(unsigned32 bh, unsigned32 bl,
                   unsigned32 ch, unsigned32 cl)
{
    dblrep a;
    unsigned32 ah, al, carry;
    unsigned32 aa[7], bb[4], cc[4];
    int i, j;
    int32 as = (bh ^ ch) & 0x80000000;    /* sign for result */
    int ax;
    int bx = (int)(bh >> 20) & 0x7ff;
    int cx = (int)(ch >> 20) & 0x7ff;
    if (bx==0 || cx==0)
    {   /* multiplication by 0.0         */
        /* note treatment of sign.       */
        return i2dbl_(a, as, 0);
    }
    bh = (bh & 0xfffff) | 0x100000;
    ch = (ch & 0xfffff) | 0x100000;
/* I split the operands into 14-bit chunks and do a long multiplication. */
/* As coded here I put more effort than might really be needed into the  */
/* low order bits of the product, but for now I am more concerned with   */
/* ease of coding and accuracy of results than with absolute speed.      */
/* On the ARM it MIGHT be that a shift-and-add long multiply coded at    */
/* the level would be faster?                                            */
    bb[0] = bh >> 7;
    bb[1] = ((bh & 0x7f) << 7) | (bl >> 25);
    bb[2] = (bl >> 11) & ~0x003fc000;
    bb[3] = (bl << 3) & 0x3fff;
    cc[0] = ch >> 7;
    cc[1] = ((ch & 0x7f) << 7) | (cl >> 25);
    cc[2] = (cl >> 11) & ~0x003fc000;
    cc[3] = (cl << 3) & 0x3fff;
    aa[0] = aa[1] = aa[2] = aa[3] = aa[4] = aa[5] = aa[6] = 0;
    for (i=0; i<4; i++)
        for (j=0; j<4; j++)
            aa[i+j] += bb[i] * cc[j];
    carry = 0;
    for (i=6; i!=0; i--)
    {   unsigned32 w = aa[i] + carry;
        aa[i] = w & 0x3fff;
        carry = w >> 14;
    }
    carry = aa[0] + carry;
    ax = bx + cx - 0x3fe;
    if ((carry & 0x08000000) == 0)
    {   carry = (carry << 1) | (aa[1] >> 13);
        aa[1] = ((aa[1] << 1) & ~(int32)0xc000) | (aa[2] >> 13);
        aa[2] = ((aa[2] << 1) & ~(int32)0xc000) | (aa[3] >> 13);
/* aa[3] to aa[6] are guard digits and do not need shifting here (!)     */
        ax -= 1;
    }
    ah = carry >> 7;
    al = ((carry & 0x7f) << 25) | (aa[1] << 11) | (aa[2] >> 3);
    carry = ((aa[2] & 0x3) | aa[3] | aa[4] | aa[5] | aa[6]) |
            ((aa[2] & 0x4) << 29);
/* The magic rules about when and how to round are implemented here      */
    if (_needcarry(carry, al))
    {   if (al == 0xffffffff)
        {   al = 0;
            ah += 1;
            if (ah & 0x00200000)
            {   ah = 0x00100000;
                ax += 1;
            }
        }
        else al += 1;
    }
    if (ax >= 0x7ff) raise(SIGFPE);      /* Overflow */
    else if (ax <= 0)
    {   /* N.B. keep sign on underflow     */
        return i2dbl_(a, as, 0);
    }
    return i2dbl_(a, (ah & ~0x00100000) | ((int32)ax << 20) | as, al);
}

long double __ddiv(unsigned32 bh, unsigned32 bl,
                   unsigned32 ch, unsigned32 cl)
{
    dblrep a;
    unsigned32 ah, al, as = (bh ^ ch) & 0x80000000;
    int i;
    int ax;
    int bx = (int)(bh >> 20) & 0x7ff;
    int cx = (int)(ch >> 20) & 0x7ff;
    if (cx == 0) raise(SIGFPE);         /* division by zero              */
    if (bx == 0)
    {   /*  0.0 / anything  = 0.0        */
        return i2dbl_(a, as, 0);
    }
    bh = (bh & 0xfffff) | 0x00100000;
    ch = (ch & 0xfffff) | 0x00100000;
    ax = bx - cx + 0x3fe;
    ah = al = 0;
/* Do the division by test-and-subtract                                  */
    for (i = 0; i<55; i++)
    {   if (bh > ch || (bh == ch && bl >= cl))
        {   unsigned32 w = (bl & 0xff) - (cl & 0xff);
/* Do a double length subtraction (oh the carry is a misery)             */
            bl = (bl >> 8) - (cl >> 8) + signed32_rightshift_(w,8);
            bh = bh - ch + signed32_rightshift_(bl,24);
            bl = (bl << 8) | (w & 0xff);
            ah = (ah << 1) | (al >> 31);
            al = (al << 1) | 1;
        }
        else
        {   ah = (ah << 1) | (al >> 31);
            al = al << 1;
        }
        bh = (bh << 1) | (bl >> 31);
        bl = bl << 1;
    }
    bh |= bl;                                    /* sticky bits now here */
    bh = (bh & 0xff) | (bh >> 8);                /* top bit clear.       */
    if (ah & 0x00400000)
    {   bh |= al & 1;
        al = (al >> 1) | (ah << 31);
        ah = ah >> 1;
        ax += 1;
    }
    bh = bh | (al << 31);
    al = (al >> 1) | (ah << 31);
    ah = ah >> 1;
/* The magic rules about when and how to round are implemented here      */
    if (_needcarry(bh, al))
    {   if (al == 0xffffffff)
        {   al = 0;
            ah += 1;
            if (ah & 0x00200000)
            {   ah = 0x00100000;
                ax += 1;
            }
        }
        else al += 1;
    }
    if (ax >= 0x7ff) raise(SIGFPE); /* Overflow on the division          */
    else if (ax <= 0)
    {   /* N.B. keep sign on underflow     */
        return i2dbl_(a, as, 0);
    }
    return i2dbl_(a, (ah & ~0x00100000) | ((int32)ax << 20) | as, al);
}

#endif /* IBMFLOAT */

long double __dneg(int32 bh, int32 bl)
{
    dblrep a;
    if (bh == 0 && bl == 0)
        return i2dbl_(a, 0, 0);
    else
        return i2dbl_(a, bh ^ 0x80000000, bl);
}


bool __dgr(int32 bh, unsigned32 bl,    int32 ch, unsigned32 cl)
{
/* +0.0 is equal to -0.0                                                 */
#ifdef IBMFLOAT
    if ((bh & 0x7fffffff) == 0 && bl == 0 &&
        (ch & 0x7fffffff) == 0 && cl == 0) return 0;
#else
    if ((bh & 0x7ff00000)==0 && (ch & 0x7ff00000)==0) return 0;
#endif
    if (bh < 0 && ch >= 0) return 0;
    else if (bh >= 0 && ch < 0) return 1;
    else if (bh < 0)
    {   int32 temp = bh;
        bh = ch & 0x7fffffff;
        ch = temp & 0x7fffffff;
        temp = bl;
        bl = cl;
        cl = temp;
    }
    if (bh < ch) return 0;
    else if (bh > ch) return 1;
    else if (bl < cl) return 0;
    else if (bl > cl) return 1;
    else return 0;
}

bool __dgeq(int32 ah, unsigned32 al,  int32 bh, unsigned32 bl)
{
    return !__dgr(bh,bl, ah,al);
}

bool __dls(int32 ah, unsigned32 al,  int32 bh, unsigned32 bl)
{
    return __dgr(bh,bl, ah,al);
}

bool __dleq(int32 ah, unsigned32 al,  int32 bh, unsigned32 bl)
{
    return !__dgr(ah,al, bh,bl);
}

bool __deq(unsigned32 bh, unsigned32 bl,   unsigned32 ch, unsigned32 cl)
{
    if (bl != cl) return 0;
    if (bh == ch) return 1;
/* I ensure that +0 == -0 here.                                          */
    if (bh != 0 && bh != 0x80000000) return 0;
    if (ch != 0 && ch != 0x80000000) return 0;
    else return 1;
}

bool __dneq(unsigned32 ah, unsigned32 al,   unsigned32 bh, unsigned32 bl)
{
    return !__deq(ah,al, bh,bl);
}


#ifdef IBMFLOAT

long double __dflt(int32 n)
{
    dblrep a;
    unsigned32 ah, al;
    int32 as;
    int ax;
    if (n==0)
        return i2dbl_(a, 0, 0);
    else if (n>0) as = 0;
    else
    {   as = 0x80000000;
        n = -n;             /* from here on n is thought of as unsigned  */
    }
    ah = 0;
    al = n;
    ax = 0x40 + (56/4);
    while ((ah & 0x00f00000)==0)
    {   ah = (ah << 4) | (al >> 28);
        al = al << 4;
        ax -= 1;
    }
    return i2dbl_(a, ah | ((int32)ax << 24) | as, al);
}

long double __dfltu(unsigned32 n)
{
    dblrep a;
    unsigned32 ah, al;
    int ax;
    if (n==0)
        return i2dbl_(a, 0, 0);
    ah = 0;
    al = n;
    ax = 0x40 + (56/4);
    while ((ah & 0x00f00000)==0)
    {   ah = (ah << 4) | (al >> 28);
        al = al << 4;
        ax -= 1;
    }
    return i2dbl_(a, ah | ((int32)ax << 24), al);
}

int32 __dfix(unsigned32 ah, unsigned32 al)
{
    int sign, ax;
    if (ah & 0x80000000) sign = 1;
    else sign = 0;
    if ((ah & 0x7fffffff)==0 && al==0) return 0;
    ax = (int)(ah >> 24) & 0x7f;
    ah = ah & 0x00ffffff;
    if (ax < 0x41)                      /* arg < 1.0 ... result 0        */
        return 0;
    else if (ax > 0x40 + (56/4)) return raise(SIGFPE); /* overflow       */
    while (ax != 0x40 + (56/4))
    {   al = (al >> 4) | (ah << 28);
        ah = ah >> 4;
        ax += 1;
    }
    if (ah != 0) return raise(SIGFPE); /* Overflow */
    if (sign)
    {   al = -al;
        if ((al & 0x80000000)==0) return raise(SIGFPE);
    }
    else if (al & 0x80000000) return raise(SIGFPE);
    return al;
}

unsigned32 __dfixu(unsigned32 ah, unsigned32 al)
{
    int ax;
    if ((ah & 0x7fffffff)==0 && al==0) return 0;
    ax = (int)(ah >> 24) & 0x7f;
    if ((ah & 0x80000000)!=0) return raise(SIGFPE);    /* negative       */
    ah = ah & 0x00ffffff;
    if (ax < 0x41)                      /* arg < 1.0 ... result 0        */
        return 0;
    else if (ax > 0x40 + (56/4)) return raise(SIGFPE); /* overflow       */
    while (ax != 0x40 + (56/4))
    {   al = (al >> 4) | (ah << 28);
        ah = ah >> 4;
        ax += 1;
    }
    if (ah != 0) return raise(SIGFPE); /* Overflow */
    return al;
}

#else /* IBMFLOAT */

long double __dflt(int32 n)
{
    unsigned32 ah, al;
    dblrep a;
    int32 as;
    int ax;
    if (n==0) return i2dbl_(a, 0, 0);
    else if (n>0) as = 0;
    else
    {   as = 0x80000000;
        n = -n;             /* from here on n is thought of as unsigned  */
    }
    ah = 0;
    al = n;
    ax = 0x400 + 51;
    while ((ah & 0x00100000)==0)
    {   ah = (ah << 1) | (al >> 31);
        al = al << 1;
        ax -= 1;
    }
    return i2dbl_(a, (ah & ~0x00100000) | ((int32)ax << 20) | as, al);
}

long double __dfltu(unsigned32 n)
{
    unsigned32 ah, al;
    dblrep a;
    int ax;
    if (n==0)
        return i2dbl_(a, 0, 0);
    ah = 0;
    al = n;
    ax = 0x400 + 51;
    while ((ah & 0x00100000)==0)
    {   ah = (ah << 1) | (al >> 31);
        al = al << 1;
        ax -= 1;
    }
    return i2dbl_(a, (ah & ~0x00100000) | ((int32)ax << 20), al);
}

int32 __dfix(unsigned32 ah, unsigned32 al)
{
    int sign, ax;
    if (ah & 0x80000000)
    {   ah &= ~0x80000000;
        sign = 1;
    }
    else sign = 0;
    ax = (int)(ah >> 20);
    ah = (ah & 0x000fffff) | 0x00100000;
/* @@@ beware: should next line be 0x3fe as in mip/ieeeflt.c?            */
    if (ax < 0x3ff)                     /* arg < 1.0 ... result 0        */
        return 0;
    else if (ax > 0x400 + 51) return raise(SIGFPE); /* overflow          */
    while (ax != 0x400 + 51)
    {   al = (al >> 1) | (ah << 31);
        ah = ah >> 1;
        ax += 1;
    }
    if (sign)
    {   if (al > 0x80000000) return raise(SIGFPE);
        al = -al;
    }
    else if (al >= 0x80000000) return raise(SIGFPE);
    return al;
}

unsigned32 __dfixu(unsigned32 ah, unsigned32 al)
{
    int ax = (int)(ah >> 20) & 0x7ff;
    if ((ah & 0x80000000)!=0 && ax!=0) return raise(SIGFPE);
                                       /* negative nonzero  */
    ah = (ah & 0x000fffff) | 0x00100000;
    if (ax < 0x3ff)                     /* arg < 1.0 ... result 0        */
        return 0;
    else if (ax > 0x400 + 51) return raise(SIGFPE); /* overflow          */
    while (ax != 0x400 + 51)
    {   al = (al >> 1) | (ah << 31);
        ah = ah >> 1;
        ax += 1;
    }
    return al;
}

#ifdef EXTENSION_FRAC
long double __dfltr(int32 n)
{
    unsigned32 ah, al;
    dblrep a;
    int32 as;
    int ax;
    if (n==0) return i2dbl_(a, 0, 0);
    else if (n>0) as = 0;
    else
    {   as = 0x80000000;
        n = -n;             /* from here on n is thought of as unsigned  */
    }
    ah = 0;
    al = n;
    ax = 0x400 + 51 - 31;
    while ((ah & 0x00100000)==0)
    {   ah = (ah << 1) | (al >> 31);
        al = al << 1;
        ax -= 1;
    }
    return i2dbl_(a, (ah & ~0x00100000) | ((int32)ax << 20) | as, al);
}

int32 __dfixr(unsigned32 ah, unsigned32 al)
{   /* We define double->frac to round.                                 */
    int sign, ax, c;
    if (ah & 0x80000000)
    {   ah &= ~0x80000000;
        sign = 1;
    }
    else sign = 0;
    ax = (int)(ah >> 20);
    ah = (ah & 0x000fffff) | 0x00100000;
    if (ax < 0x3fe - 31)                /* arg < 1.0 ... result 0        */
        return 0;
    else if (ax > 0x400 + 51 - 31) return raise(SIGFPE); /* overflow     */
    c = 0;
    while (ax != 0x400 + 51 - 31)
    {   c = (unsigned int)al & 1;
        al = (al >> 1) | (ah << 31);
        ah = ah >> 1;
        ax += 1;
    }
    al += c;
    if (sign)
    {   if (al > 0x80000000) return raise(SIGFPE);
        al = -al;
    }
    else if (al >= 0x80000000) return raise(SIGFPE);
    return al;
}

#endif /* EXTENSION_FRAC */
#endif /* IBMFLOAT */

/* end of softfp.c */


