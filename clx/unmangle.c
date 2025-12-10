
/*
 * The name unmangling function file unmangle.c
 * Copyright (C) Advanced RISC Machines Limited, 1994. All rights reserved.
 */

/*
 * RCS $Revision: 1.25 $
 * Checkin $Date: 1995/09/29 11:25:43 $
 * Revising $Author: sdouglas $
 */

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "unmangle.h"

/*
 * Basic Types
 */

static char b[] = "bool";
static char v[] = "void";
static char c[] = "char";
static char s[] = "short";
static char i[] = "int";
static char l[] = "long";
static char f[] = "float";
static char d[] = "double";
static char r[] = "long double";
static char e[] = "...";

/*
 * Type modifiers
 */

static char U[] = "unsigned ";
static char C[] = "const "; static char CA[] = " const";
static char V[] = "volatile "; static char VA[] = " volatile";
static char S[] = "signed ";
static char Static[] = "static ";

static char P[] = "*";
static char PF[] = "(*)";
static char R[] = "&";

static char qualified[] = "::";
static char ctor[] = "__ct__";
static char dtor[] = "__dt__";
static char DTOR[] = "~";
static char asterisk[] = "__ml", ml[] = "*";
static char modulo[] = "__md", md[] = "%";
static char minus[] = "__mi", mi[] = "-";
static char rshift[] = "__rs", rs[] = ">>";
static char nequal[] = "__ne", ne[] = "!=";
static char greater[] = "__gt", gt[] = ">";
static char gteq[] = "__ge", ge[] = ">=";
static char bitor[] = "__or", or[] = "|";
static char addr2[] = "__aa", aa[] = "&&";
static char not[] = "__nt", nt[] = "!";
static char plus2[] = "__pp", pp[] = "++";
static char assign[] = "__as", as[] = "=";
static char asspls[] = "__apl", apl[] = "+=";
static char assmu[] = "__amus", amu[] = "*=";
static char assmd[] = "__amd", amd[] = "%=";
static char assrs[] = "__ars", ars[] = ">>=";
static char assor[] = "__aor", aor[] = "|=";
static char comma[] = "__cm", cm[] = ",";
static char divide[] = "__dv", dv[] = "/";
static char plus[] = "__pl", pl[] = "+";
static char lshift[] = "__ls", ls[] = "<<";
static char equal[] = "__eq", eq[] = "==";
static char less[] = "__lt", lt[] = "<";
static char lesseq[] = "__le", le[] = "<=";
static char addr[] = "__ad", ad[] = "&";
static char circumflex[] = "__er", er[] = "^";
static char oror[] = "__oo", oo[] = "||";
static char tilda[] = "__co", co[] = "~";
static char mimi[] = "__mm", mm[] = "--";
static char rarrow[] = "__rf", rf[] = "->";
static char assmi[] = "__ami", ami[] = "-=";
static char assdiv[] = "__adv", adv[] = "/=";
static char asslshift[] = "__als", als[] = "<<=";
static char assad[] = "__aad", aad[] = "&=";
static char asscir[] = "__aer", aer[] = "^=";
static char mempdref[] = "__rm", rm[] = "->*";
static char fncall[] = "__cl", cl[] = "()";
static char subscript[] = "__vc", vc[] = "[]";
static char new[] = "__nw", nw[] = "new";
static char delete[] = "__dl", dl[] = "delete";

static char operator[] = "operator";
static char conv_op[] = "__op";

#define vlen strlen(v)
#define blen strlen(b)
#define clen strlen(c)
#define slen strlen(s)
#define ilen strlen(i)
#define llen strlen(l)
#define flen strlen(f)
#define dlen strlen(d)
#define rlen strlen(r)
#define elen strlen(e)
#define Ulen strlen(U)
#define Clen strlen(C)
#define CAlen strlen(CA)
#define Vlen strlen(V)
#define VAlen strlen(VA)
#define Slen strlen(S)
#define Staticlen strlen(Static)
#define Plen strlen(P)
#define PFlen strlen(PF)
#define Rlen strlen(R)
#define qlen strlen(qualified)
#define DTlen strlen(DTOR)
#define oplen strlen(operator)
#define conv_op_len strlen(conv_op)

typedef enum {Const, Volatile, Mnull} modifier;
typedef enum {Pointer, Reference, Dnull} tdeclarator;
typedef struct {
        int c;
        tdeclarator d;
    } declarator;
typedef enum {on, off} atype;

static char *bptr, *blim, *buf;

#ifdef __STDC__
#define stuffbuf(s, l)  {const char *p = s; int _len = l; \
                         while (bptr < blim && _len) {--_len; *bptr++ = *p++;}}
#else
#define stuffbuf(s, l)  {char *p = s; int _len = l; \
                         while (bptr < blim && _len) {--_len; *bptr++ = *p++;}}

#endif

#define adj(len)        {bptr += len;}
#define attempt_close() {if (bptr[-1] == ',') \
                                {bptr[-1] = ')'; *bptr = '\0';}}
#define check(op)       (strncmp(mangled, op, 4) == 0)
#define stuffopbuf(s)   {stuffbuf(operator,oplen); \
                         stuffbuf(" ", 1); stuffbuf(s, strlen(s));}

static void chk_op(const char *mangled)
{
    if check(asterisk)
        stuffopbuf(ml)
    else if check(modulo)
        stuffopbuf(md)
    else if check(minus)
        stuffopbuf(mi)
    else if check(rshift)
        stuffopbuf(rs)
    else if check(nequal)
        stuffopbuf(ne)
    else if check(greater)
        stuffopbuf(gt)
    else if check(gteq)
        stuffopbuf(ge)
    else if check(bitor)
        stuffopbuf(or)
    else if check(addr2)
        stuffopbuf(aa)
    else if check(not)
        stuffopbuf(nt)
    else if check(plus2)
        stuffopbuf(pp)
    else if check(assign)
        stuffopbuf(as)
    else if check(asspls)
        stuffopbuf(apl)
    else if check(assmu)
        stuffopbuf(amu)
    else if check(assmd)
        stuffopbuf(amd)
    else if check(assrs)
        stuffopbuf(ars)
    else if check(assor)
        stuffopbuf(aor)
    else if check(comma)
        stuffopbuf(cm)
    else if check(divide)
        stuffopbuf(dv)
    else if check(plus)
        stuffopbuf(pl)
    else if check(lshift)
        stuffopbuf(ls)
    else if check(equal)
        stuffopbuf(eq)
    else if check(less)
        stuffopbuf(lt)
    else if check(lesseq)
        stuffopbuf(le)
    else if check(addr)
        stuffopbuf(ad)
    else if check(circumflex)
        stuffopbuf(er)
    else if check(oror)
        stuffopbuf(oo)
    else if check(tilda)
        stuffopbuf(co)
    else if check(mimi)
        stuffopbuf(mm)
    else if check(rarrow)
        stuffopbuf(rf)
    else if check(assmi)
        stuffopbuf(ami)
    else if check(assdiv)
        stuffopbuf(adv)
    else if check(asslshift)
        stuffopbuf(als)
    else if check(assad)
        stuffopbuf(aad)
    else if check(asscir)
        stuffopbuf(aer)
    else if check(mempdref)
        stuffopbuf(rm)
    else if check(fncall)
        stuffopbuf(cl)
    else if check(subscript)
        stuffopbuf(vc)
    else if check(new)
        stuffopbuf(nw)
    else if check(delete)
        stuffopbuf(dl)
    else
        stuffbuf(mangled, 4)
}

static int attempt_conversion(const char *op)
{   const char *cptr = op;
    if (strncmp(op, conv_op, conv_op_len) == 0)
    {   cptr += conv_op_len;
        stuffbuf(operator, oplen);
        stuffbuf(" ", 1);
        {   int ptrcount = 0, refcount = 0;
            while (strncmp(cptr, "__", 2) != 0)
            {   if (isdigit(*cptr))
                {   int len = atoi(cptr);
                    while (isdigit(*cptr)) cptr++;
                    stuffbuf(cptr, len);
                }
                else
                switch(*cptr)
                {   case 'v':
                        stuffbuf(v, vlen);
                        break;
                    case 'b':
                        stuffbuf(b, blen);
                        break;
                    case 'c':
                        stuffbuf(c, clen);
                        break;
                    case 's':
                        stuffbuf(s, slen);
                        break;
                    case 'i':
                        stuffbuf(i, ilen);
                        break;
                    case 'l':
                        stuffbuf(l, llen);
                        break;
                    case 'f':
                        stuffbuf(f, flen);
                        break;
                    case 'd':
                        stuffbuf(d, dlen);
                        break;
                    case 'r':
                        stuffbuf(r, rlen);
                        break;
                    case 'P':
                        ptrcount += 1;
                        break;
                    case 'R':
                        refcount += 1;
                        break;
                    case 'C':
                        stuffbuf(C, Clen);
                        break;
                    default:
                        return 0;
                }
                ++cptr;
            }
            while (ptrcount) {stuffbuf(P, Plen); --ptrcount;}
            while (refcount) {stuffbuf(R, Rlen); --refcount;}
        }
        return 2;
    }
    else
        return 1;
}

static char *type_ended_with_comma(char *cptr)
{   int braces = 0;
    while (cptr < blim && (*cptr != ',' || braces)) 
    {   if (*cptr == '(') ++braces;
        if (*cptr == ')') --braces;
        ++cptr;
    }
    return (cptr == blim) ? 0 : cptr;
}

#define safe_chk(i)     {if (i <= 0) return 0;}

#define do_declarator_or_modifier_(decl, m) \
{\
   switch (decl.d) \
    {\
       case Pointer:\
           for (; decl.c; decl.c--) \
               stuffbuf(P, Plen);\
           decl.d = Dnull; \
           break;\
       case Reference:\
           for (; decl.c; decl.c--)\
               stuffbuf(R, Rlen);\
           decl.d = Dnull; \
           break;\
    }\
    switch (m) \
    {\
       case Const:\
           stuffbuf(CA, CAlen);\
           m = Mnull; \
           break;\
       case Volatile:\
           stuffbuf(VA, VAlen);\
           m = Mnull; \
           break;\
    }\
}

static int lunmangle(const char *mangled)
{
    const char* endpend = 0;
    int index = 0;
    int pendlen = 0;
    int ctpending = 0;
    int dtpending = 0;
    int is_const = 0;
    int is_volatile = 0;
    int is_static = 0;
    char *pf_pos[10];
    int pf_pos_index = 0;

    if (!strncmp(mangled, ctor, 6)) 
        ctpending = 1;
    if (!strncmp(mangled, dtor, 6)) 
        dtpending = 1;
    if (mangled[0])
        endpend = strstr(mangled+1, "__");
    if (endpend == 0) 
        return 0;


    pendlen = endpend - mangled;
    index = pendlen + 2;

    if (isdigit(mangled[index])) 
    {
        const char *classname = 0;
        int len = atoi(&mangled[index]);
        while (isdigit(mangled[index]))
            index++;
        classname = &mangled[index];
        stuffbuf(classname, len);
        if (ctpending || dtpending)
        {
            stuffbuf(qualified, qlen);
            if (dtpending) stuffbuf(DTOR, DTlen);
            stuffbuf(classname, len);
        }
        else if (pendlen) 
        {
            stuffbuf(qualified, qlen);
            if (pendlen == 4)
                chk_op(mangled);
            else
            {   int sofar = attempt_conversion(mangled);
                if (sofar == 0) return 0;
                if (sofar == 1) stuffbuf(mangled, pendlen);
            }
        }
        pendlen = 0;
        index += len;
    }
    else
    {   if (pendlen == 4) {chk_op(mangled); pendlen = 0;}
        if (ctpending || dtpending) return 0;
    }
#define do_qualified_name(i)    {   int qual = i - '0';\
                if (!(qual > 1 && qual < 10))\
                    return 0;\
                ++index;\
                while (qual)\
                {\
                    int len = atoi(&mangled[index]);\
                    while (isdigit(mangled[index]))\
                        index++;\
                    stuffbuf(&mangled[index], len);\
                    index += len;\
                    if (--qual || pendlen)\
                        stuffbuf(qualified, qlen);\
                }}

    while (mangled[index])
        switch (mangled[index++])
        {   case 'Q':       
            {   /* local classes */
                do_qualified_name(mangled[index])
                if (mangled[index] != 'F')
                {   if (pendlen == 0) return 0;
                    stuffbuf(mangled, pendlen);
                    stuffbuf("\0", 1);
                    return 1;
                }
                break;
            }
            case 'C':
                is_const = 1;
                break;
            case 'V':
                is_volatile = 1;
                break;
            case 'S':
                is_static = 1;
                break;
            case 'F':       
            {   modifier m = Mnull, pf_mod = Mnull;    
        /* @@@ things can be both const and volatile */
                declarator decl, pf_decl;
                atype at = off;
#define BUFSIZE 256
#define clear(b)        {int i; for (i = 0; i<BUFSIZE; ++i) b[i] = 0;}
                char ptof[BUFSIZE];
                char *pf = 0, *rtype_at = 0;
                decl.c = 0, decl.d = Dnull;
                pf_decl.c = 0, pf_decl.d = Dnull;
                clear(ptof); pf_pos_index = 0;
                stuffbuf(mangled, pendlen); 
                pendlen = 0;
                stuffbuf("(", 1);
                for(;;) {   switch (mangled[index])
                {   case '_': 
                    {   /* p124 E&S */
                        if (pf)
                        {
                            int len = bptr - pf;
                            strncpy(ptof, pf, len); 
                            ptof[len - 1] = ')';
                            ptof[len] = '\0';
                            adj(-len);
                            rtype_at = bptr;
                            if (pf_pos_index) 
                                pf = pf_pos[--pf_pos_index];
                            else
                                pf = 0;
                            break;
                        }
                        else 
                            return 0;
                    }
                    case 'Q':
                        /* local typedef names */
                        do_qualified_name(mangled[++index])
                        index--;
                        at = on;
                        break;
                    case 'U': 
                        stuffbuf(U, Ulen);
                        break;
                    case 'C': 
                    {   /* things can be const and volatile; push one out first */
                        char lookahead = mangled[index + 1];
                        if (lookahead == 'P' || lookahead == 'R')
                            m = Const;
                        else 
                            stuffbuf(C, Clen);
                        break;
                    }
                    case 'V':
                    {   char lookahead = mangled[index + 1];
                        if (lookahead == 'P' || lookahead == 'R')
                            m = Volatile;
                        else
                            stuffbuf(V, Vlen);
                        break;
                    }
                    case 'S': 
                        stuffbuf(S, Slen);
                        break;
                    case 'P': 
                    {   char lookahead = mangled[index + 1];
                        if (lookahead == 'F')
                        {   
                            if (pf)
                            {   if (pf_pos_index == 9) return 0;
                                                /* @@@ reasonable BUFSIZE?? */
                                pf_pos[pf_pos_index++] = pf;
                            }
                            pf = bptr;
                            pf_decl = decl, decl.c = 0, decl.d = Dnull;
                            pf_mod = m, m = Mnull;
                            stuffbuf(PF, PFlen);
                            stuffbuf("(", 1);
                            ++index;
                        } 
                        else 
                        {   decl.c += 1;
                            decl.d = Pointer;
                        }
                        break;
                    }
                    case 'A':
                    {   char *ecco = bptr;
                        stuffbuf("[", 1);
                        ++index;
                        while (mangled[index] != '_')
                            stuffbuf(&mangled[index++], 1);
                        stuffbuf("]", 1);
                        strncpy(ptof, ecco, bptr - ecco);
                        adj(-(bptr - ecco));
                        break;
                    }
                    case 'M':
                    {   int len = atoi(&mangled[++index]);
                        const char *classname = 0;      
                        char *ecco;
                        safe_chk(len);
                        while (isdigit(mangled[index]))
                            index++;
                        classname = &mangled[index];
                        index += len;
                        ecco = bptr;
                        if (mangled[index] == 'F')
                        {   pf = bptr;
                            stuffbuf("(", 1);
                        }
                        stuffbuf(classname, len);
                        stuffbuf(qualified, qlen);
                        stuffbuf(P, Plen);
                        if (!pf)
                        {   strncpy(ptof, ecco, bptr - ecco);
                            adj(-(bptr - ecco));
                        }
                        if (mangled[index] == 'F') stuffbuf(")", 1);
                        --index;
                        break;
                    }
                    case 'R': 
                        decl.c += 1;
                        decl.d = Reference;
                        break;
                    case 'v': 
                    {   char before = mangled[index - 1], after = mangled[index + 1];
                        /* Is this a 'Fv' or 'Fv_<rtype>'? */
                        if (before == 'F' && (!after || after == '_'))
                        {
                            stuffbuf(",", 1);
                        }
                        else
                        {   
                            stuffbuf(v, vlen);
                            at = on;
                        } 
                        break;
                    }
                    case 'b':
                        stuffbuf(b, blen);
                        at = on;
                        break;
                    case 'c': 
                        stuffbuf(c, clen);
                        at = on; 
                        break;
                    case 's': 
                        stuffbuf(s, slen);
                        at = on; 
                        break;
                    case 'i': 
                        stuffbuf(i, ilen);
                        at = on; 
                        break;
                    case 'l': 
                        stuffbuf(l, llen);
                        at = on; 
                        break;
                    case 'f': 
                        stuffbuf(f, flen);
                        at = on; 
                        break;
                    case 'd': 
                        stuffbuf(d, dlen);
                        at = on; 
                        break;
                    case 'r': 
                        stuffbuf(r, rlen);
                        at = on; 
                        break;
                    case 'e': 
                        stuffbuf(e, elen);
                        at = on; 
                        break;
                    case '0':
                    case '1':
                    case '2':
                    case '3':
                    case '4':
                    case '5':
                    case '6':
                    case '7':
                    case '8':
                    case '9':
                    {   int len = atoi(&mangled[index]);
                        while (isdigit(mangled[index]))
                            index++;
                        stuffbuf(&mangled[index], len);
                        index += (len - 1);
                        at = on;
                        break;
                    }
                    case 'T': 
                    {   char *begin, *end, lbuf[2];
                        int numb;

                        lbuf[0] = mangled[++index]; lbuf[1] = '\0';
                        numb = atoi(lbuf);
                        /* pf mustbe "(*)(...", hence pf+1 */
                        begin = (pf) ? strchr(pf+1, '(') : strchr(buf, '(');
                        if (!begin) return 0;
                        end = type_ended_with_comma(begin + 1);
                        if (!end) return 0;
                        safe_chk(numb);
                        begin++; numb--;
                        while (numb) 
                        {   begin = end + 1;
                            if (begin)
                            { 
                                end = type_ended_with_comma(begin);
                                if (!end) return 0;
                            }
                            else 
                                return 0; 
                            numb--;
                        }
                        stuffbuf(begin, end - begin);
                        at = on;
                        break;
                    }
                    case 'N': 
                    {   int times, which;
                        char *begin, *end, lbuf[2];

                        lbuf[1] = '\0';
                        lbuf[0] = mangled[++index];
                        times = atoi(lbuf);
                        lbuf[0] = mangled[++index];
                        which = atoi(lbuf);
        /* Assumption: a pntr to fn must be of the form (*)(...) */
                        begin = (pf) ? strchr(pf + 1, '(') : strchr(buf, '(');
                        if (!begin) return 0;
                        end = type_ended_with_comma(begin + 1);
                        if (!end) return 0;
                        safe_chk(times); safe_chk(which);
                        begin++; which--;
                        while (which) 
                        {
                            begin = end + 1;
                            if (begin)
                            {
                                end = type_ended_with_comma(begin);
                                if (!end) return 0;
                            }
                            else 
                                return 0; 
                            which--;
                        }
                        for (; times; times--) 
                        {   char *i = begin;
                            char *j = end;
                            stuffbuf(i, j - i); 
                            stuffbuf(",", 1); 
                        }
                        adj(-1);
                        at = on;
                        break;
                    }                     
                    case '\0':  
                    default:  goto lab1; /* @@@ probably an error */
                } 
                if (at == on) 
                {   do_declarator_or_modifier_(decl, m);
        /*@@@ needs more attention */
                    if (ptof[0])
                    {   char local[256];
                        if (pf_decl.c || pf_mod != Mnull)
                        {   int tmp = bptr - rtype_at;
                            strncpy(local, rtype_at, tmp);
                            adj(-tmp);
                            do_declarator_or_modifier_(pf_decl, pf_mod);
                            strncpy(bptr, local, tmp); adj(tmp);
                        }
                        stuffbuf(" ", 1);
                        stuffbuf(ptof, strlen(ptof));
                        clear(ptof);
                    } 
                    stuffbuf(",", 1);
                    at = off;
                } /* if */
                ++index;
            } /* for(;;) */
            lab1:
                attempt_close();
                break;
            default:
                return 0;
            } /* case 'F' */
        } /* switch (manglex[index<++]) */
    if (is_const) stuffbuf(CA, CAlen);
    if (is_volatile) stuffbuf(VA, VAlen);
    if (is_static)
    {   int len = bptr - buf;
        char lbuf[256];

        bptr = buf + Staticlen;
        strncpy(lbuf, buf, len);
        stuffbuf(lbuf, len);
        bptr = buf;
        stuffbuf(Static, Staticlen);
        bptr = buf + Staticlen + len;
    }
    stuffbuf("\0", 1);
    return 1;
}

void unmangle(const char *name, char *buffer, int maxlen)
{   
    bptr = buf = buffer;
    blim = buffer + maxlen;
    if (!lunmangle(name))
    {   bptr = buffer;
        stuffbuf(name, strlen(name));
        stuffbuf("\0", 1);
    }
}
