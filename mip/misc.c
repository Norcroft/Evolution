/*
 * misc.c: Misc features for the Norcroft C compiler
 * Copyright (C) Codemist Ltd, 1988-1992.
 * Copyright (C) Acorn Computers Ltd., 1988-1990.
 * Copyright (C) Advanced RISC Machines Limited, 1990-1992.
 */

/*
 * RCS $Revision: 1.48 $  Codemist 70
 * Checkin $Date: 1995/11/01 16:46:38 $
 * Revising $Author: hmeekings $
 */

/* Find cc_err below for discussion on error messages, and errors.h   */

/*
 * IDJ: 06-Jun-94: added code to support Acorn's DDE throwback facility:
 * all errors result in a SWI to DDEUtils module to send errors back
 * to the editor.  All guarded by #ifdef FOR_ACORN.
 *
 */

#include <stddef.h>
#ifdef __STDC__
#  include <stdlib.h>
#  include <stdarg.h>
#  include <string.h>
#else
#  include <varargs.h>
#  include <strings.h>
#endif
#include <ctype.h>

#include "globals.h"
#ifndef _defs_LOADED
#  include "defs.h"
#endif
#include "store.h"
#include "aeops.h"
#include "aetree.h"
#include "lex.h"               /* for curlex... */
#include "bind.h"            /* for isgensym... */
#include "util.h"
#ifdef CPLUSPLUS
#  include "unmangle.h"
#else
#  define unmangle(a,b,c) 0
#endif
#include "msg.h"                /* NLS */

#ifdef FOR_ACORN
#include "dde.h"
#include "dem.h"     /* cfront name demangling */
#define listing()    (listingstream != NULL || dde_throwback_flag != 0)
#else
#define listing()    (listingstream != NULL)
#endif

#ifndef NLS
#  undef msg_sprintf
#  define msg_sprintf _sprintf
#  undef msg_vfprintf
#  define msg_vfprintf _vfprintf
#  define DEFINE_MSG_COMPRESSION_TABLE 1
#  include "errors.h"
#  undef DEFINE_MSG_COMPRESSION_TABLE
#else
#  include "errors.h"           /* Could set COMPRESSED_MESSAGES */
#  if defined(COMPRESSED_MESSAGES)
#    undef COMPRESSED_MESSAGES  /* ... which isn't true for NLS ... */
#  endif
#endif

long sysdebugmask;
int32 suppress, feature;
int32 localcg_debugcount;
FILE *listingstream;
#ifdef PASCAL /*ECN*/
int32 rtcheck;
#endif
#ifdef FOR_ACORN
static int32 throwback_idx;   /* id of throwback session */
#endif

/* The following routines are generic list-manipulation routines.       */
/* They can only be implemented by a hack in C, but in C++ they can     */
/* be implemented by basing all list-like classes on 'List'.            */
/* In C, a struct S with a 'cdr' pointer at offset 0 can be reversed    */
/* destructively by (e.g.)  (S *)dreverse((List *)whatever).            */

int32 length(List *l)
{
    int32 i = 0;
    for (; l != NULL; l = cdr_(l)) i++;
    return i;
}

List *dreverse(List *x)         /* return reverse(x) by destroying x.   */
{   List *y = 0, *t;
    while (x != 0)
    {   t = cdr_(x);
        cdr_(x) = y;
        y = x;
        x = t;
    }
    return y;
}

List *nconc(List *x, List *y)   /* return append(x,y) by destroying x.  */
{   if (x == 0) return y;
    else
    {   List *t, *a = x;
        while ((t = cdr_(x)) != 0) x = t;
        cdr_(x) = y;
        return a;
    }
}

int32 max(int32 a, int32 b)
{
    return (a>b ? a : b);
}

int32 bitcount(int32 n)
{
/* return the number of bits present in the integer n.                   */
    int32 r = 0;
    while (n!=0) n ^= n & (-n), r++;
    return(r);
}

int32 logbase2(int32 n)
{
/* n known to be a power of two                                          */
    int32 r = 0;
    unsigned32 m = n & 0xffffffff;
    while ((m & 1)==0) m >>= 1, r++;
    return r;
}

int32 power_of_two(int32 n)
{
/* If n is an exact power of two this returns the power, else -1         */
    if (n == 0 || n != (n&(-n))) return -1;
    return logbase2(n);
}

/* 'real' in the following represents generic float/double/long double */
/* real_of_string() uses GLOBAL store for its value.  Is this reasonable? */
/* Special case flag==0 for real_to_real (and sem.c/fltrep_from_widest()) */
FloatCon *real_of_string(const char *s, int32 flag)
{   int32 wsize = offsetof(FloatCon,floatstr[0]) + padstrlen(strlen(s));
    FloatCon *x = (FloatCon *) GlobAlloc(SU_Const, wsize);
    /* real_of_string is now also used to initialise fc_two_32. */
    x->h0 = s_floatcon;
    strcpy(x->floatstr, s);
    if (flag)
    {   int failed;
        x->floatlen = flag;       /* the bitoftype_ / curlex.a2.flag pun!!! */
/* Currently sizeof_double==sizeof_ldble is assumed.                    */
        failed = fltrep_stod(s, &x->floatbin.db, NULL);
        if (failed != flt_ok) flt_report_error(failed);
        if (sizeof_double == 4 ||
            sizeof_float < sizeof_double  &&  (flag & bitoftype_(s_short)))
            /* so we only need to narrow once  */
            fltrep_narrow_round(&x->floatbin.db, &x->floatbin.fb);
    }
    return x;
}

/* the next two routines do not really belong here, but they do manipulate
   the same structure as make_floating */

#ifdef FORTRAN
/* This code was previously used in flt->flt casts only, now exploits   */
/* the more general code around sem.c/trycastreduce().                  */
/* Check f77/pascal don't use.                                          */
FloatCon *real_to_real(FloatCon *fc, SET_BITMAP m)
{   FloatCon *x = real_of_string(fc->floatstr, 0);
    /* N.B. the last line just copies the floating point number text
       and so this text does not reflect narrowing and re-widening.
       The string is only provided for the ASM output not AOF, and so
       the inaccuracy in it is not critical: the associated binary FP
       value will be kept exactly as it should be and that is what
       matters.  */
    x->floatlen = m;
    memcpy(&x->floatbin, &fc->floatbin, sizeof(x->floatbin));
/* long double = double in this implementation so widen/narrow is easy: */
    if (sizeof_float < sizeof_double)
    {   SET_BITMAP mfc = fc->floatlen;
        if ((m & bitoftype_(s_short)) && !(mfc & bitoftype_(s_short)))
            fltrep_narrow(&fc->floatbin.db, &x->floatbin.fb);
        else if (!(m & bitoftype_(s_short)) && (mfc & bitoftype_(s_short)))
            fltrep_widen(&fc->floatbin.fb, &x->floatbin.db);
    }
    return x;
}
#endif

FloatCon *int_to_real(int32 n, int32 u, SET_BITMAP m)
{   char s[20];
    /* there must be better ways - e.g. fltrep_itod in fltrep.c ??       */
    /* The use of sprintf/real_of_string ensures that the floatcon that  */
    /* is generated has a sensible string attached to it so leave this   */
    /* code alone - it works quite well enough.                          */
    if (u) _sprintf(s, "%lu.0", (long)n);
    else _sprintf(s, "%ld.0", (long)n);
    return real_of_string(s,m);
}

/* error message routines...
   0) cc_msg:  Internal compiler messages, enabled by DEBUG_xxx flags.
   1) cc_warn:  perfectly legal, but curious, C program (e.g. = for ==),
                or 'undefined at run time' (e.g. printf("%f", 1);).
   2)           Offences againgst ANSI draft (Return normally).
   2') cc_pccwarn: Recoverable, only a warning if in pcc mode.
   2a) cc_rerr:   Recoverable error without loss of code (e.g. int *x=1;)
                  Code 'works' -- compiled as on UNIX.
   2b) cc_err:    Code probably lost, e.g. syntax error, or
                       { struct {int a;}*x; f(x->b); }
                  Sets 'compilation failed' flag.
   3) cc_extension:  ditto, but allows controlled extension use.
   3) cc_fatalerr: A cause for giving up compilation (e.g. out of store/
                   too many errors).  NEVER returns.
   4) syserr:   Internal consistency error.  Exits with legs in air,
                unless debugging (syserr_behaviour) flag set.
                N.B. May return for system debuggers only.
   Note all messages (except class 0 and 4) must now be in errors.h (q.v.).
*/

int32 warncount=0, recovercount=0, errorcount=0;
/* The following 2 vars count suppressed errors/warns.  They needn't    */
/* be printed on systems where this would be unusual.                   */
int32 xwarncount=0;
int32 syserr_behaviour = 0;
#ifndef NO_LISTING_OUTPUT
/* The following variables should only be ref'd if listingstream != NULL */
static char *errsaves;
static int32 errsaven;
static int32 errsavep;
#endif

/* VERY HACKED - INTEGRATE THESE CHANGES */


static struct uncompression_record
{
    char *pointer;
    char compressed;
    unsigned char height;
#ifdef COMPRESSED_MESSAGES
    char stack[MSGSTACKDEPTH];
#endif
} errmsg;

#ifdef COMPRESSED_MESSAGES

static void start_string_char(char *s) /* Compressed but still a string */
{
    errmsg.height = 0;
    errmsg.pointer = s;
    errmsg.compressed = 1;
}

static int fetch_string_char(void)
/*
 * This is the same code (pretty well) as pp_fetch_string_char() in
 * "pp.c", but having a separate version here keeps the module structure
 * of the compiler cleaner and only spends about 150 bytes.  It also
 * reserves the option of using different compression techniques for
 * error messages and built-in headers.
 */
{
    int c, k;
    if (errmsg.height == 0) c = *errmsg.pointer++;
    else c = errmsg.stack[--errmsg.height];
    for (;;)
    {   c &= 0xff;
        k = ecompression_info[c];
        if (k == c || errmsg.compressed == 0) return c;
/*
 * The genhdrs utility establishes the greatest possible depth needed in
 * this stack and arranges to define MSGTACKDEPTH suitably - thus no
 * run-time check for stack overflow is needed.
 */
        errmsg.stack[errmsg.height++] = k;
        c = k >> 8;
    }
}

static void unfetch_string_char(int ch)
{
    errmsg.stack[errmsg.height++] = ch;
}

#else

static void start_string_char(char *s)
{
    errmsg.pointer = s;
}

static int fetch_string_char(void)
{
    return *errmsg.pointer++;
}

static void unfetch_string_char(int ch)
{
    errmsg.pointer--;
    IGNORE(ch);
}

#endif

void sstart_string_char(char *s)
{
    errmsg.height = 0;
    errmsg.pointer = s;
    errmsg.compressed = 0;
}

static void nprintf(msg_t errcode, ...)
{
    va_list a;
#ifndef NLS
    char s[80];
    char *p = s;
    start_string_char(msg_lookup(errcode));
/*
 * Convert error code into a string so that it can be printed. I expect
 * all strings used to be less than 80 characters long.
 */
    while ((*p++ = fetch_string_char()) != 0);
#endif

    va_start(a, errcode);
#ifdef NLS
    msg_vfprintf(stderr, errcode, a);
#else
    _vfprintf(stderr, s, a);
#endif
    va_end(a);
}

void summarise(void)
{
    if (warncount || recovercount || errorcount ||
        (feature & FEATURE_VERBOSE))
    {
      /* The NLS here is very minimal. We only special case 0 or 1 errors
       * (sufficient for most Western European languages). Generalising
       * this to any language would require a reworking of the NLS system
       * to allow some form of parameterised message. (bleugh)
       */

#ifndef COMPILING_ON_MPW
      switch (warncount) {
      case 0: nprintf(misc_message_sum1_zero,curlex.fl.f); break;
      case 1: nprintf(misc_message_sum1_sing,curlex.fl.f); break;
      default:
        nprintf(misc_message_sum1(curlex.fl.f, (long)warncount));
        break;
      }
#else
      switch (warncount) {
      case 0: nprintf(misc_message_sum1_zero_mpw,curlex.fl.f); break;
      case 1: nprintf(misc_message_sum1_sing_mpw,curlex.fl.f); break;
      default:
        nprintf(misc_message_sum1_mpw(curlex.fl.f, (long)warncount));
        break;
      }
#endif
      if (xwarncount && !(feature & FEATURE_PCC))
        nprintf(misc_message_sum2, (long)xwarncount);

      switch (recovercount) {
      case 0: nprintf(misc_message_sum3_zero); break;
      case 1: nprintf(misc_message_sum3_sing); break;
      default:
        nprintf(misc_message_sum3,recovercount);
        break;
      }

      switch (errorcount) {
      case 0: nprintf(misc_message_sum5_zero); break;
      case 1: nprintf(misc_message_sum5_sing); break;
      default:
        nprintf(misc_message_sum5,errorcount);
        break;
      }
    }
}

#ifndef NO_LISTING_OUTPUT
static void check_error_buffer(void)
{   /* don't rely on ANSI realloc(NULL, ..) semantics */
    if (errsaves == NULL)
        errsaves = (char *)malloc((size_t)(errsaven = 1024));
    else if (errsavep > errsaven - 200)
        errsaves = (char *)realloc(errsaves, (size_t)(errsaven += 1024));
    if (errsaves == NULL)
    {   fclose(listingstream);
        listingstream = NULL;
        cc_fatalerr(misc_fatalerr_space1);
    }
}
#endif

static void announce(msg_t msg_reason, int32 line)
{
  char *reason;
  reason=msg_lookup(msg_reason);
#ifndef TARGET_IS_UNIX
#  ifndef COMPILING_ON_MPW
    nprintf(misc_message_lineno(curlex.fl.f, (long)line, reason));
#  else
    nprintf(misc_message_lineno_mpw(curlex.fl.f, (long)line, reason));
#  endif
#else
    nprintf(misc_message_lineno_unix(curlex.fl.f, (long)line, reason));
#endif
#ifndef NO_LISTING_OUTPUT
    if (listing())
    {   check_error_buffer();
        if (list_this_file)
            msg_sprintf(&errsaves[errsavep], misc_message_announce, reason);
        else
            msg_sprintf(&errsaves[errsavep], misc_message_announce1,
                    curlex.fl.f, (long)line, reason);
        errsavep += strlen(&errsaves[errsavep]);
#ifdef FOR_ACORN
        throwback_idx = errsavep;
#endif
    }
#endif
}

void listing_diagnostics(void)
{
#ifndef NO_LISTING_OUTPUT
    if (listingstream && errsavep != 0)
    {   fprintf(listingstream, "%s", errsaves);
        errsavep = 0;
    }
#endif
}

static void errprintf(char const *s, ...)
{
  /* Takes a format, not a tag */
    va_list a;
    va_start(a, s);
    _vfprintf(stderr, s, a);
    va_end(a);
#ifndef NO_LISTING_OUTPUT
    if (listing())
    {   check_error_buffer();
        va_start(a, s);
#ifdef FOR_ACORN
        /* there is no _vsprintf() in Acorn's shared library stub...    */
        vsprintf(&errsaves[errsavep], s, a);
#else   /* ...use integer-only variant... which IS exported from armlib */
        /* or, for non-ARM hosts, is #defined in host.h to be vsprint.  */
        _vsprintf(&errsaves[errsavep], s, a);
#endif
        errsavep += strlen(&errsaves[errsavep]);
        va_end(a);
    }
#endif
}

static void qprints(char const *s)
{   /* used to print symbols (quoted) or syntactic categories */
    if (s[0] == '<' && isalpha(s[1])) errprintf("%s", s);
    else errprintf("'%s'", s);
}

/* Export for more general use soon (e.g. asm.c).                       */
static void esccharname(char *s, int ctarget)   /* s shall be char[>=5] */
{   int c;
    switch (c = char_untranslation(ctarget))
    {
default:
        if (isprint(c)) s[0] = c, s[1] = 0;
        else _sprintf(s, "\\x%.2x", ctarget);
        return;
case '\\':
case '\'':
case '\"': break;
case '\a': c = 'a'; break;
case '\b': c = 'b'; break;
case '\f': c = 'f'; break;
case '\n': c = 'n'; break;
case '\r': c = 'r'; break;
case '\t': c = 't'; break;
case '\v': c = 'v'; break;
case 0:    c = '0'; break;
    }
    s[0] = '\\', s[1] = c, s[2] = 0;
}

static void escstring(char *d, int32 dl, char const *s, int32 sl)
{   /* Used to print strings escaping chars as needed.                  */
    /* Note that s==DUFF_ADDR is OK if sl==0.                           */
    int32 si, di;
    for (si = di = 0; si<sl && di<dl-8; si++)
    {   esccharname(&d[di], s[si]);
        di += strlen(&d[di]);
    }
    if (si<sl) strcpy(&d[di], "...");
}

/* separate functions for names of types and of storage classes,
   since they now use distinct bitmaps.
*/
static char *xstgbit_name(SET_BITMAP stg)
{  AEop s;
   for (s = s_auto; bitofstg_(s) & STGBITS; s++)
       if (stg & bitofstg_(s)) return sym_name_table[s];
   return "???";
}

static char *xtypebit_name(SET_BITMAP typespec)
{  AEop s;
   /* Hack: the next line helps printing 'long int', but not much else.   */
   if (typespec & bitoftype_(s_long))
   {   if (typespec & bitoftype_(s_short))
          typespec ^= bitoftype_(s_longlong) ^ bitoftype_(s_int) ^
                     (bitoftype_(s_long) | bitoftype_(s_short));
       typespec &= ~bitoftype_(s_int);
   }
   for (s = s_bool; s < s_bool + NUM_OF_TYPES; s++)
       if (typespec & bitoftype_(s)) return sym_name_table[s];
   return "???";
}

static char const *xtype_name(TypeExpr const *e)
{   switch (h0_(e))
    {   case s_typespec:
            if (typespecmap_(e) & ENUMORCLASSBITS)
                return symname_(tagbindsym_(typespectagbind_(e)));
            return xtypebit_name(typespecmap_(e));
        case t_ovld: return "<overloaded function>";
        case t_fnap: return "<function>";
        case t_content: return "<pointer>";
        case t_subscript: return "<array>";
        case t_ref: return "<reference>";
        default: return "???";
    }
}

#ifdef  FOR_ACORN
#ifndef PASCAL
#ifndef FORTRAN
int cplusplus_flag;
#define input_from_cfront  (cplusplus_flag != 0)
#endif
#endif
#endif

static void printsym(char const *format, Symstr const *sym)
{   char const *name = symname_(sym);
#ifdef input_from_cfront
    static char *sbuf;
    if (input_from_cfront)
    {   if (!sbuf) sbuf = malloc(MAXDBUF);
        demangle(name, sbuf);
        name = sbuf;
    }
#endif
    errprintf(format, name);
    if (LanguageIsCPlusPlus)
    {   char buf[256];
        unmangle(name, buf, sizeof(buf));
        if (strcmp(name, buf) != 0) errprintf(" [%s]", buf);
    }
}

static void printparents(TagBinder const *b)
{   if (b == 0) return;
    printparents(b->tagparent);
    if (!isgensym(tagbindsym_(b)))
        errprintf("%s::", symname_(tagbindsym_(b)));
}

static void printbinder(Binder const *b)
{   if (b == 0) { errprintf("<nullbinder>"); return; }
    if (h0_(b) == s_binder || h0_(b) == s_member) {
        printparents(bindparent_(b)); /* for C, always NULL */
        printsym("%s", bindsym_(b));
    } else if (h0_(b) == s_tagbind) {
        printparents(((TagBinder const *)b)->tagparent);
        printsym("%s", bindsym_(b));
    } else
        printsym("'%s'", bindsym_(b));
}

#ifdef NLS

/* This is a version of va2type that supports the Compiler's '$' specifiers.
 * $<n>$<f> is supported, but never $<n>$<width etc.><f>
 */
static nls_type *va2type(const char *format,va_list args)
{
  static nls_type result[9];
  char fmt[9],type[9];
  int i,arg;
  const char *t,*s;

  for (i=0;i<9;i++) type[i]='\0';

  i=0;
  t=strchr(format,'%'); s=strchr(format,'$');
  while (t || s) {
    if (s==NULL || (t && t<s)) {
      t++;
      if (*t!='\0' && *t!='%') {
        int arg=-1;

        if (t[1]=='$' && *t!='0' && isdigit(*t)) {
          arg=*t-'1';
          t+=2;                 /* <n>$ */
        }

        while (*t=='-' || *t=='+' || *t==' ' || *t=='#') t++;
        if (*t=='*') {
          t++;
          if (t[1]=='$' && *t!='0' && isdigit(*t)) {
            type[*t-'1']='*';
            t+=2;               /* *<n>$ */
          } else type[i++]='*';
        } else
          while (isdigit(*t)) t++;
        if (*t=='.') {
          t++;
          if (*t=='*') {
            t++;
            if (t[1]=='$' && *t!='0' && isdigit(*t)) {
              type[*t-'1']='*';
              t+=2;             /* *<n>$ */
            } else type[i++]='*';
          } else
            while (isdigit(*t)) t++;
        }
        /* t now points at the format argument, or a preceeding 'l' */
        if (arg<0) arg=i++;
        if (*t=='l') type[arg]=*t++; /* 'l' */
        else type[arg]='%';
        fmt[arg]=*t;
      } else if (*t=='%') {
        t++;
      }

      t=strchr(t+1,'%');        /* Goto next percent */
    } else {                    /* s<t */
      int j=i;

      s++;

      if (isdigit(*s) && *s!='0' && s[1]=='$') {
        arg=s[0]-'1';
        s+=2;                   /* <n>$ */
      } else {
        arg=i++;                /* Doesn't allow $*<f> etc. */
      }

      switch (*s) {
      case '\0': break;
      case '$': case 'l':       /* $l takes no argument */
      default:                  /* really an error */
        s++; i=j;
        break;
      case 's': case 'q': case 'r': case 'e':
      case 'b': case 'c': case 't': case 'g':
      case 'm':
        fmt[arg]=*s;
        type[arg]='$';
        break;
      }

      s=strchr(s+1,'$');
    }
  }

  for (i=0;type[i] && i<9;i++) {
    switch (type[i]) {
    case 'l':                   /* long % */
      switch (fmt[i]) {
      case 'd': case 'i': case 'o': case 'u':
      case 'x': case 'X':
        result[i].cardinal=va_arg(args,long);
        break;
      case 'p': case 's': case 'n':
        result[i].pointer=va_arg(args,void *);
        break;
      case 'f': case 'e': case 'E': case 'g':
      case 'G':
        result[i].floating=va_arg(args,double);
        break;
      case 'c':
        result[i].cardinal=va_arg(args,int);
        break;
      }
      break;
    case '%':
      switch (fmt[i]) {
      case 'd': case 'i': case 'o': case 'u':
      case 'x': case 'X': case 'c':
        result[i].cardinal=va_arg(args,int);
        break;
      case 'p': case 's': case 'n':
        result[i].pointer=va_arg(args,void *);
        break;
      case 'f': case 'e': case 'E': case 'g':
      case 'G':
        result[i].floating=va_arg(args,double);
        break;
      }
      break;
    case '$':
      switch (fmt[i]) {
      case 's':
        result[i].cardinal=va_arg(args,AEop);
        break;
      case 'g': case 'm':
        result[i].cardinal=va_arg(args,SET_BITMAP);
        break;
      case 'q': case 'r': case 'e': case 'b':
      case 'c': case 't':
        result[i].pointer=va_arg(args,void *);
        break;
      }
      break;
    case '*':
      result[i].cardinal=va_arg(args,int);
      break;
    }
  }

  return result;
}

#endif


void ssuperrprintf(va_list a)
{
    /* This routine behaves like printf but also recognises escape chars of
     * the form $<char> and acts upon them.   It writes its output to
     * stderr, and if listing-file!= NULL also to a buffer associated with
     * the listing file.
     */

#ifdef NLS
    nls_type *m=va2type(errmsg.pointer,a);
    int cur_arg_n=0;
#endif

    for (;;)
    {
#define no_arg_type     0
#define int_arg_type    2
#define ptr_arg_type    4
#define no_qualifier    0
#define long_qualifier  8
#define star_qualifier 16   /* indirection for field width (yuk) */
        int arg_type = no_arg_type, w;
        void *pnt;
        long l_int;
#ifdef NLS
        nls_type *cur_arg=NULL,*star_arg;
#endif

        int ch;
        char v[80+12];      /* Buffer for segment of the format string */
        int n = 0;          /* 0..91 */

        /* Characters are moved to the buffer up as far as a $ or % escape */
        while ((ch = fetch_string_char()) != 0 &&
               ch != '$' &&
               n != 80 &&       /* also break at char 80... */
               ch != '%') v[n++] = ch;

        if (ch == '%')
        {
/* It is assumed here that there will never be more than 11 characters in */
/* an escape sequence (e.g. %-20.20l#x has 10 chars in it and seems a     */
/* bit excessive. The limit is because of overflow in the buffer v[]      */
            v[n++] = ch;
#ifdef NLS
            if (isdigit(*errmsg.pointer) &&
                errmsg.pointer[1]=='$') { /* nl_ extension */
              cur_arg=&m[fetch_string_char()-'1'];
              (void)fetch_string_char();
            }
#endif

#ifdef NLS
#  define ARG(TYPE,X) (TYPE)((cur_arg ? cur_arg : &m[cur_arg_n++])-> ## X)
#else
#  define ARG(TYPE,X) va_arg(a,TYPE)
#endif

            for (;;)
            {   ch = fetch_string_char();
                if (n == 91) syserr(syserr_bad_fmt_dir);
                v[n++] = ch;
                switch (safe_tolower(ch))
                {
        case 'l':   arg_type |= long_qualifier;
                    continue;
        case '*':   arg_type |= star_qualifier;
#ifdef NLS
                    if (isdigit(*errmsg.pointer) &&
                        errmsg.pointer[1]=='$') {
                      star_arg=&m[fetch_string_char()-'1'];
                      (void)fetch_string_char();
                    } else {
                      star_arg=&m[cur_arg_n++];
                    }
#endif
                    continue;
        /* Note that 'h' indicates that an int should be treated as a */
        /* short value, but the va_arg() call still fetches an int.   */
        default:    continue;
        case '%':   break;
        case 'c': case 'd': case 'i': case 'o':
        case 'u': case 'x':
                    arg_type |= int_arg_type;
                    break;
        case 'p': case 's':
                    arg_type |= ptr_arg_type;
                    break;
        case 'e': case 'f': case 'g':   /* disallow floating point here */
        case 'n': case 0:
                    syserr(syserr_bad_fmt_dir);
                }
                break;
              }
            ch=fetch_string_char();
          }

        v[n] = 0;           /* terminate format string */

#ifndef NO_LISTING_OUTPUT
        if (listing()) check_error_buffer();
#endif
        switch (arg_type)
        {
    default:
    case no_arg_type:
#ifndef NO_LISTING_OUTPUT
            if (listing())
                errsavep += (_sprintf(&errsaves[errsavep], v),
                             strlen(&errsaves[errsavep]));
#endif
            _fprintf(stderr, v);
            break;

    case ptr_arg_type:
            pnt = ARG(void *,pointer);
#ifndef NO_LISTING_OUTPUT
            if (listing())
                errsavep += (_sprintf(&errsaves[errsavep], v, pnt),
                             strlen(&errsaves[errsavep]));
#endif
            _fprintf(stderr, v, pnt);
            break;

/* At present I am only supporting '*' qualifiers for use with string    */
/* printing (i.e. %.*s) - at the time of writing this code there is just */
/* one such format in errors.h - I might like to get rid of it by        */
/* achieving the same result some other way so that this extra mess here */
/* wrt format decoding could be discarded.                           ACN */
    case ptr_arg_type + star_qualifier:
#ifdef NLS
            w = star_arg->cardinal;
#else
            w = ARG(int,cardinal);
#endif
            pnt = ARG(void *,pointer);
#ifndef NO_LISTING_OUTPUT
            if (listing())
                errsavep += (_sprintf(&errsaves[errsavep], v, w, pnt),
                             strlen(&errsaves[errsavep]));
#endif
            _fprintf(stderr, v, w, pnt);
            break;

    case int_arg_type + long_qualifier:
            l_int = ARG(long,cardinal);
#ifndef NO_LISTING_OUTPUT
            if (listing())
                errsavep += (_sprintf(&errsaves[errsavep], v, l_int),
                             strlen(&errsaves[errsavep]));
#endif
            _fprintf(stderr, v, l_int);
            break;

    case int_arg_type:
            w = ARG(int,cardinal);
#ifndef NO_LISTING_OUTPUT
            if (listing())
               errsavep += (_sprintf(&errsaves[errsavep], v, w),
                            strlen(&errsaves[errsavep]));
#endif
            _fprintf(stderr, v, w);
            break;
        }

        if (ch == 0)
        {
            if (syserr_behaviour==3) abort();
            return;        /* Message now complete */
        }

        if (ch != '$') unfetch_string_char(ch);
        else {
          ch=fetch_string_char();
#ifdef NLS
          if (isdigit(ch) && *errmsg.pointer=='$') {
            cur_arg=&m[ch-'1'];
            (void)fetch_string_char();
            ch = fetch_string_char();
          }
#endif

          switch (ch) {
          case 0:
            return;
          case 's':
            qprints(sym_name_table[ARG(AEop,cardinal) & 255]);
            break;
          case 'l':   /* current lexeme */
            switch (curlex.sym & 255)
            {   case s_integer:
                    errprintf("'%ld'", (long)curlex.a1.i); break;
                case s_floatcon:
                    errprintf("'%s'", curlex.a1.fc->floatstr); break;
                case s_identifier:
                case s_pseudoid:
                    printsym("'%s'", curlex.a1.sv);
                    break;
                case s_string:
                    {   char e[80];
                        escstring(e, 20, curlex.a1.s, curlex.a2.len);
                        errprintf("'\"%s\"'", e);
                    }
                    break;
                default:
                    qprints(sym_name_table[curlex.sym & 255]);
                    break;
            }
            break;
          case 'q':
          case 'r':
            {   Symstr *r = ARG(Symstr *,pointer);
                if (r==0 || h0_(r) != s_identifier)
                {   if (r == 0) errprintf("<missing>");
                    else errprintf("<oddity>");
                }
                else if (ch == 'r')
                    printsym("'%s'", r);
                else
                    printsym("%s", r);
            }
            break;
    case 'e':
            {   Expr *e = ARG(Expr *,pointer);
                while (h0_(e) == s_invisible) e = arg1_(e);
                if (h0_(e) != s_binder && h0_(e) != s_member)
                {   AEop op = h0_(e);
                    if (op == s_string
#ifdef EXTENSION_UNSIGNED_STRINGS
                        || op == s_ustring || op == s_string
#endif
                       )
                    {   char s[80];
                        StringSegList *z = ((String *)e)->strseg;
                        escstring(s, 60, z->strsegbase, z->strseglen);
                        errprintf("'\"%s\"'", s);
                    }
                    else
                    {   char *opname = sym_name_table[op & 255];
                        if (isdiad_(op))
                            errprintf("<expr> %s <expr>", opname);
                        else if (op >= s_binder)
                            errprintf("%s<expr>", opname);
                        else
                            errprintf("'%s'", opname);
                    }
                }
                else
                    printbinder((Binder *)e);
            }
            break;
    case 'b':   /* ordinary binder, but works for tag binder too.       */
            printbinder(ARG(Binder *,pointer));
            break;
    case 'c':   /* tag binder -- this will simplify many err msgs.      */
            {   TagBinder *b = ARG(TagBinder *,pointer);
                if (b == 0) break;
                errprintf("%s ", sym_name_table[tagbindsort(b)]);
                printbinder((Binder *)b);
            }
            break;
    case 't':
            qprints(xtype_name(ARG(TypeExpr *,pointer)));
            break;
    case 'g':
            errprintf("'%s'", xstgbit_name(ARG(SET_BITMAP,cardinal)));
            break;
    case 'm':
            errprintf("'%s'", xtypebit_name(ARG(SET_BITMAP,cardinal)));
            break;
    default:
            errprintf("$%c", (int)ch);   /* really an error */
            break;
          }
        }
      }
  }

static void superrprintf(msg_t errorcode, va_list a)
{
    start_string_char(msg_lookup(errorcode));
    ssuperrprintf(a);
}

void cc_msg(char *s, ...)
{
    va_list a;
    va_start(a, s);
    sstart_string_char(s); ssuperrprintf(a);
    va_end(a);
}

#ifdef NLS
void cc_msg_lookup(msg_t s, ...)
{
    va_list a;
    va_start(a, s);
    sstart_string_char(msg_lookup(s)); ssuperrprintf(a);
    va_end(a);
}
#endif

void syserr(syserr_message_type errorcode, ...)
{
    va_list a;
#ifdef NUMERIC_SYSERR_CODES     /* Incompatible with NLS? */
    char s[48];
/*
 * syserr codes are listed in the file errors.h, and are just
 * numeric to save space in the compiler.  Furthermore at present the
 * extra args are treated by printing two of them as hex values -
 * if less than two args were passed this will display junk!
 */
    _sprintf(s, "maintainer-info(errors.h,%d,%%.8x,%%.8x)", errorcode);
#else
    char *s = errorcode;
#endif
    announce(misc_message_fatal_internal, curlex.fl.l);
    va_start(a, errorcode);
    sstart_string_char(s); ssuperrprintf(a);
    va_end(a);
    errprintf("\n");
    switch (syserr_behaviour)
    {   case 1: return;         /* Try to go on with compilation */
        case 2: abort();        /* hard stop - would like diagnostics */
                break;
        default:                /* stop tolerably quietly and tidily */
#ifndef TARGET_IS_INTERPRETER
                va_start(a, errorcode);
                superrprintf(misc_disaster_banner, a);  /* no escapes */
                va_end(a);
#endif
                compile_abort(0);
                break;
    }
    exit(EXIT_syserr);
}

void cc_fatalerr_l(int32 n, msg_t errorcode, va_list a)
{
    announce(misc_message_fatal, n);
    superrprintf(errorcode, a);
    errprintf(msg_lookup(misc_message_abandoned));
    if (syserr_behaviour)
    {   show_store_use();
        syserr(syserr_syserr);  /* rethink this msg if syserrs lack strings */
    }
    compile_abort(-1);  /* (-1) is not a signal number used by signal() */
}

void cc_fatalerr(msg_t errorcode, ...)
{
    va_list a;
    va_start(a, errorcode);
    cc_fatalerr_l(curlex.fl.l, errorcode, a);
    va_end(a);
}

void cc_warn_l(int32 n, msg_t errorcode, va_list a)
{
    if (!(feature & FEATURE_NOWARNINGS))
    {
        ++warncount;
        announce(misc_message_warning, n);
        superrprintf(errorcode, a);
        errprintf("\n");
#ifdef FOR_ACORN
        if (dde_throwback_flag != 0)
            dde_throwback_send(THROWBACK_WARN, n, &errsaves[throwback_idx]);
#endif
    }
}

void cc_warn(msg_t errorcode, ...)
{
    va_list a;
    va_start(a, errorcode);
    cc_warn_l(curlex.fl.l, errorcode, a);
    va_end(a);
}

void cc_ansi_warn(msg_t errorcode, ...)
/* called to issue a warning that's suppressed in pcc mode */
{
    va_list a;
    if (feature & FEATURE_PCC) return;
    va_start(a, errorcode);
    cc_warn_l(curlex.fl.l, errorcode, a);
    va_end(a);
}

void cc_pccwarn(msg_t errorcode, ...)
/* This counts as a warning in pcc mode, but a recoverable error in */
/* ANSI mode.  Hence it should ONLY be called when the compiler     */
/* can claim to repair the construct. In PCC mode ONLY the warning  */
/* can be suppressed using the -w option on the command line.       */
{
    va_list a;
    if (feature & FEATURE_PCC)
    {   if (feature & FEATURE_NOWARNINGS) return;
        ++warncount;
        announce(misc_message_warning, (int32)curlex.fl.l);
    }
    else
    {   ++recovercount;
        announce(misc_message_error, (int32)curlex.fl.l);
    }
    va_start(a, errorcode);
    superrprintf(errorcode, a);
    va_end(a);
    errprintf("\n");
#ifdef FOR_ACORN
    if (dde_throwback_flag!= 0)
        dde_throwback_send(
            (feature & FEATURE_PCC) ? THROWBACK_WARN : THROWBACK_ERROR,
                curlex.fl.l, &errsaves[throwback_idx]);
#endif
}

void cc_rerr_l(int32 n, msg_t errorcode, va_list a)
{
    ++recovercount;
    announce(misc_message_error, n);
    superrprintf(errorcode, a);
    errprintf("\n");
#ifdef FOR_ACORN
    if (dde_throwback_flag != 0)
        dde_throwback_send(THROWBACK_ERROR, n, &errsaves[throwback_idx]);
#endif
}

void cc_rerr(msg_t errorcode, ...)
{
    va_list a;
    va_start(a, errorcode);
    cc_rerr_l(curlex.fl.l, errorcode, a);
    va_end(a);
}


void cc_ansi_rerr(msg_t errorcode, ...)
/* called to output an ANSI mode recoverable error mesage */
/*  which is always suppressed in -pcc mode.              */
{
    va_list a;
    if (feature & FEATURE_PCC) return;
    va_start(a, errorcode);
    cc_rerr_l(curlex.fl.l, errorcode, a);
    va_end(a);
}

void cc_rerr_cppwarn(msg_t errorcode, ...)
{   /* recoverable error for C; warning for C++ */
    va_list a;
    va_start(a, errorcode);
    if (!LanguageIsCPlusPlus)
      cc_rerr_l(curlex.fl.l, errorcode, a);
    else
      cc_warn_l(curlex.fl.l, errorcode, a);
    va_end(a);
}

void cc_rerr_cwarn(msg_t errorcode, ...)
{   /* recoverable error for C++; warning for C */
    va_list a;
    va_start(a, errorcode);
    if (LanguageIsCPlusPlus)
      cc_rerr_l(curlex.fl.l, errorcode, a);
    else
      /* what does PCC mode want here?  Warn like most PCC's         */
      cc_warn_l(curlex.fl.l, errorcode, a); /* ANSI C says OK.     */
    va_end(a);
}

void cc_err_l(int32 n, msg_t errorcode, va_list a)
{
    if (++errorcount > 100) cc_fatalerr(misc_fatalerr_toomanyerrs);
    announce(misc_message_serious, n);
    superrprintf(errorcode, a);
    errprintf("\n");
#ifdef FOR_ACORN
    if (dde_throwback_flag != 0)
        dde_throwback_send(THROWBACK_SERIOUS, n, &errsaves[throwback_idx]);
#endif
}

void cc_err(msg_t errorcode, ...)
{
    va_list a;
    va_start(a, errorcode);
    cc_err_l(curlex.fl.l, errorcode, a);
    va_end(a);
}

void flt_report_error(int failure) {
    switch (failure) {
    default:               syserr("flt_report_error");
    case flt_very_small:   cc_warn(fp_rerr_very_small); break;
    case flt_very_big:     cc_rerr(fp_err_very_big); break;
    case flt_big_single:   cc_rerr(fp_err_big_single); break;
    case flt_small_single: cc_warn(fp_rerr_small_single); break;
    }
}

void errstate_init(void)
{
    /* reset the following vars for each one of a many file compilation */
    warncount=0, recovercount=0, errorcount=0;
    xwarncount=0;
/* The next line is scrappy, but avoids printing the null string.       */
    curlex.fl.f = "<command line>", curlex.fl.l = 1;
#ifndef NO_LISTING_OUTPUT
    errsaves = NULL;
    errsaven = errsavep = 0;
#endif
}

/* end of mip/misc.c */
