/*
 * C compiler file compiler.c
 * Copyright (C) Codemist Ltd., 1987-1992.
 * Copyright (C) Acorn Computers Ltd., 1988-1990.
 * Copyright (C) Advanced RISC Machines Limited, 1990-1992.
 */

/*
 * RCS $Revision: 1.77 $  Codemist 147
 * Checkin $Date: 1996/01/10 14:54:27 $
 * Revising $Author: hmeeking $
 */

/* AM Mar 89: start 'ccom-like' interface.  I allow the compiler to produce */
/* both object and asm output (after all unix ccom cannot never allow       */
/* obj output!).  Fix up FLG_STDIN to allow 0 args.  Add '-' for stdin/out  */
/* Move compile_abort to here from misc.c -- move out to driver.c?          */

#include <stddef.h>
#include <time.h>
#ifdef __STDC__
#  include <stdlib.h>
#  include <string.h>
#else
#  include <strings.h>
#endif

#include <ctype.h>
#ifndef COMPILING_ON_MSDOS
#include <signal.h>
#endif

#include "globals.h"
#include "compiler.h"
#include "fname.h"
#include "pp.h"
#include "lex.h"
#include "syn.h"
#include "sem.h"      /* init */
#include "bind.h"     /* init */
#include "builtin.h"  /* init */
#include "vargen.h"   /* initstaticvar */
#include "aetree.h"
#include "cg.h"
#include "mcdep.h"
#include "aeops.h"
#include "xrefs.h"
#include "store.h"
#include "version.h"            /* for CC_BANNER */
#include "errors.h"
#ifdef TARGET_IS_KCM
#include "target.h"
#endif
#if defined(FOR_ACORN) && defined(COMPILING_ON_RISCOS)
#include "dde.h"
#endif

#ifndef COMPILING_ON_MVS
#  define BSD_LIKE_SEARCH 1     /* ansi trying to ban (like sysV/K&R) */
#endif

#ifdef  ENABLE_MAPSTORE
extern void _mapstore(void);
#endif

#define  MAX_NAME     256  /* The longest file name I'm prepared to handle */

#define  ccom_alloc(n)  GlobAlloc(SU_Other, n)

#define  INSTORE_FILE   2                     /* used in PathElement.flags */
#define  PE_USER        4                     /* used in PathElement.flags */
#define  PE_SYS         8                     /* used in PathElement.flags */

typedef struct PathElement
{   struct PathElement *link;      /* sometimes a stack; sometimes a queue */
    int    flags;
    char   name[2];         /* to allow for a trailing separator and a NUL */
} PathElement;

/* AM/LDS: beware: the stack path_hd is assumed always to be non-empty, and  */
/* the first element is treated very specially (in BSD).  Maybe it should be */
/* a separate variable.                                                      */
static PathElement *path_hd, *path_tl, *path_sav;

/* @@@ AM: I would prefer to use listingfile = NULL/string instead of      */
/* FLG_LISTING etc.                                                        */
#define FLG_COMPILE                    1             /* various ccom_flags */
#define FLG_LISTING                    2
#define FLG_MAKEFILE                   4
#define FLG_PREPROCESS                 8
#define FLG_NO_OBJECT_OUTPUT          16
#define FLG_INSTORE_FILES_IMPLICITLY  32
#define FLG_STDIN                     64
#define FLG_COUNTS                   128
#define FLG_USE_SYSTEM_PATH          256
#define FLG_NOSYSINCLUDES            512

#define TEXT_FILE                      0
#define BINARY_FILE                    1
#define TEXT_FILE_APPEND               2          /* for Acorn RISC OS DDE */

static int ccom_flags;
time_t tmuse_front, tmuse_back;
bool host_lsbytefirst;
bool target_lsbitfirst;

#ifdef MIN_ALIGNMENT_CONFIGURABLE
int32 alignof_struct_val;
int32 alignof_toplevel_static_var;
#endif

/*
 * Define the following as global, for various debugger back-ends.
 */
char *sourcefile, *objectfile, *sourcemodule;

static const char *asmfile, *listingfile, *makefile;
/* system_flavour copes with enabling this compiler to rename synbols   */
/* to reflect libraries.  E.g. on BSD sprintf must be renamed to refer  */
/* to a different symbol from on ANSI (as their results differ).        */
static const char *system_flavour;
static FILE *makestream;

#ifdef COMPILING_ON_RISC_OS
#ifdef FOR_ACORN
static int makeflg = 0;
#endif

#include "riscos.h"

static void set_time_stamp(const char *file, bool good)
{   __riscos_osfile_block b;
    b.load = 0;
    /* OS_File(2, ...) sets the load address (type & ms part of time stamp) */
    /* OS_File(9, ...) sets the type to &FFD and the time stamp to now.     */
    __riscos_osfile((good ? 9 : 2), file, &b);
}

#endif

/* DEPEND_FORMAT used to output dependency line (-m option) */
#ifdef COMPILING_ON_MACINTOSH
#  define DEPEND_FORMAT     "%s\304\t%s\n"
#else
#  define DEPEND_FORMAT     "%s:\t%s\n"
#endif

/*
 * Abort compilation if major fault found.
 */

extern void driver_abort(char *message)
{
  cc_msg_lookup(driver_abort_msg, message);
  exit(EXIT_error);
}

static FILE *cc_open(const char *s, int mode)
{
    FILE *f;
    if (*s == 0 || strcmp(s, "-") == 0)
        f = (mode == BINARY_FILE) ? (s = "binary stdout", (FILE *)0) : stdout;
    else
        f = fopen(s, (mode == BINARY_FILE) ? FOPEN_WB :
                     (mode == TEXT_FILE_APPEND) ? "a" : "w");
    if (f == 0)
    {   char msg[MAX_NAME];
        msg_sprintf(msg, driver_couldnt_write, s);
        driver_abort(msg);
    }
    return f;
}

static void cc_close(FILE **fp, const char *file)
{
/* Be careful so that the following test always closes f when needed    */
/* but tests ferror before the close.  (There may be an I/O error, and  */
/* the file still be closeable in which case it must be closed!         */
/* Consider a floppy disc running out of space.)                        */
    FILE *f = *fp;
    int err;
    *fp = NULL;
    if (f != NULL && f != stdout && (err = ferror(f), fclose(f) || err))
        cc_fatalerr(compiler_fatalerr_io_error, file);
}

#ifndef TARGET_IS_INTERPRETER
extern void compile_abort(int sig_no)
{
/* pre-conditions: initialisation done, closing not done.  Call from      */
/* SIGINT handler at your peril!                                          */
#ifndef COMPILING_ON_MSDOS
    (void) signal(SIGINT, SIG_IGN);
#endif
#ifndef NO_ASSEMBLER_OUTPUT
    if (asmstream)
    {   cc_close(&asmstream, asmfile);
#ifndef COMPILING_ON_MVS
        if (asmfile[0]) remove(asmfile);
#endif
    }
#endif
#ifndef NO_LISTING_OUTPUT
    cc_close(&listingstream, listingfile);
#endif
    if (objstream)
    {   cc_close(&objstream, objectfile);
#ifndef COMPILING_ON_MVS
        remove(objectfile);
#endif
    cc_close(&makestream, makefile);
    }
    exit(sig_no == (-1) ? EXIT_fatal : EXIT_syserr);
}
#endif

/*
 * Enable compiler features
 */

#if 'A' == 193          /* HOST_USES_EBCDIC... */
extern char _etoa[];
#  define ASCII(x)      (_etoa[x])
#else
#  define ASCII(x)      (x)
#endif

#ifdef PASCAL /*ECN*/
/*
 * Disable run time checks
 */

static void rtcheck_set(char *opts)
{
  int opt;
  int32 new_rtcheck;

  for (opt = *opts; opt != 0; opt = *++opts) {
      new_rtcheck = 0;
      switch (safe_toupper(opt)) {
          case 'A': new_rtcheck = RTCHECK_ARRAY;     break;
          case 'C': new_rtcheck = RTCHECK_CASE;      break;
          case 'N': new_rtcheck = RTCHECK_NIL;       break;
          case 'R': new_rtcheck = RTCHECK_REFERENCE; break;
          case 'P': new_rtcheck = RTCHECK_ASSERT; break;
          case 'D': new_rtcheck = RTCHECK_DEADCODE;  break;
          default:  cc_warn(warn_option_r, opt);
      }
      rtcheck ^= new_rtcheck;
  }
}
#endif

static void feature_set(char *opts)
{
  int opt;
#define UNUSED 0
  static int32 feature_flags[] = {
        /* 'A' */ FEATURE_ANOMALY,
        /* 'B' */ FEATURE_VERBOSE,
        /* 'C' */ FEATURE_LIMITED_PCC,
        /* 'D' */ UNUSED,
        /* 'E' */ FEATURE_6CHARMONOCASE,
        /* 'F' */ 0L-FEATURE_SAVENAME,               /* N.B. -ve */
        /* 'G' */ UNUSED,
        /* 'H' */ FEATURE_PREDECLARE,
        /* 'I' */ FEATURE_USERINCLUDE_LISTING,
        /* 'J' */ FEATURE_SYSINCLUDE_LISTING,
        /* 'K' */ FEATURE_KANDR_INCLUDE,
        /* 'L' */ FEATURE_DONTUSE_LINKREG,
        /* 'M' */ FEATURE_PPNOUSE,
        /* 'N' */ FEATURE_SAVENAME,
        /* 'O' */ FEATURE_WARNOLDFNS,
        /* 'P' */ FEATURE_TELL_PTRINT,
        /* 'Q' */ FEATURE_ALLOWCOUNTEDSTRINGS,
        /* 'R' */ FEATURE_LET_LONGJMP_CORRUPT_REGVARS,
        /* 'S' */ FEATURE_ANNOTATE,
        /* 'T' */ UNUSED,
        /* 'U' */ FEATURE_UNEXPANDED_LISTING,
        /* 'V' */ FEATURE_NOUSE,
        /* 'W' */ FEATURE_WR_STR_LITS,
        /* 'X' */ UNUSED,
        /* 'Y' */ FEATURE_ENUMS_ALWAYS_INT,
        /* 'Z' */ FEATURE_INLINE_CALL_KILLS_LINKREG
    };
#undef UNUSED

  for (opt = *opts;  opt != 0;  opt = *++opts)
  {   int ch = safe_toupper(opt);
      if (ch >= 'A')
      {   int n = ASCII(ch) - ASCII('A');
          if (n < sizeof(feature_flags)/sizeof(int32))
          {   int32 flag = feature_flags[n];
              if (flag < 0)
                  feature &= ~-flag;
              else if (flag > 0)
                  feature |= flag;
              else
                  goto unknown;
          }
          else if (ch == 'X')
              suppress &= ~D_SUPPRESSED;
          else if (ch == 'Z')
              ccom_flags |= FLG_USE_SYSTEM_PATH;
          else
unknown:      cc_warn(warn_option_f, opt);
      }
  }
}

/*
 * Enable debugger support.
 */

#ifdef TARGET_HAS_DEBUGGER
static void usrdbg_set(char *opts)
{
  int opt = *opts;
  if (opt == 0)
    usrdbgmask = DBG_ANY;
  else
  {
    while (opt)
    {
      int new_option = 0;
      switch (safe_toupper(opt))
      {
#ifndef TARGET_IS_UNIX
case 'F': new_option = DBG_PROC;   break;
case 'L': new_option = DBG_LINE;   break;
case 'V': new_option = DBG_VAR;    break;
case 'P': new_option = DBG_PP;     break;
#endif
default:  new_option = DBG_ANY;
          cc_warn(warn_option_g, opt);
      }
      usrdbgmask |= new_option;
      opt = *++opts;
    }
  }
}
#endif

/*
 * Disable warning messages.
 */

static void disable_warnings(char *opts)
{
  int opt = *opts;
  if (opt == 0)
  {
      var_warn_implicit_fns   = 0;
      var_warn_deprecated     = 0;
      feature |= FEATURE_NOWARNINGS;
      suppress |= D_ASSIGNTEST | D_LOWERINWIDER | D_IMPLICITNARROWING |
                  D_IMPLICITVOID | D_IMPLICITCTOR | D_PPNOSYSINCLUDECHECK;
  }
  else
  { bool switchon = NO;
    while (opt)
    {
      int32 new_suppress = 0L;
      switch (safe_toupper(opt))
      {
case '+': switchon = YES;                       break;
case 'A': new_suppress = D_ASSIGNTEST;          break;
case 'I': new_suppress = D_IMPLICITCTOR;        break;
case 'L': new_suppress = D_LOWERINWIDER;        break;
case 'N': new_suppress = D_LOWERINWIDER|D_IMPLICITNARROWING; break;
case 'P': new_suppress = D_PPNOSYSINCLUDECHECK; break;
case 'V': new_suppress = D_IMPLICITVOID;        break;
case 'G': new_suppress = D_GUARDEDINCLUDE;      break;
case 'D': var_warn_deprecated = 0;              break;
case 'F': var_warn_implicit_fns = 0;            break;

default:  cc_warn(warn_option_w, opt);
      }
      if (switchon)
        suppress &= ~new_suppress;
      else
        suppress |= new_suppress;
      opt = *++opts;
    }
  }
}

#ifdef DISABLE_ERRORS
static void disable_errors(char *opts)      /* -eXYZ suppresses ERROR messages */
{ int opt = *opts;
  bool switchon = NO;
  while (opt)
  {
    int32 new_suppress = 0,
          new_feature = 0;
    switch (safe_toupper(opt))
    {
case 'C': new_suppress = D_IMPLICITCAST;   break;
case 'M': new_suppress = D_MPWCOMPATIBLE | D_PPALLOWJUNK | D_ZEROARRAY | D_PPNOSYSINCLUDECHECK;
          new_feature = FEATURE_ALLOWCOUNTEDSTRINGS;
                                           break;
case 'P': new_suppress = D_PPALLOWJUNK;    break;
#ifdef EXTENSION_VALOF
case 'V': new_suppress = D_VALOFBLOCKS;    break;
#endif
case 'Z': new_suppress = D_ZEROARRAY;      break;
case 'F': new_suppress = D_CAST;           break;  /* Force casts */
/* ECN - Suppress errors about linkage disagreements */
case 'L': new_suppress = D_LINKAGE;        break;
case 'A': new_suppress = D_ACCESS;         break;
default:  cc_warn(warn_option_e, opt);
    }
    if (switchon)
    {  suppress &= ~new_suppress;
       feature &= ~new_feature;
    } else {
      suppress |= new_suppress;
      feature |= new_feature;
    }
    opt = *++opts;
  }
}
#endif

/*
 * Pre-set #pragmas - simulate a #pragma -Xnnn.
 */

static void pragma_set(char *p)
{ /* N.B. the word 'pragma' is reserved by High-C (yuk).                */
  int pragmachar = *p++;
  pragmachar = safe_tolower(pragmachar);        /* beware re-evaluation */

  if (islower(pragmachar))
  { int32 value;
    int ch = *p;
    if (isdigit(ch))
    { value = strtol(p, NULL, 0);  /* allow 0x... form too */
#ifdef FORTRAN
      if ((pragmachar == 'x' || pragmachar == 'w') &&
           pp_pragmavec[pragmachar-'a'] != -1)
      { pp_pragmavec[pragmachar-'a'] |= value;
        return;
      }
#endif
    }
    else value = -1L;
    pp_pragmavec[pragmachar-'a'] = value;
  }
}

/*
 * Enable internal compiler diagnostics.
 *
 */

static void debug_set(char *opts)
{
  int opt;
  long debugmask = 0L;

  for (opt = *opts;  opt != 0;  opt = *++opts)
  {
    debugmask = 0L;
    switch (safe_toupper(opt))
    {
#ifdef ENABLE_AETREE
case 'A': ++aetree_debugcount;
          debugmask = DEBUG_AETREE;    break;
#endif
#ifdef ENABLE_BIND
case 'B': debugmask = DEBUG_BIND;      break;
#endif
#ifdef ENABLE_CSE
case 'C': cse_debugcount++;
          debugmask = DEBUG_CSE;       break;
#endif
#ifdef ENABLE_DATA
case 'D': debugmask = DEBUG_DATA;      break;
#endif
#ifdef ENABLE_TEMPLATE
case 'E': debugmask = DEBUG_TEMPLATE;  break;
#endif
#ifdef ENABLE_FNAMES
case 'F': debugmask = DEBUG_FNAMES;    break;
#endif
#ifdef ENABLE_CG
case 'G': debugmask = DEBUG_CG;        break;
#endif
#ifdef ENABLE_SPILL
case 'H': debugmask = DEBUG_SPILL;     break;
#endif
#ifdef ENABLE_FILES
case 'I': debugmask = DEBUG_FILES;     break;
#endif
#ifdef ENABLE_LOCALCG
case 'K': localcg_debugcount++;
          debugmask = DEBUG_LOCALCG;   break;
#endif
#ifdef ENABLE_LEX
case 'L': debugmask = DEBUG_LEX;       break;
#endif
#ifdef ENABLE_MAPSTORE
case 'M': debugmask = DEBUG_MAPSTORE;  break;
#endif
#ifdef ENABLE_OBJ
case 'O': debugmask = DEBUG_OBJ;       break;
#endif
#ifdef ENABLE_PP
case 'P': debugmask = DEBUG_PP;        break;
#endif
#ifdef ENABLE_Q
case 'Q': debugmask = DEBUG_Q;         break;
#endif
#ifdef ENABLE_REGS
case 'R': debugmask = DEBUG_REGS;      break;
#endif
#ifdef ENABLE_SYN
case 'S': debugmask = DEBUG_SYN;       break;
#endif
#ifdef ENABLE_TYPE
case 'T': debugmask = DEBUG_TYPE;      break;
#endif
#ifdef ENABLE_STORE
case 'U': debugmask = DEBUG_STORE;     break;
#endif
#ifdef ENABLE_2STORE
case 'W': debugmask = DEBUG_2STORE;    break;
#endif
#ifdef ENABLE_X
case 'X': debugmask = DEBUG_X;         break;
#endif
#ifdef ENABLE_LOOP
case 'Y': debugmask = DEBUG_LOOP;      break;
#endif
/*
 * -Qz to modify syserr behaviour is always available
 */
case 'Z': syserr_behaviour++;
#ifndef COMPILING_ON_MSDOS
#ifdef __CC_NORCROFT
          (void) signal(SIGINT, SIG_DFL);     /* permit NorCroft backtrace */
#endif
#endif
          break;
default:  cc_warn(warn_option_zq, opt);
    }
    sysdebugmask |= debugmask;
  }
}

/*
 * Enable the features for PCC style compilation.
 */

static int32 pcc_features(void)
{
  var_warn_implicit_fns   = 0;
  var_warn_deprecated     = 0;
  pp_preundefine("__STDC__");
  suppress |= D_ASSIGNTEST | D_IMPLICITNARROWING | D_IMPLICITVOID;
  return FEATURE_PCC | FEATURE_UNIX_STYLE_LONGJMP | FEATURE_SIGNED_CHAR;
}

static void translate_fname(const char *file, UnparsedName *un, char *new_file)
{   fname_parse(file, FNAME_INCLUDE_SUFFIXES, un);
    fname_unparse(un, FNAME_AS_NAME, new_file, MAX_NAME);
}

static void translate_path(const char *path, UnparsedName *un, char *new_path)
{   fname_parse(path, FNAME_INCLUDE_SUFFIXES, un);
    fname_unparse(un, FNAME_AS_PATH, new_path, MAX_NAME);
#if defined(FOR_ACORN) && defined(COMPILING_ON_RISC_OS)
    /* DDE fix for paths like foo: ... */
    if (un->un_pathlen > 1 && new_path[un->un_pathlen-2] == ':')
        new_path[un->un_pathlen-1] = 0;          /* kill trailing separator */
#endif
}

static PathElement *mk_path_element(PathElement *link, int flags, char *name)
{ PathElement *p;
  if (debugging(DEBUG_FILES))
    cc_msg("mk_path_element(%s)\n", name);
  p = (PathElement *) ccom_alloc((int32)strlen(name) + sizeof(PathElement));
  p->link = link;
  p->flags = flags;
  strcpy(p->name, name);
  return p;
}

/*
 * Set the user/sys include path (no difference for bsd!).
 */

static void set_include_path(const char *path, int flags)
{
  PathElement *p;
  UnparsedName unparse;
  int ch;
  char new_path[MAX_NAME];
  char path_element[MAX_NAME];

  for (ch = *path;  ch != 0;)
  {   /* one or more path elements joined by commas... */
      int length = 0;
      while (ch != ',' && ch != 0)
      {   if (length < (MAX_NAME-1)) path_element[length++] = ch;
          ch = *(++path);
      }
      if (ch == ',') ch = *(++path);
      path_element[length] = 0;
      if (length == 1 && path_element[0] == '-'    /* unix-like 'std place' */
#ifdef COMPILING_ON_ACORN_KIT
          /* backwards compatibility -- RISCOS only soon? */
          || strcmp(path_element, ":mem") == 0
          || strcmp(path_element, ":MEM") == 0
#endif
         )
      {
          flags |= INSTORE_FILE;  length = 0;
          ccom_flags &= ~FLG_INSTORE_FILES_IMPLICITLY;
          new_path[0] = 0;
      }
      else
          translate_path(path_element, &unparse, new_path);
      /* Add a new path element at the END of the list. */
      p = mk_path_element(NULL, flags, new_path);
      if (path_hd == NULL)
          path_tl = path_hd = p;
      else
          path_tl = (path_tl->link = p);
  }
}

/*
 * Stack include path name.
 */

static char *push_include(char *path, char *name)
{
  /* Return a copy of the native (translated) file-name. Do this so ASD    */
  /* will have a file-name it can use directly for its 'type' command.     */
  char *hostname = strcpy((char *)ccom_alloc((int32)strlen(name)+1L), name);
#ifdef BSD_LIKE_SEARCH
  { PathElement *p;
    UnparsedName unparse;
    char new_path[MAX_NAME];

    if (feature & FEATURE_KANDR_INCLUDE) return hostname;

    /* remove the head of the path and push it on the save stack */
    p = path_hd;
    path_hd = p->link;
    p->link = path_sav;
    path_sav = p;

    if (path != NULL)
    {   translate_fname(path, &unparse, new_path);
        new_path[unparse.un_pathlen] = 0;
    }
    else
        new_path[0] = 0;
    path_hd = p = mk_path_element(path_hd,
                                  /* @@@ next line flag PE_USER/SYS? */
                                  path == NULL ? INSTORE_FILE : 0,
                                  new_path);
  }
#endif
  return hostname;
}

static void pop_include(void)
{
#ifdef BSD_LIKE_SEARCH
    PathElement *p;
    if (feature & FEATURE_KANDR_INCLUDE) return;
    /*
     * pop the saved path element off the save stack and
     * push it on to the front of the regular path element stack.
     */
    p = path_hd;
    path_hd = path_sav;
    path_sav = path_sav->link;
    path_hd->link = p->link;
#endif
}

static void preprocess_only(void)
{
  int character;

#ifdef PASCAL /*ECN*/
  syn_init();
#endif

  if (ccom_flags & FLG_PREPROCESS)
  {   /* Selected if -E or -E -MD set */
      while ((character = pp_nextchar()) != PP_EOF) putchar(character);
  }
  else
  {   /* Selected if -M set */
      while ( (character = pp_nextchar()) != PP_EOF );
  }
}

#ifdef  FOR_ACORN
#ifndef PASCAL
#ifndef FORTRAN
int cplusplus_preprocessing(void)
{
    return (ccom_flags & FLG_PREPROCESS) && cplusplus_flag;
}
#endif
#endif
#endif

static void pre_include(char *file)
{   FILE *f;
    if ((f = fopen(file, "r")) != NULL)
    {   pp_notesource(file, f);
        preprocess_only();
    }
    else
        cc_warn(warn_preinclude, file);
}

/*
 * Initialise compiler state.
 */

static void set_debug_options(int argc, char *argv[])
{
  int count;
  for (count=1;  count < argc;  ++count)
  {   char *current = argv[count];
      if (current[0] == '-' && current[1] != 0)
          switch (safe_toupper(current[1]))
          {
#ifdef TARGET_HAS_DEBUGGER
      case 'G': usrdbg_set(current+2);        break;
#endif
      case 'Z': if (safe_toupper(current[2]) == 'Q') debug_set(current+3);
                break;
          }
  }
}

static int makeflag;

static void set_compile_options(int argc, char *argv[])
{
  int count, files = 0;
  char message[MAX_NAME];

   /* AM: the following code is intended to deal with things like          */
   /* __CLK_TCK (see <time.h>) being different for different targets.      */
   /* It probably needs better amalgamation into Acorn's code for predefs  */
#ifdef TARGET_PREDEFINES
    {   static char *predefs[] = TARGET_PREDEFINES;
        int i;
        for (i=0; i < sizeof(predefs)/sizeof(predefs[0]); i++)
            /* note that the arg may be of the form "name" or "name=toks" */
            /* the "name" form is equivalent to "name=1".                 */
            pp_predefine(predefs[i]);
    }
#endif

  for (count=1;  count < argc;  ++count)
  {   char *current = argv[count];
      if (current[0] == '-' && current[1] != 0)
      {
          switch (safe_toupper(current[1]))
          {
      case 'C': if (ccom_flags & FLG_PREPROCESS)
                    feature |= FEATURE_PPCOMMENT;
                else
                    ccom_flags |= FLG_NO_OBJECT_OUTPUT;
                break;

      case 'D': pp_predefine(current+2);      break;

      case 'E': /* do we wish to fault -Exxx more generally?            */
#ifdef DISABLE_ERRORS
                if (current[2])
                {   disable_errors(current+2);
                    break;
                }
#endif
#ifdef COMPILING_ON_MVS
                if (current[2])
                {   cc_warn(warn_option_E, current);
                    break;
                }
#endif
                ccom_flags = (ccom_flags | FLG_PREPROCESS) & ~FLG_COMPILE;
                break;

      case 'F': feature_set(current+2);       break;

      case 'G': break;  /* already done */

      case 'I': set_include_path(current+2, PE_USER);  break;

      case 'J': ccom_flags &= ~FLG_INSTORE_FILES_IMPLICITLY;
                set_include_path(current+2, PE_SYS);  break;

      case 'K': ccom_flags |= FLG_COUNTS;
                break;

#ifndef NO_LISTING_OUTPUT
      case 'L': ccom_flags |= FLG_LISTING;
                listingfile = current + 2;
                break;
#endif

      case 'M': ccom_flags |= FLG_MAKEFILE;
                if (current[2] == '<' || current[2] == 0)
                {   ccom_flags &= ~(FLG_COMPILE+FLG_NOSYSINCLUDES);
                    if (current[2] == '<') ccom_flags |= FLG_NOSYSINCLUDES;
                }
                else if (current[2] == '+')
                {   makefile = current+3;
                    break;
                }
                else
                    makefile = current+2;
                makeflag = 0;
                break;

      case 'O': /*
                 * Hum, really we should never get here because this
                 * this should be done by -zpz1.
                 */
                break;

      case 'P': { int ch = safe_toupper(current[2]);
                  int32 profile = 1L;
                  if (ch == 0)
                    /* do nothing */;
                  else if (ch == 'G' || ch == 'X' && current[3] == 0)
                    profile = 2L;
                  else
                    cc_warn(warn_option_p, current);
                  var_profile_option = profile;
                }
                break;

      case 'R':
#ifdef PASCAL /*ECN*/
                rtcheck_set(current+2);
#else
                feature &= ~FEATURE_WR_STR_LITS;
#endif
                break;

      case 'S': asmfile = current+2;  /* "" for -S */
#ifdef COMPILING_ON_MVS
                /* pesky cc163 compatibility */
                if (*asmfile == 0 && count+1<argc)
                    asmfile = argv[++count];
#endif
                break;

      case 'U': pp_preundefine(current+2);    break;

      case 'W': disable_warnings(current+2);  break;

#ifdef TARGET_IS_MVS    /* TARGET_USES_CCOM_INTERFACE ??? */
/* The following case provides -Xcsectname for CC 163 compatibility.    */
/* It may be that the CCOM-style interface should stuff ALL unknown     */
/* options to mcdep_config_option()?                                    */
      case 'X': if (!mcdep_config_option(current[1], current+2))
                    cc_warn(warn_option, current);
                break;
#endif

      case 'Z': switch(safe_toupper(current[2]))
                {
#ifdef MIN_ALIGNMENT_CONFIGURABLE
          case 'A': if (safe_toupper(current[3]) == 'S' &&
                        isdigit(current[4])) {
                        alignof_struct_val = current[4] - '0';
                        break;
                    } else if (safe_toupper(current[3]) == 'T' &&
                        isdigit(current[4])) {
                        alignof_toplevel_static_var = current[4] - '0';
                        break;
                    }
                    goto check_mcdep;
#endif
          case 'B': target_lsbitfirst = !target_lsbytefirst;
                    if (isdigit(current[3]))
                        target_lsbitfirst = (current[3]-'0') != 0;
                    break;
#ifdef TARGET_ENDIANNESS_CONFIGURABLE
          case 'E': { int lsbytefirst = current[3] - '0';
                      if (lsbytefirst)
                          config &= ~CONFIG_BIG_ENDIAN;
                      else
                          config |= CONFIG_BIG_ENDIAN;
                      target_lsbitfirst = lsbytefirst != 0;
                    }
                    break;
#endif
          case 'C': feature |= FEATURE_SIGNED_CHAR;
                    break;
          case 'F': feature = (feature | FEATURE_FUSSY) & ~FEATURE_LIMITED_PCC;
                    break;
          case 'I': pre_include(current+3);
                    break;
          case 'J': config |= CONFIG_INDIRECT_SETJMP;  /* related to -fR */
                    break;
          case 'P': pragma_set(current+3);
                    break;
          case 'O': feature |= FEATURE_AOF_AREA_PER_FN;
                    break;
          case 'Q': break;  /* already done */
#ifndef TARGET_IS_HELIOS
          case 'S': system_flavour = current+3;
                    break;
#endif
          case 'U': feature |= pcc_features();
                    feature &= ~(FEATURE_ANSI|FEATURE_CPP|FEATURE_CFRONT);
                    break;
          case 'Y': feature |= FEATURE_ANSI;
                    feature &= ~(FEATURE_PCC|FEATURE_CPP|FEATURE_CFRONT);
                    break;
#ifdef CPLUSPLUS
          case 'Z': feature |= FEATURE_CFRONT;
                    /* drop through */
          case 'X': feature |= FEATURE_CPP;
                    feature &= ~(FEATURE_PCC|FEATURE_ANSI);
                    break;
#endif
#ifdef PASCAL
          case 'Z': feature |= FEATURE_ISO;
                    break;
#endif
          case 'T': config |= (current[3] == '+') ? CONFIG_OPTIMISE_TIME :
                                                    CONFIG_OPTIMISE_SPACE;
          break;
          check_mcdep:
          default:  if (!mcdep_config_option(current[2], current+3))
                        cc_warn(warn_option, current);
                    break;
                }
                break;
      default:  cc_msg(msg_lookup(warn_option), current);
                break;
          }
      }
      else
      {
          switch (++files)
          {
      case 1:   if (strcmp(current, "-") == 0)
                {   /* then just leave as stdin */
#ifdef COMPILING_ON_RISC_OS
                    /* Change default no-buffering to line buffering...  */
#ifdef FOR_ACORN
                    /* A fault in the shared library forces the use of a */
                    /* real buffer. NULL will not do...                  */
                    static char input_buffer[256];
                    setvbuf(stdin, input_buffer, _IOLBF, sizeof(input_buffer));
                    dde_prefix_init("");
#else
#  if defined(TARGET_IS_ARM) && !defined(OBSOLETE_ARM_NAMES)
                    setvbuf(stdin, NULL, _IOLBF, 256);
#  endif
#endif
#endif
                }
                else
                {
#ifdef FOR_ACORN
                  /* IDJ: 06-Jun-94. Set desktop "current directory" */
                  dde_prefix_init(current);
                  dde_sourcefile_init();
#endif
                  if (freopen(current,"r",stdin) != NULL)
                  { UnparsedName unparse;
                    char new_dir[MAX_NAME], *mod;
                    ccom_flags &= ~FLG_STDIN;
                    sourcefile = current;
                    /*
                     * Add path name of source file to the -I list.
                     */
                    translate_fname(current, &unparse, new_dir);
                    new_dir[unparse.un_pathlen] = '\0';
                    path_hd = mk_path_element(path_hd, PE_USER, new_dir);
/* Make sure path_tl is always the tail, even if file precedes -I    */
                    if (path_hd->link == 0) path_tl = path_hd;
/* set up 'sourcemodule' from main part of file name:                   */
                    mod = (char *)ccom_alloc(unparse.rlen+1L);
                    memcpy(mod, unparse.root, unparse.rlen);
                    mod[unparse.rlen] = 0;
                    sourcemodule = mod;
                  }
                  else
                  { msg_sprintf(message, driver_couldnt_read, current);
                    driver_abort(message);
                  }
                }
                break;
      case 2:   objectfile = current;
                break;
      default:  driver_abort(msg_lookup(driver_too_many_file_args));
          }
      }
  }
  if (ccom_flags & FLG_STDIN)
  {   path_hd = mk_path_element(path_hd, PE_USER, "");
      if (path_hd->link == 0) path_tl = path_hd;
  }

#ifdef TARGET_HAS_SEPARATE_CODE_DATA_SEGS
  /* On machines like amd29000 code and data buses are separate    */
  /* so that all non-instruction data must go in the data segment. */
  feature |= FEATURE_WR_STR_LITS;
#endif

  if (ccom_flags & FLG_COMPILE)
  {
      /* under the driver.c interface at most one of the following is true */
#ifndef NO_OBJECT_OUTPUT
      if (objectfile[0] != '\0' && !(ccom_flags & FLG_NO_OBJECT_OUTPUT))
      {   objstream = cc_open(objectfile, BINARY_FILE);
# ifdef COMPILING_ON_RISC_OS
          set_time_stamp(objectfile, NO);
# endif
      }
#endif
#ifndef NO_ASSEMBLER_OUTPUT
      if (asmfile[0] != '\0') asmstream = cc_open(asmfile, TEXT_FILE);
#endif
      if (objectfile[0] == '\0' && asmfile[0] == '\0')
      {
          asmstream = stdout;
          feature |= FEATURE_ANNOTATE;   /* simple test use */
      }

#ifndef NO_LISTING_OUTPUT
      if (ccom_flags & FLG_LISTING)
      {
          if (listingfile[0] != '\0')
          {
              listingstream = cc_open(listingfile, TEXT_FILE);
              if (listingstream != stdout)   /* @@@ get rid of this hack */
                  fprintf(listingstream, "     1  ");
          }
          else listingstream = stdout;
          if (ccom_flags & FLG_COUNTS)
          { FILE *map = fopen("counts", "rb");
            if (map == NULL) driver_abort(msg_lookup(driver_couldnt_read_counts));
            if (!map_init(map)) driver_abort(msg_lookup(driver_malformed_counts));
          }
      }
#endif
  }

  if (ccom_flags & FLG_MAKEFILE)
  {   if (makefile[0] == 0)
          makestream = stdout;
      else
      {   /* if -M+, then open with append ("a") if already writing to  */
          /* makefile (makeflag != 0)                                   */
          makestream = cc_open(makefile,
                  makeflag ? TEXT_FILE_APPEND : TEXT_FILE);
          makeflag = 1;
      }
      /* Print out source file and object file for -M option...         */
      fprintf(makestream, DEPEND_FORMAT, objectfile, sourcefile);
  }
}

/*
 * Inner compile control routine.
 */

static void compile_statements(void)
{   bool decls = NO;
#ifndef PASCAL /*ECN*/
/* AM, Sept 91: I am inclined to think the best interface is to call    */
/* rd_topdecl(0) (1,2,3,...) until it returns NULL (or list1(s_eof))    */
/* Then all conceivable actions could be done, instead of this mess.    */
/* May 1993: process almost complete for C/C++...                       */
    for (;;)
#endif
    { TopDecl *d; AEop h0d;
      clock_t t0;
      phasename = "reinit";
      lex_beware_reinit();  /* preserve needed things over reinit */
      drop_local_store();   /* in case nextsym() above read #if   */
      alloc_reinit();
      lex_reinit();
      cg_reinit();          /* must be done BEFORE parsing */
#ifdef PASCAL /*ECN*/
      syn_init();           /* @@@ AM: surely is just part of rd_topdecl? */
                            /* then syn_init can be OK as usual.          */
#endif

#ifndef PASCAL /*ECN*/
      t0 = clock();
#endif
      phasename = "parse";
      d = rd_topdecl();
      if (d == 0) syserr("rd_topdecl() => NULL");
      alloc_noteAEstoreuse();
#ifndef PASCAL /*ECN*/
      tmuse_front += clock() - t0;
#endif

      if (debugging(DEBUG_AETREE)) pr_topdecl(d);

      t0 = clock();
      phasename = "jopcode";
      h0d = h0_(d);             /* killed by drop_local_store()!        */
      if (h0d == s_fndef) cg_topdecl(d, curlex.fl);
      tmuse_back += clock() - t0;
      drop_local_store();
      if (h0d == s_eof) break; else decls = YES;
    }
#ifndef PASCAL /*ECN*/
    if (!decls && (feature & FEATURE_ANSI))    /* move to rd_topdecl()? */
      cc_rerr(compiler_rerr_no_extern_decl);
#endif
    if (LanguageIsCPlusPlus) vg_ref_dynamic_init();
#ifdef TARGET_IS_HELIOS
    obj_makestubs();             /* tentative positioning */
#endif
}

static void cleanup(void)
{
  bind_cleanup();
  pp_tidyup();

  if (debugging(DEBUG_STORE))
  {
      fprintf( stderr,"Time: %ldcs front-end %ldcs back-end\n",
               (long) tmuse_front,(long) tmuse_back);
      show_store_use();
  }

  cg_tidy();

#ifndef NO_OBJECT_OUTPUT
# ifdef COMPILING_ON_ACORN_KIT
  {   bool have_obj = (objstream != NULL);
      /* objstream cannot be stdout, so cc_close does a spurious test. */
      cc_close(&objstream, objectfile);
#   ifdef COMPILING_ON_RISC_OS
      if (have_obj) set_time_stamp(objectfile, YES);
#   endif
#   ifdef COMPILING_ON_UNIX
      if (have_obj && system_flavour != NULL)
      {   char *cmd;
          cmd = GlobAlloc(SU_Other,
                          24 + strlen(system_flavour) + strlen(objectfile));
          sprintf(cmd,"/usr/bin/symrename -%s %s",system_flavour,objectfile);
          system(cmd);
      }
#   endif
  }
# else
  cc_close(&objstream, objectfile);
# endif
#endif

#ifndef NO_ASSEMBLER_OUTPUT
  cc_close(&asmstream, asmfile);
#endif

  cc_close(&listingstream, listingfile);
  cc_close(&makestream, makefile);

  summarise();

#ifdef ENABLE_MAPSTORE
  if (debugging(DEBUG_MAPSTORE)) _mapstore();
#endif

  alloc_dispose();
}

/*
 * Open include file for pre-processor.
 * (included here because of system dependencies).
 */

static void show_h_line(int32 line, char *file, bool to_makefile)
{
  if (ccom_flags & FLG_PREPROCESS)
      printf("#%s %lu \"%s\"\n",
             (feature & FEATURE_PCC ? "" : "line"), (long)line, file);
  if (to_makefile && ccom_flags & FLG_MAKEFILE)
      fprintf(makestream, DEPEND_FORMAT, objectfile, file);
}

#ifndef NO_INSTORE_FILES

static FILE *try_instore_file(char *file, bool *sf)
{   FILE *include_file;
    if (debugging(DEBUG_FILES)) cc_msg("Try instore file '%s'\n", file);
    include_file = open_builtin_header(file, sf);
    if (include_file != NULL)
    {   show_h_line(1, file, NO);
        push_include(NULL, file);
    }
    return include_file;
}

#endif /* NO_INSTORE_FILES */

extern FILE *pp_inclopen(char *file, bool systemheader,
            bool *sf, char **hostname)
{
  FILE *new_include_file;
  UnparsedName unparse;
  char new_file[MAX_NAME];

  *hostname = file;
  translate_fname(file, &unparse, new_file);

  if (!(unparse.type & FNAME_ROOTED))
  {   PathElement *p;
#ifndef NO_INSTORE_FILES
      if ((ccom_flags & FLG_INSTORE_FILES_IMPLICITLY) && systemheader)
      {
/* Note that it is important for portability (even across unix/riscos)  */
/* to have the original 'file' and not the munged 'new_file' instore.   */
          new_include_file = try_instore_file(file, sf);
          if (new_include_file != NULL) return new_include_file;
      }
#endif
      p = path_hd;
      if (systemheader || (ccom_flags & FLG_USE_SYSTEM_PATH)) p = p->link;
      while (p != NULL)
      {   char current[MAX_NAME];
          if (p->flags & INSTORE_FILE)
          {
#ifndef NO_INSTORE_FILES
/* Note that it is important for portability (even across unix/riscos)  */
/* to have the original 'file' and not the munged 'new_file' instore.   */
              new_include_file = try_instore_file(file, sf);
              if (new_include_file != NULL) return new_include_file;
#endif
          }
          else /* current path is not :mem */
          {   strcpy(current, p->name);
              if (strlen(current) + strlen(new_file) + 1 <= MAX_NAME)
              {   strcat(current, new_file);
                  if (debugging(DEBUG_FILES))
                      cc_msg("Try file '%s'\n", current);
                  if ((new_include_file = fopen(current, "r")) != 0)
                  {   if (!(systemheader && (ccom_flags & FLG_NOSYSINCLUDES)))
                          show_h_line(1, current, YES);
                      *hostname = push_include(current, current);
                      return new_include_file;
                  }
              }
          }
          p = p->link;
      }
#ifdef COMPILING_ON_RISC_OS
      /* IDJ 06-Jun-94: before trying instore headers we try just the filename
       * by itself.  This is because of something like <foo$dir>.h.bar which
       * may or may not be a rooted filename.
       */
      if (debugging(DEBUG_FILES)) cc_msg("Try file '%s'\n", new_file);
      if ((new_include_file = fopen(new_file, "r")) != 0)
      {   if (!(systemheader && (ccom_flags & FLG_NOSYSINCLUDES)))
              show_h_line(1, new_file, YES);
          *hostname = push_include(new_file, new_file);
          return new_include_file;
      }
#endif
#ifndef NO_INSTORE_FILES
/* Not found - ANSI require looking for "stdio.h" as <stdio.h> when all */
/* else has failed.                                                     */
/* Note that it is important for portability (even across unix/riscos)  */
/* to have the original 'file' and not the munged 'new_file' instore.   */
      return try_instore_file(file, sf);
#endif
  }
  else  /* rooted file */
  {
      if (debugging(DEBUG_FILES)) cc_msg("Try file '%s'\n", new_file);
      if ((new_include_file = fopen(new_file, "r")) != 0)
      {   if (!(systemheader && (ccom_flags & FLG_NOSYSINCLUDES)))
              show_h_line(1, new_file, YES);
          *hostname = push_include(new_file, new_file);
          return new_include_file;
      }
  }
  return 0;  /* failed */
}

extern void pp_inclclose(FileLine fl)
{   pop_include();
    show_h_line(fl.l, fl.f, NO);
}

#ifndef TARGET_IS_INTERPRETER
#ifdef HOST_USES_CCOM_INTERFACE
/* don't move the opening braces in the lines below : topcc needs them */
extern int main(int argc, char *argv[]) {
#else
extern int ccom(int argc, char *argv[]) { /* must match spec for main */
#endif
#ifdef HOST_USES_CCOM_INTERFACE
#ifndef  TARGET_IS_UNIX
  fprintf(stderr, "%s\n", CC_BANNER);
#ifndef COMPILING_ON_MSDOS
  (void) signal(SIGINT, compile_abort);
#endif
#else /* TARGET_IS_UNIX */
  /* The signal ignore state can be inherited from the parent... */
#define sig_ign ((void (*)(int))(SIG_IGN))
  if (signal(SIGINT,  sig_ign) != sig_ign)
    (void) signal(SIGINT, compile_abort);
  if (signal(SIGHUP,  sig_ign) != sig_ign)
    (void) signal(SIGHUP, compile_abort);
  if (signal(SIGTERM, sig_ign) != sig_ign)
    (void) signal(SIGTERM, compile_abort);
#endif
#endif

#if (defined NLS) && (defined HOST_USES_CCOM_INTERFACE)
  msg_init(argv[0],MSG_TOOL_NAME);
#endif

  ccom_flags = FLG_STDIN + FLG_COMPILE + FLG_INSTORE_FILES_IMPLICITLY;

  asmfile = listingfile = makefile = objectfile = sourcefile = sourcemodule = ""
;
  system_flavour = NULL;

  asmstream = objstream = listingstream = makestream = 0;
  makeflag = 0;

  tmuse_front = tmuse_back = 0;

  path_hd = path_sav = 0;

#ifdef PASCAL /*ECN*/
  rtcheck = RTCHECK_DEADCODE;
#endif

#ifdef TARGET_WANTS_FUNCTION_NAMES
  feature          = FEATURE_SAVENAME;
                              /* keep function names (for debugger) */
#else
  feature          = 0;
#endif

#ifdef CPLUSPLUS
  feature          |= FEATURE_CPP;
#else
/* /* PASCAL? FORTRAN? others? */
  feature          |= FEATURE_ANSI;
#endif

#ifdef TARGET_IS_UNIX
  feature          |= FEATURE_WR_STR_LITS | FEATURE_LIMITED_PCC;
#endif

  suppress         = D_SUPPRESSED;
  sysdebugmask     = 0;
  syserr_behaviour = 0;
  aetree_debugcount = 0;
  cse_debugcount   = 0;
  localcg_debugcount = 0;
#ifdef TARGET_HAS_DEBUGGER
  usrdbgmask       = 0;
#endif

#ifndef NO_CONFIG
    config_init();
#else
     config = 0;
#endif

  {   static int endian_test = 1;
      host_lsbytefirst = *((char *)&endian_test) != 0;
  }
#ifdef TARGET_ENDIANNESS_CONFIGURABLE
  if (!(config & CONFIG_ENDIANNESS_SET))
      if (!host_lsbytefirst) config |= CONFIG_BIG_ENDIAN;
#endif
  target_lsbitfirst = target_lsbytefirst;

#ifdef MIN_ALIGNMENT_CONFIGURABLE
  alignof_struct_val = alignof_struct_default;
  alignof_toplevel_static_var = alignof_toplevel_static_default;
#endif

  phasename = "init";
  currentfunction.symstr = NULL;

  errstate_init();
  set_debug_options(argc, argv);

  alloc_init();
  pp_init(&curlex.fl);
                  /* for pp_predefine() option and pragma on command line  */
                  /* must init_sym_tab here if pp shares its symbol tables */
  sourcefile = "<stdin>"; sourcemodule = "none";

#ifdef FORTRAN
  pp_pragmavec['x'-'a'] = 1;   /* Enable double-complex by default for now */
#endif

#ifndef DEFAULT_DOES_NO_CSE
/*
 * Enable CSE by default, even though there may be bugs in the code yet...
 * Use -zpz0 to disable CSE if you need to.
 */
  var_cse_enabled = 1;
#endif

  var_cc_private_flags = 0;      /* No development options switched on yet */

  set_compile_options(argc, argv);
#ifdef SOFTWARE_FLOATING_POINT
  if (software_floating_point_enabled) pp_predefine("__SOFTFP__");
#endif
  if (config & CONFIG_OPTIMISE_TIME) var_crossjump_enabled = 0;

  pp_notesource(sourcefile, stdin);
  show_h_line(1, sourcefile, NO);

  aetree_init();
  bind_init();
  lex_init();                        /* sets curlex.sym to s_nothing   */
  builtin_init();                    /* change to setup from syn_init? */
  sem_init();
#ifndef PASCAL /*ECN*/
  syn_init();
#endif
  vargen_init();
  cg_init();
  initstaticvar(datasegment, 1);    /* nasty here */
  drop_local_store();       /* required for alloc_reinit()             */

  if (ccom_flags & FLG_COMPILE)
      compile_statements();
  else
      preprocess_only();
  cleanup();

  /* AM: re-open the discussion on return codes and errors.             */
  if (errorcount != 0) return EXIT_error;
  if (pp_pragmavec['e'-'a'] > 0) return 0; /* -zpe1 => "generous" mode */
  if (recovercount != 0) return EXIT_error;
  if (warncount != 0) return EXIT_warn;
  return 0;
}
#endif

#ifdef TARGET_IS_INTERPRETER
void compiler_init(void)
{
     path_hd = mk_path_element(path_hd, PE_USER, "");
}
#endif

/* end of compiler.c */
