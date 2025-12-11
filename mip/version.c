/* mip/version.c
 * Copyright (C) Acorn Computers Ltd., 1988-1990.
 * Copyright (C) Advanced RISC Machines Limited, 1991-1992.
 * Copyright (C) Codemist Ltd, 1987-1994.
 * Defines the version string and banner for the Codemist or OEM compiler.
 */

/*
 * RCS $Revision: 1.5 $
 * Checkin $Date: 93/10/12 12:41:22 $
 * Revising $Author: irickard $
 */

#ifdef __STDC__
#  include <string.h>
#else
#  include <strings.h>
#endif
#include "globals.h"                    /* for TARGET_MACHINE */
#include "version.h"
#include "fevsn.h"
#include "mipvsn.h"
#include "mcvsn.h"

#ifndef __DATE__
#  include "datenow.h"
#endif
#ifndef __TIME__
#  define __TIME__ ""
#endif

#ifdef __STDC__
#  ifndef NO_STATIC_BANNER
#    define STATIC_BANNER 1       /* no string concatenation.             */
#  endif
#endif

#ifdef VENDOR_SPRINTF_BANNER
#  undef STATIC_BANNER
#endif

/* AM: company-specific version strings should appear in options.h.     */
#undef VERSION_STRING
#ifdef RELEASE_VSN
#  define VERSION_STRING RELEASE_VSN
#else
#  ifdef NON_RELEASE_VSN
#    define VERSION_STRING NON_RELEASE_VSN
#  else
#    ifdef __STDC__
#      define VERSION_STRING FE_VERSION "/" MIP_VERSION "/" MC_VERSION
#    else
#      define VERSION_STRING "<unspecified>"
#    endif
#  endif
#endif

/* Note the comment in mip/version.h re the 4 nulls at end of string.   */

#ifdef STATIC_BANNER

#ifndef VERSION_DATE
#  define VERSION_DATE __DATE__
#endif

static char cc_banner[] =  "Norcroft " \
                           TARGET_SYSTEM " " TARGET_MACHINE " " LANGUAGE \
                           " vsn " VERSION_STRING " [" VERSION_DATE "]\0\0\0";

char *version_banner(void)
{
  return cc_banner;
}

#else  /* ! STATIC_BANNER */

/*
 * Can't build the banner using ANSI string concatenation,
 * so build it dynamically instead.
 */

static char cc_banner[128] = "";        /* expression instead of 128?   */

#ifdef VERSION_DATE_UK_NUMERIC
static char *uk_numeric_date()
{   static char d[] = __DATE__;         /* e.g. "Jul 19 1995"           */
    static char r[] = "dd/mm/yy";
    static char m[] =
            "Jan01Feb02Mar03Apr04May05Jun06Jul07Aug08Sep09Oct10Nov11Dec12";
    char *p;
    for (p = m; p < m+60; p += 5)
        if (memcmp(d, p, 3) == 0)
        {   r[0] = d[4] == ' ' ? '0' : d[4];
            r[1] = d[5];
            r[3] = p[3];
            r[4] = p[4];
            r[6] = d[9];
            r[7] = d[10];
            return r;
        }
    return d;
}
#define VERSION_DATE uk_numeric_date()
#endif

#ifndef VERSION_DATE
#  define VERSION_DATE __DATE__
#endif

char *version_banner(void)
{   if (cc_banner[0]=='\0')
    {   sprintf(cc_banner,
#ifdef VENDOR_SPRINTF_BANNER
                           VENDOR_SPRINTF_BANNER
#else
                           "Norcroft %s %s %s vsn %s [%s]\0\0\0",
                           TARGET_SYSTEM, TARGET_MACHINE, LANGUAGE,
                           VERSION_STRING, VERSION_DATE
#endif
               );
      }
    return cc_banner;
}

#endif /* STATIC_BANNER */

/* end of mip/version.c */
