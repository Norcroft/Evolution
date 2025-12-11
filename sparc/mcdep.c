/*
  Title:        mcdep.c - miscellaneous target-dependent things.
  Copyright:    (C) 1988, Codemist Ltd
*/

#include <ctype.h>

#include "globals.h"
#include "mcdep.h"
#include "mcdpriv.h"

int32 config;

bool mcdep_config_option(char name, char tail[])
{
  switch (name) {
  default:
    return NO;
  case 'h': case 'H':
    switch (tail[0]) {
/*    case 'a':			/* -zha => do not use floating arg regs */
/*      config ^= CONFIG_FPREGARGS;
/*      return YES;
 */
    case 'b':			/* -zhb => no instruction scheduling */
      sparc_opt |= OPT_NOSCHED;
      return YES;
    case 'x':			/* -zhx => trace instructions */
      sparc_opt |= OPT_TRACE;
      return YES;
    default:
      return NO;
    }
  }
  return NO;
}

    /*************************************************************/
    /*                                                           */
    /*       Code to configure compiler for host system          */
    /*                                                           */
    /*************************************************************/

void config_init(void)
{
    config = 0;    /* was CONFIG_HAS_MULTIPLY, not suitable for SPARC */
}

KW_Status mcdep_keyword(const char *key, int *argp, char **argv)
{   return KW_NONE;
}
