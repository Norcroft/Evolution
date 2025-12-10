/*
  Title:        mcdep.c - miscellaneous target-dependent things.
  Copyright:    (C) 1988-2001, Codemist Ltd
*/

#include <ctype.h>
#include <string.h>

#include "globals.h"
#include "mcdep.h"
#include "mcdpriv.h"

int32 config;
int32 mips_opt = 0;

bool mcdep_config_option(char name, char tail[])
{
    switch (name)
    {
case 'h': case 'H':
        switch (tail[0])
        {
  case 'C':
  case 'c': if (isdigit(tail[1]))
            {   /* i.e. set procedure calling standard */
                mips_opt |= (int32)1 << (tail[1] - '0' + 10);
                return YES;
            }
            break;
        }
        if (isdigit(tail[0]))
        {   mips_opt |= (int32)1 << (tail[0] - '0');
            return YES;
        }
        break;
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
    config = CONFIG_FPREGARGS;
    config |= CONFIG_BIG_ENDIAN|CONFIG_ENDIANNESS_SET;
}

KW_Status mcdep_keyword(const char *key, int *argp, char **argv)
{   if (strcmp(key, "-mips16") == 0) 
    {   mips_opt = 3;  /* + (1<<6); */
        return KW_OK;
    }
    return KW_NONE;
}

/* end of mips/mcdep.c */
