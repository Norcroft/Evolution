
#include "hostsys.h"                            /* things like _initio() */
#include <stdio.h>                              /* for EOF               */
#include <stdlib.h>                             /* for exit()            */

int remove(const char *pathname)
{
    return _syscall1(SYS_unlink,(int)pathname);
}

int rename(const char *oldname, const char *newname)
{
  if (_syscall2(SYS_link, (int)oldname, (int)newname) == 0) {
    (void) remove(oldname);
    return 0;
  }
  return -1;
}

