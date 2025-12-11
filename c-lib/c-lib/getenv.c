#include <stdlib.h>
#include "hostsys.h"

#ifdef CMsim

/* static char envbuf[256]; */
/* extern char *getenv(const char *name)
 * {   return (char *)_syscall3(SYS_getenv, (int)name, (int)envbuf, 256);
 * }
 */
extern char *getenv(const char *name) { return NULL; }

#else

extern char ** _environ;

#ifndef NULL
#define NULL (char*)0
#endif

static char *getenv1(const char *name, char *envst)
{
  while (*name!='\0') {
    if (*envst == '\0') return NULL; /* Ran out of environment */
    if (*name++ != *envst++) return NULL; /* Not equal at start */
  }
  return (*envst++ == '=' ? envst : NULL);
}

extern char *getenv(const char *name)
{
  char **env = _environ;
  char *ee;
  char *ans;
  while ((ee = *env++) != 0) {
    if ((ans = getenv1(name, ee)) != 0)
      return ans;
  }
  return NULL;
}
#endif
