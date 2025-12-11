#include "hostsys.h"
#include <stdlib.h>
#include <signal.h>

#ifdef __sparc
#define SYS_vfork 65
#endif

#ifdef CMsim
int system(const char *string) { return 1; }
#else

extern char ** _environ;
extern int _vfork(void);
extern int _wait(int*);

extern int system(const char *string)
{
  int pid;
  void (*s2)(int);
  void (*s3)(int);
  if ((pid = _vfork()) == 0) {
    char *gv[4] = {"sh", "-c", (char*)0, (char*)0};
    gv[2] = (char *)string;
    _syscall3(SYS_execve, (int)"/bin/sh", (int)gv, (int)_environ);
    _exit(127);			/* Should not get here!! */
  }
  s2 = signal(SIGINT, SIG_IGN);
  s3 = signal(SIGQUIT, SIG_IGN);
  while (1) {
    int status;
    int wp = _wait(&status);
    if (wp == pid || wp == -1) {
      signal(SIGINT,s2);
      signal(SIGQUIT,s3);
      return (wp == -1 ? -1 : status);
    }
  }
}
#endif
