#include <stdlib.h>
#include "hostsys.h"

extern int raise(int sig)
{
  int pid = _syscall0(SYS_getpid);
  return _syscall2(SYS_kill, pid, sig);
}
