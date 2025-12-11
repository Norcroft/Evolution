#include "hostsys.h"
#include "syscall.h"

int getuid(void)
{
  return _syscall0(SYS_getuid);
}


