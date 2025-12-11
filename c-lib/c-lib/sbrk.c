#ifdef __sparc
	/* **Dangerous** */
#define SYS_sbreak	17
#endif

#include "hostsys.h"

int _curbrk;

#ifdef _Codemist
extern volatile int _Codemist_errno;
#else
extern volatile int errno;
#endif

extern void * _sbrk(size_t n)
{
  int *ans;
  _curbrk = (_curbrk+7)&(-8);
  errno = 0;
  ans = (int*)_syscall1(SYS_sbreak, _curbrk + (n = (n+7)&(-8)));
  if (ans != 0) return (void *)-1;
  _curbrk += n;
  return (void*)( _curbrk - n);
}
