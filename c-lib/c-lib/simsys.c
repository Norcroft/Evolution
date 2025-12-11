
/* simsys.c:  Copyright (C) Codemist Ltd, 1996-1999.    */
/* Very specific to Codemist interpreter (nec,c4p etc). */

/* version 2 */

#define __system_io 1
#include "hostsys.h"                            /* things like _initio() */
#include <stdio.h>                              /* for EOF               */
#include <stdlib.h>                             /* for exit()            */
#include <ctype.h>                              /* for isprint()         */
#include <string.h>                             /* for strlen()          */
#include <time.h>                               /* for clock             */
#include <errno.h>                               /* for clock             */

extern int main(int argc, char **argv);          /* the point of it all   */
extern void _ctype_init(void);

volatile int errno;

#ifdef _Codemist
#define __systrap(a)
#endif

extern __systrap(CMsim) int _syscall0x(int);
extern __systrap(CMsim) int _syscall1x(int, int);
extern __systrap(CMsim) int _syscall2x(int, int, int);
extern __systrap(CMsim) int _syscall3x(int, int, int, int);
extern int _syscall0(int a) { return _syscall0x(a);}
extern int _syscall1(int a,int b) { return _syscall1x(a,b);}
extern int _syscall2(int a,int b,int c) { return _syscall2x(a,b,c);}
extern int _syscall3(int a,int b,int c,int d) { return _syscall3x(a,b,c,d);}

/* timing things... */
struct timeval {
	long	tv_sec;		/* seconds */
	long	tv_usec;	/* and microseconds */
};

clock_t clock()
{   return _syscall0(SYS_clock);
}

time_t time(time_t *timer)
{
  time_t ans;
  struct timeval {
    long tv_sec;        /* seconds since Jan. 1, 1970 */
    long tv_usec;       /* and microseconds */
  } tv;
  errno = 0;
  if (_syscall2(SYS_gettimeofday, (int)&tv, NULL) == 0)
    ans = (time_t)tv.tv_sec;
  else ans = (time_t)-1;
  if (timer != NULL) *timer = ans;
  return ans;      
}

/* system dependent I/O routines ... */

/* The following line just happens to match the WATCOM C includes, but   */
/* this is only coincidental, since the bits are re-processed back to    */
/* fopen flags in unixtrap.c.                                            */
#define	O_RDONLY	000		/* open for reading */
#define	O_WRONLY	001		/* open for writing */
#define	O_RDWR		002		/* open for read & write */
#define	O_APPEND	00010		/* append on each write */
#define	O_CREAT		01000		/* open with file create */
#define	O_TRUNC		02000		/* open with truncation */
#define O_TEXT          04000           /* open in text mode */

FILEHANDLE _sys_open(const char *filename, int openmode)
{   /* nasty magic number interface for openmode */
    static int modtab[6] = { /* r = */  O_RDONLY,
			     /* r+ = */ O_RDWR,
                             /* w = */  O_WRONLY|O_CREAT|O_TRUNC,
			     /* w+ = */ O_RDWR  |O_TRUNC|O_CREAT,
                             /* a = */  O_WRONLY|O_CREAT,
			     /* a+ = */ O_RDWR  |O_CREAT|O_APPEND };
    char *name = (char *)filename;            /* yuk */
    FILEHANDLE fh;
    int newmode = modtab[openmode >> 1];         /* forget the 'b'inary bit */
    if ((openmode & 1) == 0) newmode |= O_TEXT;
    fh = _syscall3(SYS_open, (int)name, newmode, 0666);
    if (fh<0) return NONHANDLE;            /* not found             */
    return fh;
}

extern int _error_recursion;
int _error_recursion;

int _fileno(FILE *strm)
{
  return (int)strm->__file;
}

void _sys_msg(const char *s)
{ 
    if ((stderr->__flag & _IOWRITE) && !_error_recursion)
    {   _error_recursion = 1;
        putc('\n', stderr);
        puts(c, stderr);
        putc('\n', stderr);
        _error_recursion = 0;
    }
    else
    {   _ttywrite((unsigned char *)"\n", 1, 0);
        _ttywrite((unsigned char *)s, strlen(s), 0);
        _ttywrite((unsigned char *)"\n", 1, 0);
    }
}

int _sys_read_(FILEHANDLE fh, unsigned char *buf, int len, int mode)
{
  int n = _syscall3(SYS_read, (int)fh, (int)buf, (int)len);
  if (n==-1) {			/* error condition */
    n=0;
  }
  n = len - n;			/* number of bytes NOT read */
  if (n!=0) n |= 0x80000000;
  return n;
}

#ifndef _Codemist
extern int _curbrk;

int __main(int argc, char **argv)
{   int ans;
    _error_recursion = 0;
    _curbrk = _syscall0(SYS_gbreak);
    _ctype_init();		/* init to C locale */
    _init_alloc();
    _exit_init();           /* must happen before exit() can be called   */
    _initio(NULL, NULL, NULL);
    ans = main(argc, argv);
    while (_number_of_exit_functions!=0)
      (*_exitvector[--_number_of_exit_functions])();
    _terminateio();
    _exit(ans);
    l: goto l;                  /* interpreter will see this */
}

#endif

/* end of simsys.c */
