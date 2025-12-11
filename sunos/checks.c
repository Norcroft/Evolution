#include <stdio.h>
#include <stdlib.h>
#include "hostsys.h"

extern unsigned int etext;
#ifdef __clipper
#define LOW_LIMIT	0x8000
#else
#define LOW_LIMIT	1
#endif
extern void *_sbrk(int);

unsigned int _rd1chk(unsigned int x)
{
  if ((x < LOW_LIMIT) || (x > (unsigned int)_sbrk(0))) {
    fprintf(stderr,"Read check failure %x\n", x);
    exit(1);
  }
  return x;
}

unsigned int _rd2chk(unsigned int x)
{
  if ((x&1) != 0 || (x < LOW_LIMIT) || (x > (unsigned int)_sbrk(0))) {
    fprintf(stderr,"Read check failure %x\n", x);
    exit(1);
  }
  return x;
}

unsigned int _rd4chk(unsigned int x)
{
  if (((x&3) != 0) || (x < LOW_LIMIT) || (x > (unsigned int)_sbrk(0))) {
    fprintf(stderr,"Read check failure %x\n", x);
    exit(1);
  }
  return x;
}

unsigned int _wr1chk(unsigned int x)
{
  if ((x < etext) || (x > (unsigned int)_sbrk(0))) {
    fprintf(stderr,"Write check failure %x\n", x);
    exit(1);
  }
  return x;
}

unsigned int _wr2chk(unsigned int x)
{
  if ((x&1) != 0 || (x < etext) || (x > (unsigned int)_sbrk(0))) {
    fprintf(stderr,"Write check failure %x\n", x);
    exit(1);
  }
  return x;
}

unsigned int _wr4chk(unsigned int x)
{
  if (((x&3) != 0) || (x < etext) || (x > (unsigned int)_sbrk(0))) {
    fprintf(stderr,"Write check failure %x\n", x);
    exit(1);
  }
  return x;
}

