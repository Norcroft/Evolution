/*
 * Code to provide a test-base for the Codemist C library
 */

#include <stdio.h>

#include "hostsys.h"
#include "simsyint.h"

extern int _Codemist_main(int argc, char *argv[]);

extern int _curbrk;
extern int _error_recursion;
extern void _ctype_init(void);
extern void _init_alloc(void);
extern void _exit_init(void);
extern void _initio(char *, char *, char *);
extern void _terminateio(void);

int main(int argc, char *argv[])
{
    int ans;
    printf("Codemist Library test harness starting up\n");
    _error_recursion = 0;
    _curbrk = _syscall0(SYS_gbreak);
/*
 * The initialisation calls here force loading of various bits of the
 * library even when nothing else does. Thus at present ctype_init causes
 * a bunch of character tables to be loaded even if ni isalmum() etc
 * things are used. Hmmm.
 */
    _ctype_init();		/* init to C locale */
    _init_alloc();
    _exit_init();           /* must happen before exit() can be called   */
    _initio(NULL, NULL, NULL);
    ans = _Codemist_main(argc, argv);
    while (_number_of_exit_functions!=0)
      (*_exitvector[--_number_of_exit_functions])();
    _terminateio();
    _exit(ans);
    return ans;
/*    l: goto l;                  /* interpreter will see this */
}

void debug_msg0(char *s)
{
    printf(s);
    fflush(stdout);
}

void debug_msg1(char *s, int a)
{
    printf(s, a);
    fflush(stdout);
}

int _syscall0x(int op)
{
    printf("syscall0x %d called\n", op);
    fflush(stdout);
    return 0;
}

int _syscall1x(int op, int a)
{
    printf("syscall1x(%d, %d) called\n", op, a);
    fflush(stdout);
    return 0;
}

int _syscall2x(int op, int a, int b)
{
    printf("syscall2x(%d,%d,%d) called\n", op, a, b);
    fflush(stdout);
    return 0;
}

int _syscall3x(int op, int a, int b, int c)
{
    printf("syscall3x(%d, %d, %d, %d) called\n", op, a, b, c);
    fflush(stdout);
    return 0;
}

/* end of lib-base.c */
