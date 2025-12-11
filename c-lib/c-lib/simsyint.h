
/* simsyint.h: pseudo-unix traps for Codemist interpreter (nec,c4p etc) */

#ifndef	__sys_syscall_h
#define	__sys_syscall_h

#define	SYS_exit	1
#define	SYS_read	3
#define	SYS_write	4
#define	SYS_open	5
#define	SYS_close	6
#define	SYS_link	9
#define	SYS_unlink	10
#define	SYS_lseek	19
#define	SYS_getpid	20
#define	SYS_kill	37
#define	SYS_sigvec	108
#define	SYS_gettimeofday 116

#define SYS_isatty      1024      /* special case of ioctl */
#define SYS_getenv      1025
#define	SYS_clock	1026      /* special case of getrusage */
#define SYS_sbreak      1027
#define SYS_gbreak      1028      /* i.e. &end */
#define SYS_flen        1029      /* special case of fstat */

#endif	/* !__sys_syscall_h */
