	.globl	__syscall0
	.globl	__syscall1
	.globl	__syscall2
	.globl	__syscall3
	.globl	___ctype
	.globl	__cerror
	.globl	__sys_istty
__syscall3:
__syscall2:
__syscall1:
__syscall0:
	clr	%g1
	ta	%g0
	blu	__cerror
	sethi	%hi(_errno), %g1
	retl
	nop
__cerror:
	st	%i0, [%g1+%lo(_errno)]
	retl
	or      %g0, -0x1, %o0

__sys_istty:
	sethi	%hi(0x40125400), %o1
	or	%o1,0x1,%o1		! The IOCTL constant
	sethi	%hi(__isbuff),%o2
	or	%o2,%lo(__isbuff),%o2
	or	%g0,0x36,%g1
	ta	%g0
	blu	__cerror
	sethi	%hi(_errno), %g1
	retl
	nop

	.data
__isbuff:
	.long	0	! iflag/oflag
	.long	0	! cflag/lflag
	.long	0	! discipline and 8 controls
	.long	0	! discipline and 8 controls
	.long	0	! discipline and 8 controls

	.align	8

	.long	0
___ctype:
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
