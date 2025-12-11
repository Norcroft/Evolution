	.global __start
	.text
__start:
	mov	0, %fp
	ld	[%sp + 64], %o0
	add	%sp, 68, %o1
	sll	%o0, 2,	%o2
	add	%o2, 4,	%o2
	add	%o1, %o2, %o2
	sethi	%hi(__environ), %o3
	st	%o2, [%o3+%lo(__environ)]
	andn	%sp, 7,	%sp
	call	___init
	sub	%sp, 24, %sp		! Never returns
!	or	%g0, 1, %g1
!	ta	%g0
!
	.data
	.align	8
! __fparg1:
	.word	0
	.word	0
_environ:
	.word	0

