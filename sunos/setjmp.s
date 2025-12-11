	.globl	_setjmp
	.globl	_longjmp
_setjmp:
	save	%sp, -0x68, %sp
	clr	%o0		! Answer
	mov	109, %g1	! Syscall sigblock
	ta	0
	st	%o0, [%i0 + 4]
	ld	[%sp], %g0	! No idea why this is done
	add	%sp, 92, %o1	! 
	clr	%o0
	mov	112, %g1	! Syscall sigset
	ta	0
	ld	[%sp + 96], %g1
	st	%fp, [%i0 + 8]
	st	%g1, [%i0]
	add	%i7, 8, %g1	! the return address
	st	%g1, [%i0 + 12]
	add	%g1, 4, %g1
	st	%g1, [%i0 + 16]
	st	%g0, [%i0 + 20]	! Zero these words
	st	%g0, [%i0 + 24]
	st	%g0, [%i0 + 32]
	ret
	restore	

_longjmp:
	mov	1, %g1
	orcc	%g0, %o1, %g0
	be,a	L4
	mov	1, %o1
L4:
	orcc	%g0, %g1, %g0
	bne,a	L5
	mov	0x8b, %g1	! The unused 139!
	ta	0x3
	sub	%sp, 64, %sp
	ld	[%o0 + 8], %fp
	ld	[%o0 + 12], %g1
	sub	%g1, 8, %o7	! odd code here; why fiddle?
	jmp	%o7 + 8
	restore	%o1, 0, %o0	! Set the answer
L5:
	st	%o1, [%o0 + 28]
	ta	0
