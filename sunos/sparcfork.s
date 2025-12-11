	.globl	__vfork
	.globl	__wait
__vfork:
	or	%g0,2,%g1	! actually fork, (66 is vfork)
	ta	%g0
	bcs    __cerror
	nop
	orcc	%g0,%o1,%g0
	bne,a	vexit
	or	%g0,%g0,%o0
vexit:	jmp	%o7+8
	nop

__wait: or	%o0,0,%o1
	or	%g0,%g0,%o0
	or	%g0,%g0,%o2
	or	%g0,%g0,%o3
	or	%g0,7,%g1
	ta	%g0
	bcs    __cerror
	nop
	jmp	%o7+8
	nop
