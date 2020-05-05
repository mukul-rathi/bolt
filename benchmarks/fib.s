	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 14
	.globl	_fib                    ## -- Begin function fib
	.p2align	4, 0x90
_fib:                                   ## @fib
## %bb.0:                               ## %entry
	pushq	%rbp
	pushq	%rbx
	pushq	%rax
	movl	$1, %ebp
	cmpl	$2, %edi
	jb	LBB0_3
## %bb.1:                               ## %else.preheader
	movl	%edi, %ebx
	movl	$1, %ebp
	.p2align	4, 0x90
LBB0_2:                                 ## %else
                                        ## =>This Inner Loop Header: Depth=1
	leal	-1(%rbx), %edi
	callq	_fib
	addl	$-2, %ebx
	addl	%eax, %ebp
	cmpl	$1, %ebx
	ja	LBB0_2
LBB0_3:                                 ## %ifcont
	movl	%ebp, %eax
	addq	$8, %rsp
	popq	%rbx
	popq	%rbp
	retq
                                        ## -- End function
	.globl	_main                   ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
## %bb.0:                               ## %entry
	pushq	%rax
	movl	$46, %edi
	callq	_fib
	leaq	L___unnamed_1(%rip), %rdi
	movl	%eax, %esi
	xorl	%eax, %eax
	callq	_printf
	xorl	%eax, %eax
	popq	%rcx
	retq
                                        ## -- End function
	.section	__TEXT,__cstring,cstring_literals
L___unnamed_1:                          ## @0
	.asciz	"Fib Bolt of 46 is: %d\n"


.subsections_via_symbols
