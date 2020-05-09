	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 14
	.section	__TEXT,__literal8,8byte_literals
	.p2align	3               ## -- Begin function main
LCPI0_0:
	.quad	4621593937607602996     ## double 9.6000000000000014
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movabsq	$4614388178203810202, %rax ## imm = 0x400999999999999A
	movq	%rax, (%rsp)
	leaq	L_str_fmt.1(%rip), %rdi
	movsd	LCPI0_0(%rip), %xmm0    ## xmm0 = mem[0],zero
	movb	$1, %al
	callq	_printf
	xorl	%eax, %eax
	popq	%rcx
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__cstring,cstring_literals
L_int_fmt:                              ## @int_fmt
	.asciz	"%d\n"

L_str_fmt:                              ## @str_fmt
	.asciz	"%s\n"

L_str_fmt.1:                            ## @str_fmt.1
	.asciz	"%f\n"

L_char_fmt:                             ## @char_fmt
	.asciz	"%s\n"

L_bool_fmt:                             ## @bool_fmt
	.asciz	"%d\n"


.subsections_via_symbols
