	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 14
	.globl	_main                   ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset %rbx, -16
	movl	$10, %edi
	movl	$5, %esi
	callq	_add
	leaq	L_int_fmt(%rip), %rbx
	movq	%rbx, %rdi
	movl	%eax, %esi
	xorl	%eax, %eax
	callq	_printf
	movl	$10, %edi
	movl	$5, %esi
	callq	_sub
	movq	%rbx, %rdi
	movl	%eax, %esi
	xorl	%eax, %eax
	callq	_printf
	movl	$10, %edi
	movl	$5, %esi
	callq	_mul
	movq	%rbx, %rdi
	movl	%eax, %esi
	xorl	%eax, %eax
	callq	_printf
	movl	$10, %edi
	movl	$5, %esi
	callq	_div
	movq	%rbx, %rdi
	movl	%eax, %esi
	xorl	%eax, %eax
	callq	_printf
	movl	$10, %edi
	movl	$3, %esi
	callq	_mod
	movq	%rbx, %rdi
	movl	%eax, %esi
	xorl	%eax, %eax
	callq	_printf
	movl	$10, %edi
	callq	_add_equal
	movq	%rbx, %rdi
	movl	%eax, %esi
	xorl	%eax, %eax
	callq	_printf
	movl	$10, %edi
	callq	_sub_equal
	movq	%rbx, %rdi
	movl	%eax, %esi
	xorl	%eax, %eax
	callq	_printf
	movl	$10, %edi
	callq	_mul_equal
	movq	%rbx, %rdi
	movl	%eax, %esi
	xorl	%eax, %eax
	callq	_printf
	movl	$10, %edi
	callq	_div_equal
	movq	%rbx, %rdi
	movl	%eax, %esi
	xorl	%eax, %eax
	callq	_printf
	xorl	%eax, %eax
	popq	%rbx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_add                    ## -- Begin function add
	.p2align	4, 0x90
_add:                                   ## @add
	.cfi_startproc
## %bb.0:                               ## %entry
                                        ## kill: def $esi killed $esi def $rsi
                                        ## kill: def $edi killed $edi def $rdi
	movl	%edi, -4(%rsp)
	movl	%esi, -8(%rsp)
	leal	(%rdi,%rsi), %eax
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_sub                    ## -- Begin function sub
	.p2align	4, 0x90
_sub:                                   ## @sub
	.cfi_startproc
## %bb.0:                               ## %entry
	movl	%edi, %eax
	movl	%edi, -4(%rsp)
	movl	%esi, -8(%rsp)
	subl	%esi, %eax
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_mul                    ## -- Begin function mul
	.p2align	4, 0x90
_mul:                                   ## @mul
	.cfi_startproc
## %bb.0:                               ## %entry
	movl	%edi, %eax
	movl	%edi, -4(%rsp)
	movl	%esi, -8(%rsp)
	imull	%esi, %eax
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_div                    ## -- Begin function div
	.p2align	4, 0x90
_div:                                   ## @div
	.cfi_startproc
## %bb.0:                               ## %entry
	movl	%edi, %eax
	movl	%edi, -4(%rsp)
	movl	%esi, -8(%rsp)
	cltd
	idivl	%esi
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_mod                    ## -- Begin function mod
	.p2align	4, 0x90
_mod:                                   ## @mod
	.cfi_startproc
## %bb.0:                               ## %entry
	movl	%edi, %eax
	movl	%edi, -4(%rsp)
	movl	%esi, -8(%rsp)
	cltd
	idivl	%esi
	movl	%edx, %eax
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_add_equal              ## -- Begin function add_equal
	.p2align	4, 0x90
_add_equal:                             ## @add_equal
	.cfi_startproc
## %bb.0:                               ## %entry
	movl	%edi, %eax
	addl	$5, %eax
	movl	%eax, -4(%rsp)
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_sub_equal              ## -- Begin function sub_equal
	.p2align	4, 0x90
_sub_equal:                             ## @sub_equal
	.cfi_startproc
## %bb.0:                               ## %entry
	movl	%edi, %eax
	addl	$-5, %eax
	movl	%eax, -4(%rsp)
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_mul_equal              ## -- Begin function mul_equal
	.p2align	4, 0x90
_mul_equal:                             ## @mul_equal
	.cfi_startproc
## %bb.0:                               ## %entry
                                        ## kill: def $edi killed $edi def $rdi
	leal	(%rdi,%rdi,4), %eax
	movl	%eax, -4(%rsp)
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_div_equal              ## -- Begin function div_equal
	.p2align	4, 0x90
_div_equal:                             ## @div_equal
	.cfi_startproc
## %bb.0:                               ## %entry
	movslq	%edi, %rax
	imulq	$1717986919, %rax, %rax ## imm = 0x66666667
	movq	%rax, %rcx
	shrq	$63, %rcx
	sarq	$33, %rax
	addl	%ecx, %eax
	movl	%eax, -4(%rsp)
                                        ## kill: def $eax killed $eax killed $rax
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
