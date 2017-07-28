/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

#ifdef SYS_macosx
#define ALIGN 4
#else
#define ALIGN 16
#endif

#ifdef SYS_macosx
#define CALL_GEN_CODE _call_gen_code
#define CAML_C_CALL _caml_c_call
#define CAML_NEGF_MASK _caml_negf_mask
#define CAML_ABSF_MASK _caml_absf_mask
#else
#define CALL_GEN_CODE call_gen_code
#define CAML_C_CALL caml_c_call
#define CAML_NEGF_MASK caml_negf_mask
#define CAML_ABSF_MASK caml_absf_mask
#endif

        .globl  CALL_GEN_CODE
        .align  ALIGN
CALL_GEN_CODE:
        pushq   %rbx
        pushq   %rbp
        pushq   %r12
        pushq   %r13
        pushq   %r14
        pushq   %r15
        movq    %rdi, %r10
        movq    %rsi, %rax
        movq    %rdx, %rbx
        movq    %rcx, %rdi
        movq    %r8, %rsi
        call    *%r10
        popq    %r15
        popq    %r14
        popq    %r13
        popq    %r12
        popq    %rbp
        popq    %rbx
        ret

        .globl  CAML_C_CALL
        .align  ALIGN
CAML_C_CALL:
        jmp     *%rax

#ifdef SYS_macosx
        .literal16
#elif defined(SYS_mingw64) || defined(SYS_cygwin)
        .section        .rodata.cst8
#else
        .section        .rodata.cst8,"aM",@progbits,8
#endif
        .globl  CAML_NEGF_MASK
        .align  ALIGN
CAML_NEGF_MASK:
        .quad   0x8000000000000000, 0
        .globl  CAML_ABSF_MASK
        .align  ALIGN
CAML_ABSF_MASK:
        .quad   0x7FFFFFFFFFFFFFFF, 0

        .comm   young_limit, 8
