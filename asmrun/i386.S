/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Asm part of the runtime system, Intel 386 processor */
/* Must be preprocessed by cpp */

#include "../config/m.h"

/* Linux/BSD with ELF binaries and Solaris do not prefix identifiers with _.
   Linux/BSD with a.out binaries and NextStep do. */

#if (defined(SYS_solaris) && !defined(__GNUC__))
#define CONCAT(a,b) a/**/b
#else
#define CONCAT(a,b) a##b
#endif

#if defined(SYS_linux_elf) || defined(SYS_bsd_elf) \
 || defined(SYS_solaris) || defined(SYS_beos) || defined(SYS_gnu)
#define G(x) x
#define LBL(x) CONCAT(.L,x)
#else
#define G(x) CONCAT(_,x)
#define LBL(x) CONCAT(L,x)
#endif

#if defined(SYS_linux_elf) || defined(SYS_bsd_elf) \
 || defined(SYS_solaris) || defined(SYS_beos) || defined(SYS_cygwin) \
 || defined(SYS_mingw) || defined(SYS_gnu)
#define FUNCTION_ALIGN 4
#else
#define FUNCTION_ALIGN 2
#endif

#define FUNCTION(name) \
        .globl G(name); \
        .align FUNCTION_ALIGN; \
        G(name):

#ifdef ASM_CFI_SUPPORTED
#define CFI_STARTPROC .cfi_startproc
#define CFI_ENDPROC .cfi_endproc
#define CFI_ADJUST(n) .cfi_adjust_cfa_offset n
#else
#define CFI_STARTPROC
#define CFI_ENDPROC
#define CFI_ADJUST(n)
#endif

#if defined(PROFILING)
#if defined(SYS_linux_elf) || defined(SYS_gnu)
#define PROFILE_CAML \
        pushl %ebp; CFI_ADJUST(4); \
        movl %esp, %ebp; \
        pushl %eax; CFI_ADJUST(4); \
        pushl %ecx; CFI_ADJUST(4); \
        pushl %edx; CFI_ADJUST(4); \
        call mcount; \
        popl %edx; CFI_ADJUST(-4); \
        popl %ecx; CFI_ADJUST(-4); \
        popl %eax; CFI_ADJUST(-4); \
        popl %ebp; CFI_ADJUST(-4)
#define PROFILE_C \
        pushl %ebp; CFI_ADJUST(4); \
        movl %esp, %ebp; \
        call mcount; \
        popl %ebp; CFI_ADJUST(-4)
#elif defined(SYS_bsd_elf)
#define PROFILE_CAML \
        pushl %ebp; CFI_ADJUST(4); \
        movl %esp, %ebp; \
        pushl %eax; CFI_ADJUST(4); \
        pushl %ecx; CFI_ADJUST(4); \
        pushl %edx; CFI_ADJUST(4); \
        call .mcount; \
        popl %edx; CFI_ADJUST(-4); \
        popl %ecx; CFI_ADJUST(-4); \
        popl %eax; CFI_ADJUST(-4); \
        popl %ebp; CFI_ADJUST(-4)
#define PROFILE_C \
        pushl %ebp; CFI_ADJUST(4); \
        movl %esp, %ebp; \
        call .mcount; \
        popl %ebp; CFI_ADJUST(-4)
#elif defined(SYS_macosx)
#define PROFILE_CAML \
        pushl %ebp; CFI_ADJUST(4); \
        movl %esp, %ebp; \
        pushl %eax; CFI_ADJUST(4); \
        pushl %ecx; CFI_ADJUST(4); \
        pushl %edx; CFI_ADJUST(4); \
        call Lmcount$stub;  \
        popl %edx; CFI_ADJUST(-4); \
        popl %ecx; CFI_ADJUST(-4); \
        popl %eax; CFI_ADJUST(-4); \
        popl %ebp; CFI_ADJUST(-4)
#define PROFILE_C \
        pushl %ebp; CFI_ADJUST(4); \
        movl %esp, %ebp; \
        call Lmcount$stub; \
        popl %ebp; CFI_ADJUST(-4)
#endif
#else
#define PROFILE_CAML
#define PROFILE_C
#endif

/* PR#6038: GCC and Clang seem to require 16-byte alignment nowadays,
   even if only MacOS X's ABI formally requires it. */	
#define ALIGN_STACK(amount) subl $ amount, %esp ; CFI_ADJUST(amount)
#define UNDO_ALIGN_STACK(amount) addl $ amount, %esp ; CFI_ADJUST(-amount)

/* Allocation */

        .text
        .globl  G(caml_system__code_begin)
G(caml_system__code_begin):

FUNCTION(caml_call_gc)
        CFI_STARTPROC
        PROFILE_CAML
    /* Record lowest stack address and return address */
        movl    0(%esp), %eax
        movl    %eax, G(caml_last_return_address)
        leal    4(%esp), %eax
        movl    %eax, G(caml_bottom_of_stack)
LBL(105):
#if !defined(SYS_mingw) && !defined(SYS_cygwin)
    /* Touch the stack to trigger a recoverable segfault
       if insufficient space remains */
        subl    $16384, %esp
        movl    %eax, 0(%esp)
        addl    $16384, %esp
#endif
    /* Build array of registers, save it into caml_gc_regs */
        pushl   %ebp; CFI_ADJUST(4)
        pushl   %edi; CFI_ADJUST(4)
        pushl   %esi; CFI_ADJUST(4)
        pushl   %edx; CFI_ADJUST(4)
        pushl   %ecx; CFI_ADJUST(4)
        pushl   %ebx; CFI_ADJUST(4)
        pushl   %eax; CFI_ADJUST(4)
        movl    %esp, G(caml_gc_regs)
        /* MacOSX note: 16-alignment of stack preserved at this point */
    /* Call the garbage collector */
        call    G(caml_garbage_collection)
    /* Restore all regs used by the code generator */
        popl    %eax; CFI_ADJUST(-4)
        popl    %ebx; CFI_ADJUST(-4)
        popl    %ecx; CFI_ADJUST(-4)
        popl    %edx; CFI_ADJUST(-4)
        popl    %esi; CFI_ADJUST(-4)
        popl    %edi; CFI_ADJUST(-4)
        popl    %ebp; CFI_ADJUST(-4)
    /* Return to caller */
        ret
        CFI_ENDPROC

FUNCTION(caml_alloc1)
        CFI_STARTPROC
        PROFILE_CAML
        movl    G(caml_young_ptr), %eax
        subl    $8, %eax
        movl    %eax, G(caml_young_ptr)
        cmpl    G(caml_young_limit), %eax
        jb      LBL(100)
        ret
LBL(100):
        movl    0(%esp), %eax
        movl    %eax, G(caml_last_return_address)
        leal    4(%esp), %eax
        movl    %eax, G(caml_bottom_of_stack)
        ALIGN_STACK(12)
        call    LBL(105)
        UNDO_ALIGN_STACK(12)
        jmp     G(caml_alloc1)
        CFI_ENDPROC

FUNCTION(caml_alloc2)
        CFI_STARTPROC
        PROFILE_CAML
        movl    G(caml_young_ptr), %eax
        subl    $12, %eax
        movl    %eax, G(caml_young_ptr)
        cmpl    G(caml_young_limit), %eax
        jb      LBL(101)
        ret
LBL(101):
        movl    0(%esp), %eax
        movl    %eax, G(caml_last_return_address)
        leal    4(%esp), %eax
        movl    %eax, G(caml_bottom_of_stack)
        ALIGN_STACK(12)
        call    LBL(105)
        UNDO_ALIGN_STACK(12)
        jmp     G(caml_alloc2)
        CFI_ENDPROC

FUNCTION(caml_alloc3)
        CFI_STARTPROC
        PROFILE_CAML
        movl    G(caml_young_ptr), %eax
        subl    $16, %eax
        movl    %eax, G(caml_young_ptr)
        cmpl    G(caml_young_limit), %eax
        jb      LBL(102)
        ret
LBL(102):
        movl    0(%esp), %eax
        movl    %eax, G(caml_last_return_address)
        leal    4(%esp), %eax
        movl    %eax, G(caml_bottom_of_stack)
        ALIGN_STACK(12)
        call    LBL(105)
        UNDO_ALIGN_STACK(12)
        jmp     G(caml_alloc3)
        CFI_ENDPROC

FUNCTION(caml_allocN)
        CFI_STARTPROC
        PROFILE_CAML
        subl    G(caml_young_ptr), %eax /* eax = size - caml_young_ptr */
        negl    %eax                    /* eax = caml_young_ptr - size */
        cmpl    G(caml_young_limit), %eax
        jb      LBL(103)
        movl    %eax, G(caml_young_ptr)
        ret
LBL(103):
        subl    G(caml_young_ptr), %eax /* eax = - size */
        negl    %eax                    /* eax = size */
        pushl   %eax; CFI_ADJUST(4)     /* save desired size */
        subl    %eax, G(caml_young_ptr) /* must update young_ptr */
        movl    4(%esp), %eax
        movl    %eax, G(caml_last_return_address)
        leal    8(%esp), %eax
        movl    %eax, G(caml_bottom_of_stack)
        ALIGN_STACK(8)
        call    LBL(105)
        UNDO_ALIGN_STACK(8)
        popl    %eax; CFI_ADJUST(-4)    /* recover desired size */
        jmp     G(caml_allocN)
        CFI_ENDPROC

/* Call a C function from OCaml */

FUNCTION(caml_c_call)
        CFI_STARTPROC
        PROFILE_CAML
    /* Record lowest stack address and return address */
        movl    (%esp), %edx
        movl    %edx, G(caml_last_return_address)
        leal    4(%esp), %edx
        movl    %edx, G(caml_bottom_of_stack)
#if !defined(SYS_mingw) && !defined(SYS_cygwin)
    /* Touch the stack to trigger a recoverable segfault
       if insufficient space remains */
        subl    $16384, %esp
        movl    %eax, 0(%esp)
        addl    $16384, %esp
#endif
    /* Call the function (address in %eax) */
        jmp     *%eax
        CFI_ENDPROC

/* Start the OCaml program */

FUNCTION(caml_start_program)
        CFI_STARTPROC
        PROFILE_C
    /* Save callee-save registers */
        pushl   %ebx; CFI_ADJUST(4)
        pushl   %esi; CFI_ADJUST(4)
        pushl   %edi; CFI_ADJUST(4)
        pushl   %ebp; CFI_ADJUST(4)
    /* Initial entry point is caml_program */
        movl    $ G(caml_program), %esi
    /* Common code for caml_start_program and caml_callback* */
LBL(106):
    /* Build a callback link */
        pushl   G(caml_gc_regs); CFI_ADJUST(4)
        pushl   G(caml_last_return_address); CFI_ADJUST(4)
        pushl   G(caml_bottom_of_stack); CFI_ADJUST(4)
        /* Note: 16-alignment preserved on MacOSX at this point */
    /* Build an exception handler */
        pushl   $ LBL(108); CFI_ADJUST(4)
        ALIGN_STACK(8)
        pushl   G(caml_exception_pointer); CFI_ADJUST(4)
        movl    %esp, G(caml_exception_pointer)
    /* Call the OCaml code */
        call    *%esi
LBL(107):
    /* Pop the exception handler */
        popl    G(caml_exception_pointer); CFI_ADJUST(-4)
        addl    $12, %esp       ; CFI_ADJUST(-12)
LBL(109):
    /* Pop the callback link, restoring the global variables */
        popl    G(caml_bottom_of_stack); CFI_ADJUST(-4)
        popl    G(caml_last_return_address); CFI_ADJUST(-4)
        popl    G(caml_gc_regs); CFI_ADJUST(-4)
    /* Restore callee-save registers. */
        popl    %ebp; CFI_ADJUST(-4)
        popl    %edi; CFI_ADJUST(-4)
        popl    %esi; CFI_ADJUST(-4)
        popl    %ebx; CFI_ADJUST(-4)
    /* Return to caller. */
        ret
LBL(108):
    /* Exception handler*/
    /* Mark the bucket as an exception result and return it */
        orl     $2, %eax
        jmp     LBL(109)
        CFI_ENDPROC

/* Raise an exception from OCaml */

FUNCTION(caml_raise_exn)
        CFI_STARTPROC
        testl   $1, G(caml_backtrace_active)
        jne     LBL(110)
        movl    G(caml_exception_pointer), %esp
        popl    G(caml_exception_pointer); CFI_ADJUST(-4)
        UNDO_ALIGN_STACK(8)
        ret
LBL(110):
        movl    $0, G(caml_backtrace_pos)
LBL(111):
        movl    %eax, %esi          /* Save exception bucket in esi */
        movl    G(caml_exception_pointer), %edi /* SP of handler */
        movl    0(%esp), %eax       /* PC of raise */
        leal    4(%esp), %edx       /* SP of raise */
        ALIGN_STACK(12)
        pushl   %edi; CFI_ADJUST(4)         /* arg 4: sp of handler */
        pushl   %edx; CFI_ADJUST(4)         /* arg 3: sp of raise */
        pushl   %eax; CFI_ADJUST(4)         /* arg 2: pc of raise */
        pushl   %esi; CFI_ADJUST(4)         /* arg 1: exception bucket */
        call    G(caml_stash_backtrace)
        movl    %esi, %eax              /* Recover exception bucket */
        movl    %edi, %esp
        popl    G(caml_exception_pointer); CFI_ADJUST(-4)
        UNDO_ALIGN_STACK(8)
        ret
        CFI_ENDPROC

FUNCTION(caml_reraise_exn)
        CFI_STARTPROC
        testl   $1, G(caml_backtrace_active)
        jne     LBL(111)
        movl    G(caml_exception_pointer), %esp
        popl    G(caml_exception_pointer); CFI_ADJUST(-4)
        UNDO_ALIGN_STACK(8)
        ret
        CFI_ENDPROC

/* Raise an exception from C */

FUNCTION(caml_raise_exception)
        CFI_STARTPROC
        PROFILE_C
        testl   $1, G(caml_backtrace_active)
        jne     LBL(112)
        movl    4(%esp), %eax
        movl    G(caml_exception_pointer), %esp
        popl    G(caml_exception_pointer); CFI_ADJUST(-4)
        UNDO_ALIGN_STACK(8)
        ret
LBL(112):
        movl    4(%esp), %esi          /* Save exception bucket in esi */
        ALIGN_STACK(12)
        pushl   G(caml_exception_pointer); CFI_ADJUST(4)  /* 4: sp of handler */
        pushl   G(caml_bottom_of_stack); CFI_ADJUST(4)    /* 3: sp of raise */
        pushl   G(caml_last_return_address); CFI_ADJUST(4)/* 2: pc of raise */
        pushl   %esi; CFI_ADJUST(4)                    /* 1: exception bucket */
        call    G(caml_stash_backtrace)
        movl    %esi, %eax              /* Recover exception bucket */
        movl    G(caml_exception_pointer), %esp
        popl    G(caml_exception_pointer); CFI_ADJUST(-4)
        UNDO_ALIGN_STACK(8)
        ret
        CFI_ENDPROC

/* Callback from C to OCaml */

FUNCTION(caml_callback_exn)
        CFI_STARTPROC
        PROFILE_C
    /* Save callee-save registers */
        pushl   %ebx; CFI_ADJUST(4)
        pushl   %esi; CFI_ADJUST(4)
        pushl   %edi; CFI_ADJUST(4)
        pushl   %ebp; CFI_ADJUST(4)
    /* Initial loading of arguments */
        movl    20(%esp), %ebx   /* closure */
        movl    24(%esp), %eax   /* argument */
        movl    0(%ebx), %esi    /* code pointer */
        jmp     LBL(106)
        CFI_ENDPROC

FUNCTION(caml_callback2_exn)
        CFI_STARTPROC
        PROFILE_C
    /* Save callee-save registers */
        pushl   %ebx; CFI_ADJUST(4)
        pushl   %esi; CFI_ADJUST(4)
        pushl   %edi; CFI_ADJUST(4)
        pushl   %ebp; CFI_ADJUST(4)
    /* Initial loading of arguments */
        movl    20(%esp), %ecx   /* closure */
        movl    24(%esp), %eax   /* first argument */
        movl    28(%esp), %ebx   /* second argument */
        movl    $ G(caml_apply2), %esi   /* code pointer */
        jmp     LBL(106)
        CFI_ENDPROC

FUNCTION(caml_callback3_exn)
        CFI_STARTPROC
        PROFILE_C
    /* Save callee-save registers */
        pushl   %ebx; CFI_ADJUST(4)
        pushl   %esi; CFI_ADJUST(4)
        pushl   %edi; CFI_ADJUST(4)
        pushl   %ebp; CFI_ADJUST(4)
    /* Initial loading of arguments */
        movl    20(%esp), %edx   /* closure */
        movl    24(%esp), %eax   /* first argument */
        movl    28(%esp), %ebx   /* second argument */
        movl    32(%esp), %ecx   /* third argument */
        movl    $ G(caml_apply3), %esi   /* code pointer */
        jmp     LBL(106)
        CFI_ENDPROC

FUNCTION(caml_ml_array_bound_error)
        CFI_STARTPROC
    /* Empty the floating-point stack */
        ffree   %st(0)
        ffree   %st(1)
        ffree   %st(2)
        ffree   %st(3)
        ffree   %st(4)
        ffree   %st(5)
        ffree   %st(6)
        ffree   %st(7)
    /* Record lowest stack address and return address */
        movl    (%esp), %edx
        movl    %edx, G(caml_last_return_address)
        leal    4(%esp), %edx
        movl    %edx, G(caml_bottom_of_stack)
    /* Re-align the stack */
        andl    $-16, %esp
    /* Branch to [caml_array_bound_error] (never returns) */
        call    G(caml_array_bound_error)
        CFI_ENDPROC

        .globl  G(caml_system__code_end)
G(caml_system__code_end):

        .data
        .globl  G(caml_system__frametable)
G(caml_system__frametable):
        .long   1               /* one descriptor */
        .long   LBL(107)        /* return address into callback */
#ifndef SYS_solaris
        .word   -1              /* negative frame size => use callback link */
        .word   0               /* no roots here */
#else
        .value  -1              /* negative frame size => use callback link */
        .value  0               /* no roots here */
#endif

        .globl  G(caml_extra_params)
G(caml_extra_params):
#ifndef SYS_solaris
        .space  64
#else
        .zero   64
#endif

#if defined(PROFILING) && defined(SYS_macosx)
        .section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
Lmcount$stub:
        .indirect_symbol mcount
        hlt ; hlt ; hlt ; hlt ; hlt
        .subsections_via_symbols
#endif

#if defined(SYS_linux_elf)
    /* Mark stack as non-executable, PR#4564 */
        .section .note.GNU-stack,"",%progbits
#endif
