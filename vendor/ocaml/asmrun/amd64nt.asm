;***********************************************************************
;*                                                                     *
;*                                OCaml                                *
;*                                                                     *
;*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         *
;*                                                                     *
;*  Copyright 2006 Institut National de Recherche en Informatique et   *
;*  en Automatique.  All rights reserved.  This file is distributed    *
;*  under the terms of the GNU Library General Public License, with    *
;*  the special exception on linking described in file ../LICENSE.     *
;*                                                                     *
;***********************************************************************

; Asm part of the runtime system, AMD64 processor, Intel syntax

; Notes on Win64 calling conventions:
;     function arguments in RCX, RDX, R8, R9 / XMM0 - XMM3
;     caller must reserve 32 bytes of stack space
;     callee must preserve RBX, RBP, RSI, RDI, R12-R15, XMM6-XMM15

        EXTRN  caml_garbage_collection: NEAR
        EXTRN  caml_apply2: NEAR
        EXTRN  caml_apply3: NEAR
        EXTRN  caml_program: NEAR
        EXTRN  caml_array_bound_error: NEAR
        EXTRN  caml_young_limit: QWORD
        EXTRN  caml_young_ptr: QWORD
        EXTRN  caml_bottom_of_stack: QWORD
        EXTRN  caml_last_return_address: QWORD
        EXTRN  caml_gc_regs: QWORD
        EXTRN  caml_exception_pointer: QWORD
        EXTRN  caml_backtrace_pos: DWORD
        EXTRN  caml_backtrace_active: DWORD
        EXTRN  caml_stash_backtrace: NEAR

        .CODE

; Allocation

        PUBLIC  caml_call_gc
        ALIGN   16
caml_call_gc:
    ; Record lowest stack address and return address
        mov     rax, [rsp]
        mov     caml_last_return_address, rax
        lea     rax, [rsp+8]
        mov     caml_bottom_of_stack, rax
L105:
    ; Save caml_young_ptr, caml_exception_pointer
        mov     caml_young_ptr, r15
        mov     caml_exception_pointer, r14
    ; Build array of registers, save it into caml_gc_regs
        push    rbp
        push    r11
        push    r10
        push    r13
        push    r12
        push    r9
        push    r8
        push    rcx
        push    rdx
        push    rsi
        push    rdi
        push    rbx
        push    rax
        mov     caml_gc_regs, rsp
    ; Save floating-point registers
        sub     rsp, 16*8
        movsd   QWORD PTR [rsp + 0*8], xmm0
        movsd   QWORD PTR [rsp + 1*8], xmm1
        movsd   QWORD PTR [rsp + 2*8], xmm2
        movsd   QWORD PTR [rsp + 3*8], xmm3
        movsd   QWORD PTR [rsp + 4*8], xmm4
        movsd   QWORD PTR [rsp + 5*8], xmm5
        movsd   QWORD PTR [rsp + 6*8], xmm6
        movsd   QWORD PTR [rsp + 7*8], xmm7
        movsd   QWORD PTR [rsp + 8*8], xmm8
        movsd   QWORD PTR [rsp + 9*8], xmm9
        movsd   QWORD PTR [rsp + 10*8], xmm10
        movsd   QWORD PTR [rsp + 11*8], xmm11
        movsd   QWORD PTR [rsp + 12*8], xmm12
        movsd   QWORD PTR [rsp + 13*8], xmm13
        movsd   QWORD PTR [rsp + 14*8], xmm14
        movsd   QWORD PTR [rsp + 15*8], xmm15
    ; Call the garbage collector
        sub rsp, 32      ; PR#5008: bottom 32 bytes are reserved for callee
        call caml_garbage_collection
        add rsp, 32      ; PR#5008
    ; Restore all regs used by the code generator
        movsd   xmm0, QWORD PTR [rsp + 0*8]
        movsd   xmm1, QWORD PTR [rsp + 1*8]
        movsd   xmm2, QWORD PTR [rsp + 2*8]
        movsd   xmm3, QWORD PTR [rsp + 3*8]
        movsd   xmm4, QWORD PTR [rsp + 4*8]
        movsd   xmm5, QWORD PTR [rsp + 5*8]
        movsd   xmm6, QWORD PTR [rsp + 6*8]
        movsd   xmm7, QWORD PTR [rsp + 7*8]
        movsd   xmm8, QWORD PTR [rsp + 8*8]
        movsd   xmm9, QWORD PTR [rsp + 9*8]
        movsd   xmm10, QWORD PTR [rsp + 10*8]
        movsd   xmm11, QWORD PTR [rsp + 11*8]
        movsd   xmm12, QWORD PTR [rsp + 12*8]
        movsd   xmm13, QWORD PTR [rsp + 13*8]
        movsd   xmm14, QWORD PTR [rsp + 14*8]
        movsd   xmm15, QWORD PTR [rsp + 15*8]
        add     rsp, 16*8
        pop     rax
        pop     rbx
        pop     rdi
        pop     rsi
        pop     rdx
        pop     rcx
        pop     r8
        pop     r9
        pop     r12
        pop     r13
        pop     r10
        pop     r11
        pop     rbp
    ; Restore caml_young_ptr, caml_exception_pointer
        mov     r15, caml_young_ptr
        mov     r14, caml_exception_pointer
    ; Return to caller
        ret

        PUBLIC  caml_alloc1
        ALIGN   16
caml_alloc1:
        sub     r15, 16
        cmp     r15, caml_young_limit
        jb      L100
        ret
L100:
        mov     rax, [rsp + 0]
        mov     caml_last_return_address, rax
        lea     rax, [rsp + 8]
        mov     caml_bottom_of_stack, rax
        sub     rsp, 8
        call    L105
        add     rsp, 8
        jmp     caml_alloc1

        PUBLIC  caml_alloc2
        ALIGN   16
caml_alloc2:
        sub     r15, 24
        cmp     r15, caml_young_limit
        jb      L101
        ret
L101:
        mov     rax, [rsp + 0]
        mov     caml_last_return_address, rax
        lea     rax, [rsp + 8]
        mov     caml_bottom_of_stack, rax
        sub     rsp, 8
        call    L105
        add     rsp, 8
        jmp     caml_alloc2

        PUBLIC  caml_alloc3
        ALIGN   16
caml_alloc3:
        sub     r15, 32
        cmp     r15, caml_young_limit
        jb      L102
        ret
L102:
        mov     rax, [rsp + 0]
        mov     caml_last_return_address, rax
        lea     rax, [rsp + 8]
        mov     caml_bottom_of_stack, rax
        sub     rsp, 8
        call    L105
        add     rsp, 8
        jmp     caml_alloc3

        PUBLIC  caml_allocN
        ALIGN   16
caml_allocN:
        sub     r15, rax
        cmp     r15, caml_young_limit
        jb      L103
        ret
L103:
        push    rax                       ; save desired size
        mov     rax, [rsp + 8]
        mov     caml_last_return_address, rax
        lea     rax, [rsp + 16]
        mov     caml_bottom_of_stack, rax
        call    L105
        pop     rax                      ; recover desired size
        jmp     caml_allocN

; Call a C function from OCaml

        PUBLIC  caml_c_call
        ALIGN   16
caml_c_call:
    ; Record lowest stack address and return address
        pop     r12
        mov     caml_last_return_address, r12
        mov     caml_bottom_of_stack, rsp
    ; Make the exception handler and alloc ptr available to the C code
        mov     caml_young_ptr, r15
        mov     caml_exception_pointer, r14
    ; Call the function (address in rax)
        call    rax
    ; Reload alloc ptr
        mov     r15, caml_young_ptr
    ; Return to caller
        push    r12
        ret

; Start the OCaml program

        PUBLIC  caml_start_program
        ALIGN   16
caml_start_program:
    ; Save callee-save registers
        push    rbx
        push    rbp
        push    rsi
        push    rdi
        push    r12
        push    r13
        push    r14
        push    r15
        sub     rsp, 8+10*16       ; stack 16-aligned + 10 saved xmm regs
        movapd  OWORD PTR [rsp + 0*16], xmm6
        movapd  OWORD PTR [rsp + 1*16], xmm7
        movapd  OWORD PTR [rsp + 2*16], xmm8
        movapd  OWORD PTR [rsp + 3*16], xmm9
        movapd  OWORD PTR [rsp + 4*16], xmm10
        movapd  OWORD PTR [rsp + 5*16], xmm11
        movapd  OWORD PTR [rsp + 6*16], xmm12
        movapd  OWORD PTR [rsp + 7*16], xmm13
        movapd  OWORD PTR [rsp + 8*16], xmm14
        movapd  OWORD PTR [rsp + 9*16], xmm15
    ; Initial entry point is caml_program
        lea     r12, caml_program
    ; Common code for caml_start_program and caml_callback*
L106:
    ; Build a callback link
        sub     rsp, 8  ; stack 16-aligned
        push    caml_gc_regs
        push    caml_last_return_address
        push    caml_bottom_of_stack
    ; Setup alloc ptr and exception ptr
        mov     r15, caml_young_ptr
        mov     r14, caml_exception_pointer
    ; Build an exception handler
        lea     r13, L108
        push    r13
        push    r14
        mov     r14, rsp
    ; Call the OCaml code
        call    r12
L107:
    ; Pop the exception handler
        pop     r14
        pop     r12    ; dummy register
L109:
    ; Update alloc ptr and exception ptr
        mov     caml_young_ptr, r15
        mov     caml_exception_pointer, r14
    ; Pop the callback restoring, link the global variables
        pop     caml_bottom_of_stack
        pop     caml_last_return_address
        pop     caml_gc_regs
        add     rsp, 8
    ; Restore callee-save registers.
        movapd  xmm6, OWORD PTR [rsp + 0*16]
        movapd  xmm7, OWORD PTR [rsp + 1*16]
        movapd  xmm8, OWORD PTR [rsp + 2*16]
        movapd  xmm9, OWORD PTR [rsp + 3*16]
        movapd  xmm10, OWORD PTR [rsp + 4*16]
        movapd  xmm11, OWORD PTR [rsp + 5*16]
        movapd  xmm12, OWORD PTR [rsp + 6*16]
        movapd  xmm13, OWORD PTR [rsp + 7*16]
        movapd  xmm14, OWORD PTR [rsp + 8*16]
        movapd  xmm15, OWORD PTR [rsp + 9*16]
        add     rsp, 8+10*16
        pop     r15
        pop     r14
        pop     r13
        pop     r12
        pop     rdi
        pop     rsi
        pop     rbp
        pop     rbx
    ; Return to caller
        ret
L108:
    ; Exception handler
    ; Mark the bucket as an exception result and return it
        or      rax, 2
        jmp     L109

; Raise an exception from OCaml

        PUBLIC  caml_raise_exn
        ALIGN   16
caml_raise_exn:
        test    caml_backtrace_active, 1
        jne     L110
        mov     rsp, r14             ; Cut stack
        pop     r14                  ; Recover previous exception handler
        ret                          ; Branch to handler
L110:
        mov     caml_backtrace_pos, 0
L111:
        mov     r12, rax             ; Save exception bucket in r12
        mov     rcx, rax             ; Arg 1: exception bucket
        mov     rdx, [rsp]           ; Arg 2: PC of raise
        lea     r8, [rsp+8]          ; Arg 3: SP of raise
        mov     r9, r14              ; Arg 4: SP of handler
        sub     rsp, 32              ; Reserve 32 bytes on stack
        call    caml_stash_backtrace
        mov     rax, r12             ; Recover exception bucket
        mov     rsp, r14             ; Cut stack
        pop     r14                  ; Recover previous exception handler
        ret                          ; Branch to handler

        PUBLIC  caml_reraise_exn
        ALIGN   16
caml_reraise_exn:
        test    caml_backtrace_active, 1
        jne     L111
        mov     rsp, r14             ; Cut stack
        pop     r14                  ; Recover previous exception handler
        ret                          ; Branch to handler

; Raise an exception from C

        PUBLIC  caml_raise_exception
        ALIGN   16
caml_raise_exception:
        test    caml_backtrace_active, 1
        jne     L112
        mov     rax, rcx             ; First argument is exn bucket
        mov     rsp, caml_exception_pointer
        pop     r14                  ; Recover previous exception handler
        mov     r15, caml_young_ptr ; Reload alloc ptr
        ret
L112:
        mov     r12, rcx             ; Save exception bucket in r12
                                     ; Arg 1: exception bucket
        mov     rdx, caml_last_return_address ; Arg 2: PC of raise
        mov     r8, caml_bottom_of_stack      ; Arg 3: SP of raise
        mov     r9, caml_exception_pointer    ; Arg 4: SP of handler
        sub     rsp, 32              ; Reserve 32 bytes on stack
        call    caml_stash_backtrace
        mov     rax, r12             ; Recover exception bucket
        mov     rsp, caml_exception_pointer
        pop     r14                  ; Recover previous exception handler
        mov     r15, caml_young_ptr ; Reload alloc ptr
        ret

; Callback from C to OCaml

        PUBLIC  caml_callback_exn
        ALIGN   16
caml_callback_exn:
    ; Save callee-save registers
        push    rbx
        push    rbp
        push    rsi
        push    rdi
        push    r12
        push    r13
        push    r14
        push    r15
        sub     rsp, 8+10*16       ; stack 16-aligned + 10 saved xmm regs
        movapd  OWORD PTR [rsp + 0*16], xmm6
        movapd  OWORD PTR [rsp + 1*16], xmm7
        movapd  OWORD PTR [rsp + 2*16], xmm8
        movapd  OWORD PTR [rsp + 3*16], xmm9
        movapd  OWORD PTR [rsp + 4*16], xmm10
        movapd  OWORD PTR [rsp + 5*16], xmm11
        movapd  OWORD PTR [rsp + 6*16], xmm12
        movapd  OWORD PTR [rsp + 7*16], xmm13
        movapd  OWORD PTR [rsp + 8*16], xmm14
        movapd  OWORD PTR [rsp + 9*16], xmm15
    ; Initial loading of arguments
        mov     rbx, rcx      ; closure
        mov     rax, rdx      ; argument
        mov     r12, [rbx]    ; code pointer
        jmp     L106

        PUBLIC  caml_callback2_exn
        ALIGN   16
caml_callback2_exn:
    ; Save callee-save registers
        push    rbx
        push    rbp
        push    rsi
        push    rdi
        push    r12
        push    r13
        push    r14
        push    r15
        sub     rsp, 8+10*16       ; stack 16-aligned + 10 saved xmm regs
        movapd  OWORD PTR [rsp + 0*16], xmm6
        movapd  OWORD PTR [rsp + 1*16], xmm7
        movapd  OWORD PTR [rsp + 2*16], xmm8
        movapd  OWORD PTR [rsp + 3*16], xmm9
        movapd  OWORD PTR [rsp + 4*16], xmm10
        movapd  OWORD PTR [rsp + 5*16], xmm11
        movapd  OWORD PTR [rsp + 6*16], xmm12
        movapd  OWORD PTR [rsp + 7*16], xmm13
        movapd  OWORD PTR [rsp + 8*16], xmm14
        movapd  OWORD PTR [rsp + 9*16], xmm15
    ; Initial loading of arguments
        mov     rdi, rcx        ; closure
        mov     rax, rdx        ; first argument
        mov     rbx, r8         ; second argument
        lea     r12, caml_apply2  ; code pointer
        jmp     L106

        PUBLIC  caml_callback3_exn
        ALIGN   16
caml_callback3_exn:
    ; Save callee-save registers
        push    rbx
        push    rbp
        push    rsi
        push    rdi
        push    r12
        push    r13
        push    r14
        push    r15
        sub     rsp, 8+10*16       ; stack 16-aligned + 10 saved xmm regs
        movapd  OWORD PTR [rsp + 0*16], xmm6
        movapd  OWORD PTR [rsp + 1*16], xmm7
        movapd  OWORD PTR [rsp + 2*16], xmm8
        movapd  OWORD PTR [rsp + 3*16], xmm9
        movapd  OWORD PTR [rsp + 4*16], xmm10
        movapd  OWORD PTR [rsp + 5*16], xmm11
        movapd  OWORD PTR [rsp + 6*16], xmm12
        movapd  OWORD PTR [rsp + 7*16], xmm13
        movapd  OWORD PTR [rsp + 8*16], xmm14
        movapd  OWORD PTR [rsp + 9*16], xmm15
    ; Initial loading of arguments
        mov     rsi, rcx        ; closure
        mov     rax, rdx        ; first argument
        mov     rbx, r8         ; second argument
        mov     rdi, r9         ; third argument
        lea     r12, caml_apply3      ; code pointer
        jmp     L106

        PUBLIC  caml_ml_array_bound_error
        ALIGN   16
caml_ml_array_bound_error:
        lea     rax, caml_array_bound_error
        jmp     caml_c_call

        .DATA
        PUBLIC  caml_system__frametable
caml_system__frametable LABEL QWORD
        QWORD   1           ; one descriptor
        QWORD   L107        ; return address into callback
        WORD    -1          ; negative frame size => use callback link
        WORD    0           ; no roots here
        ALIGN   8

        PUBLIC  caml_negf_mask
        ALIGN   16
caml_negf_mask LABEL QWORD
        QWORD   8000000000000000H, 0

        PUBLIC  caml_absf_mask
        ALIGN   16
caml_absf_mask LABEL QWORD
        QWORD   7FFFFFFFFFFFFFFFH, 0FFFFFFFFFFFFFFFFH

        END
