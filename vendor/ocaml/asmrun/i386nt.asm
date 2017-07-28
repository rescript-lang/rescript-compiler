;***********************************************************************
;*                                                                     *
;*                                OCaml                                *
;*                                                                     *
;*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *
;*                                                                     *
;*  Copyright 1996 Institut National de Recherche en Informatique et   *
;*  en Automatique.  All rights reserved.  This file is distributed    *
;*  under the terms of the GNU Library General Public License, with    *
;*  the special exception on linking described in file ../LICENSE.     *
;*                                                                     *
;***********************************************************************

; Asm part of the runtime system, Intel 386 processor, Intel syntax

        .386
        .MODEL FLAT

        EXTERN  _caml_garbage_collection: PROC
        EXTERN  _caml_apply2: PROC
        EXTERN  _caml_apply3: PROC
        EXTERN  _caml_program: PROC
        EXTERN  _caml_array_bound_error: PROC
        EXTERN  _caml_young_limit: DWORD
        EXTERN  _caml_young_ptr: DWORD
        EXTERN  _caml_bottom_of_stack: DWORD
        EXTERN  _caml_last_return_address: DWORD
        EXTERN  _caml_gc_regs: DWORD
        EXTERN  _caml_exception_pointer: DWORD
        EXTERN  _caml_backtrace_pos: DWORD
        EXTERN  _caml_backtrace_active: DWORD
        EXTERN  _caml_stash_backtrace: PROC

; Allocation

        .CODE
        PUBLIC  _caml_alloc1
        PUBLIC  _caml_alloc2
        PUBLIC  _caml_alloc3
        PUBLIC  _caml_allocN
        PUBLIC  _caml_call_gc

_caml_call_gc:
    ; Record lowest stack address and return address
        mov     eax, [esp]
        mov     _caml_last_return_address, eax
        lea     eax, [esp+4]
        mov     _caml_bottom_of_stack, eax
    ; Save all regs used by the code generator
L105:   push    ebp
        push    edi
        push    esi
        push    edx
        push    ecx
        push    ebx
        push    eax
        mov     _caml_gc_regs, esp
    ; Call the garbage collector
        call    _caml_garbage_collection
    ; Restore all regs used by the code generator
        pop     eax
        pop     ebx
        pop     ecx
        pop     edx
        pop     esi
        pop     edi
        pop     ebp
    ; Return to caller
        ret

        ALIGN  4
_caml_alloc1:
        mov     eax, _caml_young_ptr
        sub     eax, 8
        mov     _caml_young_ptr, eax
        cmp     eax, _caml_young_limit
        jb      L100
        ret
L100:   mov     eax, [esp]
        mov     _caml_last_return_address, eax
        lea     eax, [esp+4]
        mov     _caml_bottom_of_stack, eax
        call    L105
        jmp     _caml_alloc1

        ALIGN  4
_caml_alloc2:
        mov     eax, _caml_young_ptr
        sub     eax, 12
        mov     _caml_young_ptr, eax
        cmp     eax, _caml_young_limit
        jb      L101
        ret
L101:   mov     eax, [esp]
        mov     _caml_last_return_address, eax
        lea     eax, [esp+4]
        mov     _caml_bottom_of_stack, eax
        call    L105
        jmp     _caml_alloc2

        ALIGN  4
_caml_alloc3:
        mov     eax, _caml_young_ptr
        sub     eax, 16
        mov     _caml_young_ptr, eax
        cmp     eax, _caml_young_limit
        jb      L102
        ret
L102:   mov     eax, [esp]
        mov     _caml_last_return_address, eax
        lea     eax, [esp+4]
        mov     _caml_bottom_of_stack, eax
        call    L105
        jmp     _caml_alloc3

        ALIGN  4
_caml_allocN:
        sub     eax, _caml_young_ptr         ; eax = size - young_ptr
        neg     eax                     ; eax = young_ptr - size
        cmp     eax, _caml_young_limit
        jb      L103
        mov     _caml_young_ptr, eax
        ret
L103:   sub     eax, _caml_young_ptr         ; eax = - size
        neg     eax                     ; eax = size
        push    eax                     ; save desired size
        sub     _caml_young_ptr, eax         ; must update young_ptr
        mov     eax, [esp+4]
        mov     _caml_last_return_address, eax
        lea     eax, [esp+8]
        mov     _caml_bottom_of_stack, eax
        call    L105
        pop     eax                     ; recover desired size
        jmp     _caml_allocN

; Call a C function from OCaml

        PUBLIC  _caml_c_call
        ALIGN  4
_caml_c_call:
    ; Record lowest stack address and return address
        mov     edx, [esp]
        mov     _caml_last_return_address, edx
        lea     edx, [esp+4]
        mov     _caml_bottom_of_stack, edx
    ; Call the function (address in %eax)
        jmp     eax

; Start the OCaml program

        PUBLIC  _caml_start_program
        ALIGN  4
_caml_start_program:
    ; Save callee-save registers
        push    ebx
        push    esi
        push    edi
        push    ebp
    ; Initial code pointer is caml_program
        mov     esi, offset _caml_program

; Code shared between caml_start_program and callback*

L106:
    ; Build a callback link
        push    _caml_gc_regs
        push    _caml_last_return_address
        push    _caml_bottom_of_stack
    ; Build an exception handler
        push    L108
        push    _caml_exception_pointer
        mov     _caml_exception_pointer, esp
    ; Call the OCaml code
        call    esi
L107:
    ; Pop the exception handler
        pop     _caml_exception_pointer
        pop     esi             ; dummy register
L109:
    ; Pop the callback link, restoring the global variables
    ; used by caml_c_call
        pop     _caml_bottom_of_stack
        pop     _caml_last_return_address
        pop     _caml_gc_regs
    ; Restore callee-save registers.
        pop     ebp
        pop     edi
        pop     esi
        pop     ebx
    ; Return to caller.
        ret
L108:
    ; Exception handler
    ; Mark the bucket as an exception result and return it
        or      eax, 2
        jmp     L109

; Raise an exception for OCaml

        PUBLIC  _caml_raise_exn
        ALIGN   4
_caml_raise_exn:
        test    _caml_backtrace_active, 1
        jne     L110
        mov     esp, _caml_exception_pointer
        pop     _caml_exception_pointer
        ret
L110:
        mov     _caml_backtrace_pos, 0
L111:
        mov     esi, eax                ; Save exception bucket in esi
        mov     edi, _caml_exception_pointer ; SP of handler
        mov     eax, [esp]              ; PC of raise
        lea     edx, [esp+4]
        push    edi                     ; arg 4: SP of handler
        push    edx                     ; arg 3: SP of raise
        push    eax                     ; arg 2: PC of raise
        push    esi                     ; arg 1: exception bucket
        call    _caml_stash_backtrace
        mov     eax, esi                ; recover exception bucket
        mov     esp, edi                ; cut the stack
        pop     _caml_exception_pointer
        ret

        PUBLIC  _caml_reraise_exn
        ALIGN   4
_caml_reraise_exn:
        test    _caml_backtrace_active, 1
        jne     L111
        mov     esp, _caml_exception_pointer
        pop     _caml_exception_pointer
        ret

                                ; Raise an exception from C

        PUBLIC  _caml_raise_exception
        ALIGN  4
_caml_raise_exception:
        test    _caml_backtrace_active, 1
        jne     L112
        mov     eax, [esp+4]
        mov     esp, _caml_exception_pointer
        pop     _caml_exception_pointer
        ret
L112:
        mov     esi, [esp+4]            ; Save exception bucket in esi
        push    _caml_exception_pointer ; arg 4: SP of handler
        push    _caml_bottom_of_stack   ; arg 3: SP of raise
        push    _caml_last_return_address ; arg 2: PC of raise
        push    esi                     ; arg 1: exception bucket
        call    _caml_stash_backtrace
        mov     eax, esi                ; recover exception bucket
        mov     esp, _caml_exception_pointer ; cut the stack
        pop     _caml_exception_pointer
        ret

; Callback from C to OCaml

        PUBLIC  _caml_callback_exn
        ALIGN  4
_caml_callback_exn:
    ; Save callee-save registers
        push    ebx
        push    esi
        push    edi
        push    ebp
    ; Initial loading of arguments
        mov     ebx, [esp+20]   ; closure
        mov     eax, [esp+24]   ; argument
        mov     esi, [ebx]      ; code pointer
        jmp     L106

        PUBLIC  _caml_callback2_exn
        ALIGN  4
_caml_callback2_exn:
    ; Save callee-save registers
        push    ebx
        push    esi
        push    edi
        push    ebp
    ; Initial loading of arguments
        mov     ecx, [esp+20]   ; closure
        mov     eax, [esp+24]   ; first argument
        mov     ebx, [esp+28]   ; second argument
        mov     esi, offset _caml_apply2   ; code pointer
        jmp     L106

        PUBLIC  _caml_callback3_exn
        ALIGN   4
_caml_callback3_exn:
    ; Save callee-save registers
        push    ebx
        push    esi
        push    edi
        push    ebp
    ; Initial loading of arguments
        mov     edx, [esp+20]   ; closure
        mov     eax, [esp+24]   ; first argument
        mov     ebx, [esp+28]   ; second argument
        mov     ecx, [esp+32]   ; third argument
        mov     esi, offset _caml_apply3   ; code pointer
        jmp     L106

        PUBLIC  _caml_ml_array_bound_error
        ALIGN   4
_caml_ml_array_bound_error:
    ; Empty the floating-point stack
        ffree   st(0)
        ffree   st(1)
        ffree   st(2)
        ffree   st(3)
        ffree   st(4)
        ffree   st(5)
        ffree   st(6)
        ffree   st(7)
    ; Branch to caml_array_bound_error
        mov     eax, offset _caml_array_bound_error
        jmp     _caml_c_call

        .DATA
        PUBLIC  _caml_system__frametable
_caml_system__frametable LABEL DWORD
        DWORD   1               ; one descriptor
        DWORD   L107            ; return address into callback
        WORD    -1              ; negative frame size => use callback link
        WORD    0               ; no roots here

        PUBLIC  _caml_extra_params
_caml_extra_params LABEL DWORD
        BYTE    64 DUP (?)

        END
