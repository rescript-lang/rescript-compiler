;*********************************************************************;
;                                                                     ;
;                                OCaml                                ;
;                                                                     ;
;            Xavier Leroy, projet Cristal, INRIA Rocquencourt         ;
;                                                                     ;
;  Copyright 1996 Institut National de Recherche en Informatique et   ;
;  en Automatique.  All rights reserved.  This file is distributed    ;
;  under the terms of the Q Public License version 1.0.               ;
;                                                                     ;
;*********************************************************************;

        .386
        .MODEL FLAT

        .CODE
        PUBLIC  _call_gen_code
        ALIGN   4
_call_gen_code:
        push    ebp
        mov     ebp, esp
        push    ebx
        push    esi
        push    edi
        mov     eax, [ebp+12]
        mov     ebx, [ebp+16]
        mov     ecx, [ebp+20]
        mov     edx, [ebp+24]
        call    DWORD PTR [ebp+8]
        pop     edi
        pop     esi
        pop     ebx
        pop     ebp
        ret

        PUBLIC  _caml_c_call
        ALIGN   4
_caml_c_call:
        ffree   st(0)
        ffree   st(1)
        ffree   st(2)
        ffree   st(3)
        jmp     eax

        PUBLIC  _caml_call_gc
        PUBLIC  _caml_alloc
        PUBLIC  _caml_alloc1
        PUBLIC  _caml_alloc2
        PUBLIC  _caml_alloc3
_caml_call_gc:
_caml_alloc:
_caml_alloc1:
_caml_alloc2:
_caml_alloc3:
        int     3

        .DATA
        PUBLIC  _caml_exception_pointer
_caml_exception_pointer dword 0
        PUBLIC  _young_ptr
_young_ptr      dword 0
        PUBLIC  _young_limit
_young_limit    dword 0

        END
