|***********************************************************************
|*                                                                     *
|*                                OCaml                                *
|*                                                                     *
|*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *
|*                                                                     *
|*  Copyright 1996 Institut National de Recherche en Informatique et   *
|*  en Automatique.  All rights reserved.  This file is distributed    *
|*  under the terms of the Q Public License version 1.0.               *
|*                                                                     *
|***********************************************************************

| call_gen_code is used with the following types:
|       unit -> int
|       int -> int
|       int -> double
|       int * int * address -> void
|       int * int -> void
|       unit -> unit
| Hence arg1 -> d0, arg2 -> d1, arg3 -> a0,
| and we need a special case for int -> double

        .text
        .globl  _call_gen_code
_call_gen_code:
        link    a6, #0
        movem   d2-d7/a2-a6, a7@-
        fmovem  fp2-fp7, a7@-
        movel   a6@(8), a1
        movel   a6@(12), d0
        movel   a6@(16), d1
        movel   a6@(20), a0
        jsr     a1@
        fmovem  a7@+, fp2-fp7
        movem   a7@+, d2-d7/a2-a6
        unlk    a6
        rts

        .globl  _call_gen_code_float
_call_gen_code_float:
        link    a6, #0
        moveml  d2-d7/a2-a6, a7@-
        fmovem  fp2-fp7, a7@-
        movel   a6@(8), a1
        movel   a6@(12), d0
        jsr     a1@
        fmoved  fp0, a7@-
        movel   a7@+, d0
        movel   a7@+, d1
        fmovem  a7@+, fp2-fp7
        moveml  a7@+, d2-d7/a2-a6
        unlk    a6
        rts

        .globl  _caml_c_call
_caml_c_call:
        jmp     a0@
