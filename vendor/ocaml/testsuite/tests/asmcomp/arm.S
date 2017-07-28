/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1998 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

        .text

        .global call_gen_code
        .type   call_gen_code, %function
        .align  0
call_gen_code:
        mov     ip, sp
        stmfd   sp!, {r4, r5, r6, r7, r8, r9, fp, ip, lr, pc}
        sub     fp, ip, #4
    @ r0 is function to call
    @ r1, r2, r3 are arguments 1, 2, 3
        mov     r4, r0
        mov     r0, r1
        mov     r1, r2
        mov     r2, r3
        mov     lr, pc
        mov     pc, r4
        ldmea   fp, {r4, r5, r6, r7, r8, r9, fp, sp, pc}

        .global caml_c_call
        .type   caml_c_call, %function
        .align  0
caml_c_call:
    @ function to call is in r10
        mov     pc, r10
