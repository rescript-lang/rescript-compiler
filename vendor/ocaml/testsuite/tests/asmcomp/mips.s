/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

        .globl  call_gen_code
        .ent    call_gen_code
call_gen_code:
        subu    $sp, $sp, 0x90
        sd      $31, 0x88($sp)
    /* Save all callee-save registers */
        sd      $16, 0x0($sp)
        sd      $17, 0x8($sp)
        sd      $18, 0x10($sp)
        sd      $19, 0x18($sp)
        sd      $20, 0x20($sp)
        sd      $21, 0x28($sp)
        sd      $22, 0x30($sp)
        sd      $23, 0x38($sp)
        sd      $30, 0x40($sp)
        s.d     $f20, 0x48($sp)
        s.d     $f22, 0x50($sp)
        s.d     $f24, 0x58($sp)
        s.d     $f26, 0x60($sp)
        s.d     $f28, 0x68($sp)
        s.d     $f30, 0x70($sp)
    /* Shuffle arguments */
        move    $8, $5
        move    $9, $6
        move    $10, $7
        move    $25, $4
        jal     $4
    /* Restore registers */
        ld      $31, 0x88($sp)
        ld      $16, 0x0($sp)
        ld      $17, 0x8($sp)
        ld      $18, 0x10($sp)
        ld      $19, 0x18($sp)
        ld      $20, 0x20($sp)
        ld      $21, 0x28($sp)
        ld      $22, 0x30($sp)
        ld      $23, 0x38($sp)
        ld      $30, 0x40($sp)
        l.d     $f20, 0x48($sp)
        l.d     $f22, 0x50($sp)
        l.d     $f24, 0x58($sp)
        l.d     $f26, 0x60($sp)
        l.d     $f28, 0x68($sp)
        l.d     $f30, 0x70($sp)
        addu    $sp, $sp, 0x90
        j       $31

        .end    call_gen_code

/* Call a C function */

        .globl  caml_c_call
        .ent    caml_c_call
caml_c_call:
        move    $25, $24
        j       $24
        .end    caml_c_call
