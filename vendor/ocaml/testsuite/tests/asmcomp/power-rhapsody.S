/*********************************************************************/
/*                                                                   */
/*                               OCaml                               */
/*                                                                   */
/*           Xavier Leroy, projet Cristal, INRIA Rocquencourt        */
/*                                                                   */
/* Copyright 1996 Institut National de Recherche en Informatique et  */
/* en Automatique.  All rights reserved.  This file is distributed   */
/* under the terms of the Q Public License version 1.0.              */
/*                                                                   */
/*********************************************************************/

/* Save and restore all callee-save registers */
/* GPR 14 at sp+16 ... GPR 31 at sp+84
   FPR 14 at sp+92 ... FPR 31 at sp+228 */

#define Save_callee_save \
        addic   r11, r1, 16-4; \
        stwu    r14, 4(r11); \
        stwu    r15, 4(r11); \
        stwu    r16, 4(r11); \
        stwu    r17, 4(r11); \
        stwu    r18, 4(r11); \
        stwu    r19, 4(r11); \
        stwu    r20, 4(r11); \
        stwu    r21, 4(r11); \
        stwu    r22, 4(r11); \
        stwu    r23, 4(r11); \
        stwu    r24, 4(r11); \
        stwu    r25, 4(r11); \
        stwu    r26, 4(r11); \
        stwu    r27, 4(r11); \
        stwu    r28, 4(r11); \
        stwu    r29, 4(r11); \
        stwu    r30, 4(r11); \
        stwu    r31, 4(r11); \
        stfdu   f14, 8(r11); \
        stfdu   f15, 8(r11); \
        stfdu   f16, 8(r11); \
        stfdu   f17, 8(r11); \
        stfdu   f18, 8(r11); \
        stfdu   f19, 8(r11); \
        stfdu   f20, 8(r11); \
        stfdu   f21, 8(r11); \
        stfdu   f22, 8(r11); \
        stfdu   f23, 8(r11); \
        stfdu   f24, 8(r11); \
        stfdu   f25, 8(r11); \
        stfdu   f26, 8(r11); \
        stfdu   f27, 8(r11); \
        stfdu   f28, 8(r11); \
        stfdu   f29, 8(r11); \
        stfdu   f30, 8(r11); \
        stfdu   f31, 8(r11)

#define Restore_callee_save \
        addic   r11, r1, 16-4; \
        lwzu    r14, 4(r11); \
        lwzu    r15, 4(r11); \
        lwzu    r16, 4(r11); \
        lwzu    r17, 4(r11); \
        lwzu    r18, 4(r11); \
        lwzu    r19, 4(r11); \
        lwzu    r20, 4(r11); \
        lwzu    r21, 4(r11); \
        lwzu    r22, 4(r11); \
        lwzu    r23, 4(r11); \
        lwzu    r24, 4(r11); \
        lwzu    r25, 4(r11); \
        lwzu    r26, 4(r11); \
        lwzu    r27, 4(r11); \
        lwzu    r28, 4(r11); \
        lwzu    r29, 4(r11); \
        lwzu    r30, 4(r11); \
        lwzu    r31, 4(r11); \
        lfdu    f14, 8(r11); \
        lfdu    f15, 8(r11); \
        lfdu    f16, 8(r11); \
        lfdu    f17, 8(r11); \
        lfdu    f18, 8(r11); \
        lfdu    f19, 8(r11); \
        lfdu    f20, 8(r11); \
        lfdu    f21, 8(r11); \
        lfdu    f22, 8(r11); \
        lfdu    f23, 8(r11); \
        lfdu    f24, 8(r11); \
        lfdu    f25, 8(r11); \
        lfdu    f26, 8(r11); \
        lfdu    f27, 8(r11); \
        lfdu    f28, 8(r11); \
        lfdu    f29, 8(r11); \
        lfdu    f30, 8(r11); \
        lfdu    f31, 8(r11)

        .text

        .globl  _call_gen_code
_call_gen_code:
    /* Allocate and link stack frame */
        stwu    r1, -256(r1)
    /* Save return address */
        mflr    r0
        stw     r0, 256+4(r1)
    /* Save all callee-save registers */
        Save_callee_save
    /* Shuffle arguments */
        mtlr    r3
        mr r3, r4
        mr r4, r5
        mr r5, r6
        mr r6, r7
    /* Call the function */
        blrl
    /* Restore callee-save registers */
        Restore_callee_save
    /* Reload return address */
        lwz     r0, 256+4(r1)
        mtlr    r0
    /* Return */
        addi    r1, r1, 256
        blr

        .globl  _caml_c_call
_caml_c_call:
    /* Jump to C function (address in 11) */
        mtctr   r11
        bctr
