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
        addic   11, 1, 16-4; \
        stwu    14, 4(11); \
        stwu    15, 4(11); \
        stwu    16, 4(11); \
        stwu    17, 4(11); \
        stwu    18, 4(11); \
        stwu    19, 4(11); \
        stwu    20, 4(11); \
        stwu    21, 4(11); \
        stwu    22, 4(11); \
        stwu    23, 4(11); \
        stwu    24, 4(11); \
        stwu    25, 4(11); \
        stwu    26, 4(11); \
        stwu    27, 4(11); \
        stwu    28, 4(11); \
        stwu    29, 4(11); \
        stwu    30, 4(11); \
        stwu    31, 4(11); \
        stfdu   14, 8(11); \
        stfdu   15, 8(11); \
        stfdu   16, 8(11); \
        stfdu   17, 8(11); \
        stfdu   18, 8(11); \
        stfdu   19, 8(11); \
        stfdu   20, 8(11); \
        stfdu   21, 8(11); \
        stfdu   22, 8(11); \
        stfdu   23, 8(11); \
        stfdu   24, 8(11); \
        stfdu   25, 8(11); \
        stfdu   26, 8(11); \
        stfdu   27, 8(11); \
        stfdu   28, 8(11); \
        stfdu   29, 8(11); \
        stfdu   30, 8(11); \
        stfdu   31, 8(11)

#define Restore_callee_save \
        addic   11, 1, 16-4; \
        lwzu    14, 4(11); \
        lwzu    15, 4(11); \
        lwzu    16, 4(11); \
        lwzu    17, 4(11); \
        lwzu    18, 4(11); \
        lwzu    19, 4(11); \
        lwzu    20, 4(11); \
        lwzu    21, 4(11); \
        lwzu    22, 4(11); \
        lwzu    23, 4(11); \
        lwzu    24, 4(11); \
        lwzu    25, 4(11); \
        lwzu    26, 4(11); \
        lwzu    27, 4(11); \
        lwzu    28, 4(11); \
        lwzu    29, 4(11); \
        lwzu    30, 4(11); \
        lwzu    31, 4(11); \
        lfdu    14, 8(11); \
        lfdu    15, 8(11); \
        lfdu    16, 8(11); \
        lfdu    17, 8(11); \
        lfdu    18, 8(11); \
        lfdu    19, 8(11); \
        lfdu    20, 8(11); \
        lfdu    21, 8(11); \
        lfdu    22, 8(11); \
        lfdu    23, 8(11); \
        lfdu    24, 8(11); \
        lfdu    25, 8(11); \
        lfdu    26, 8(11); \
        lfdu    27, 8(11); \
        lfdu    28, 8(11); \
        lfdu    29, 8(11); \
        lfdu    30, 8(11); \
        lfdu    31, 8(11)

        .section ".text"

        .globl  call_gen_code
        .type   call_gen_code, @function
call_gen_code:
    /* Allocate and link stack frame */
        stwu    1, -256(1)
    /* Save return address */
        mflr    0
        stw     0, 256+4(1)
    /* Save all callee-save registers */
        Save_callee_save
    /* Shuffle arguments */
        mtlr    3
        mr 3, 4
        mr 4, 5
        mr 5, 6
        mr 6, 7
    /* Call the function */
        blrl
    /* Restore callee-save registers */
        Restore_callee_save
    /* Reload return address */
        lwz     0, 256+4(1)
        mtlr    0
    /* Return */
        addi    1, 1, 256
        blr

        .globl  caml_c_call
        .type   caml_c_call, @function
caml_c_call:
    /* Jump to C function (address in 11) */
        mtctr   11
        bctr
