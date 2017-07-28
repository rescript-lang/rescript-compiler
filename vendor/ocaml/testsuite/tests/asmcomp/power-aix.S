#*********************************************************************
#*                                                                   *
#*                               OCaml                               *
#*                                                                   *
#*           Xavier Leroy, projet Cristal, INRIA Rocquencourt        *
#*                                                                   *
#* Copyright 1996 Institut National de Recherche en Informatique et  *
#* en Automatique.  All rights reserved.  This file is distributed   *
#* under the terms of the Q Public License version 1.0.              *
#*                                                                   *
#*********************************************************************

        .csect  .text[PR]

        .globl  .call_gen_code
.call_gen_code:
# Save return address
        mflr 0
        stw 0, 8(1)
# Save all callee-save registers
        stw 13,-76(1)
        stw 14,-72(1)
        stw 15,-68(1)
        stw 16,-64(1)
        stw 17,-60(1)
        stw 18,-56(1)
        stw 19,-52(1)
        stw 20,-48(1)
        stw 21,-44(1)
        stw 22,-40(1)
        stw 23,-36(1)
        stw 24,-32(1)
        stw 25,-28(1)
        stw 26,-24(1)
        stw 27,-20(1)
        stw 28,-16(1)
        stw 29,-12(1)
        stw 30,-8(1)
        stw 31,-4(1)
        stfd 14, -224(1)
        stfd 15, -216(1)
        stfd 16, -208(1)
        stfd 17, -200(1)
        stfd 18, -192(1)
        stfd 19, -184(1)
        stfd 20, -176(1)
        stfd 21, -168(1)
        stfd 22, -160(1)
        stfd 23, -152(1)
        stfd 24, -144(1)
        stfd 25, -136(1)
        stfd 26, -128(1)
        stfd 27, -120(1)
        stfd 28, -112(1)
        stfd 29, -104(1)
        stfd 30, -96(1)
        stfd 31, -88(1)
# Allocate and link stack frame
        stwu 1, -280(1)
# Save global pointer
        stw 2, 20(1)
# Load code to call
        lwz 0, 0(3)
        lwz 2, 4(3)
        mtlr 0
# Shuffle arguments
        mr 3, 4
        mr 4, 5
        mr 5, 6
        mr 6, 7
# Call the function
        blrl
# Restore global pointer
        lwz 2, 20(1)
# Deallocate stack frame
        addic 1, 1, 280
# Restore callee-save registers
        lwz 13,-76(1)
        lwz 14,-72(1)
        lwz 15,-68(1)
        lwz 16,-64(1)
        lwz 17,-60(1)
        lwz 18,-56(1)
        lwz 19,-52(1)
        lwz 20,-48(1)
        lwz 21,-44(1)
        lwz 22,-40(1)
        lwz 23,-36(1)
        lwz 24,-32(1)
        lwz 25,-28(1)
        lwz 26,-24(1)
        lwz 27,-20(1)
        lwz 28,-16(1)
        lwz 29,-12(1)
        lwz 30,-8(1)
        lwz 31,-4(1)
        lfd 14, -224(1)
        lfd 15, -216(1)
        lfd 16, -208(1)
        lfd 17, -200(1)
        lfd 18, -192(1)
        lfd 19, -184(1)
        lfd 20, -176(1)
        lfd 21, -168(1)
        lfd 22, -160(1)
        lfd 23, -152(1)
        lfd 24, -144(1)
        lfd 25, -136(1)
        lfd 26, -128(1)
        lfd 27, -120(1)
        lfd 28, -112(1)
        lfd 29, -104(1)
        lfd 30, -96(1)
        lfd 31, -88(1)
# Reload return address
        lwz 0, 8(1)
        mtlr 0
# Return
        blr

        .globl  .caml_c_call
.caml_c_call:
# Preserve RTOC and return address in callee-save registers
# The C function will preserve them, and the OCaml code does not
# expect them to be preserved
# Return address is in 25, RTOC is in 26
        mflr    25
        mr      26, 2
# Call desired function (descriptor in r11)
        lwz     0, 0(11)
        lwz     2, 4(11)
        mtlr    0
        blrl
# Restore return address and RTOC
        mtlr    25
        mr      2, 26
# Return to caller
        blr

# Function closures

        .globl  call_gen_code
        .csect  call_gen_code[DS]
call_gen_code:
        .long   .call_gen_code, TOC[tc0], 0

        .globl  caml_c_call
        .csect  caml_c_call[DS]
caml_c_call:
        .long   .caml_c_call, TOC[tc0], 0
