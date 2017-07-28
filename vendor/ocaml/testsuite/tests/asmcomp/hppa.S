;*********************************************************************
;*                                                                   *
;*                               OCaml                               *
;*                                                                   *
;*           Xavier Leroy, projet Cristal, INRIA Rocquencourt        *
;*                                                                   *
;* Copyright 1996 Institut National de Recherche en Informatique et  *
;* en Automatique.  All rights reserved.  This file is distributed   *
;* under the terms of the Q Public License version 1.0.              *
;*                                                                   *
;*********************************************************************

; Must be preprocessed by cpp

#ifdef SYS_hpux
#define G(x) x
#define CODESPACE .code
#define CODE_ALIGN 4
#define EXPORT_CODE(x) .export x, entry, priv_lev=3
#define STARTPROC .proc ! .callinfo frame=0, no_calls ! .entry
#define ENDPROC .exit ! .procend
#endif

#ifdef SYS_nextstep
#define G(x) _##x
#define CODESPACE .text
#define CODE_ALIGN 2
#define EXPORT_CODE(x) .globl x
#define STARTPROC
#define ENDPROC
#endif

#ifdef SYS_hpux
        .space $PRIVATE$
        .subspa $DATA$,quad=1,align=8,access=31
        .subspa $BSS$,quad=1,align=8,access=31,zero,sort=82
        .space $TEXT$
        .subspa $LIT$,quad=0,align=8,access=44
        .subspa $CODE$,quad=0,align=8,access=44,code_only
        .import $global$, data
        .import $$dyncall, millicode
#endif

        CODESPACE
        .align  CODE_ALIGN
        EXPORT_CODE(G(call_gen_code))
G(call_gen_code):
        STARTPROC
        stw     %r2,-20(%r30)
        ldo     256(%r30), %r30
; Save the callee-save registers
        ldo     -32(%r30), %r1
        stws,ma %r3, -4(%r1)
        stws,ma %r4, -4(%r1)
        stws,ma %r5, -4(%r1)
        stws,ma %r6, -4(%r1)
        stws,ma %r7, -4(%r1)
        stws,ma %r8, -4(%r1)
        stws,ma %r9, -4(%r1)
        stws,ma %r10, -4(%r1)
        stws,ma %r11, -4(%r1)
        stws,ma %r12, -4(%r1)
        stws,ma %r13, -4(%r1)
        stws,ma %r14, -4(%r1)
        stws,ma %r15, -4(%r1)
        stws,ma %r16, -4(%r1)
        stws,ma %r17, -4(%r1)
        stws,ma %r18, -4(%r1)
        fstds,ma %fr12, -8(%r1)
        fstds,ma %fr13, -8(%r1)
        fstds,ma %fr14, -8(%r1)
        fstds,ma %fr15, -8(%r1)
        fstds,ma %fr16, -8(%r1)
        fstds,ma %fr17, -8(%r1)
        fstds,ma %fr18, -8(%r1)
        fstds,ma %fr19, -8(%r1)
        fstds,ma %fr20, -8(%r1)
        fstds,ma %fr21, -8(%r1)
        fstds,ma %fr22, -8(%r1)
        fstds,ma %fr23, -8(%r1)
        fstds,ma %fr24, -8(%r1)
        fstds,ma %fr25, -8(%r1)
        fstds,ma %fr26, -8(%r1)
        fstds,ma %fr27, -8(%r1)
        fstds,ma %fr28, -8(%r1)
        fstds,ma %fr29, -8(%r1)
        fstds,ma %fr30, -8(%r1)
        fstds,ma %fr31, -8(%r1)

; Shuffle the arguments and call
        copy    %r26, %r22
        copy    %r25, %r26
        copy    %r24, %r25
        copy    %r23, %r24
        fcpy,dbl %fr5, %fr4
#ifdef SYS_hpux
        bl      $$dyncall, %r2
        nop
#else
        ble     0(4, %r22)
        copy    %r31, %r2
#endif
; Shuffle the results
        copy    %r26, %r28
; Restore the callee-save registers
        ldo     -32(%r30), %r1
        ldws,ma -4(%r1), %r3
        ldws,ma -4(%r1), %r4
        ldws,ma -4(%r1), %r5
        ldws,ma -4(%r1), %r6
        ldws,ma -4(%r1), %r7
        ldws,ma -4(%r1), %r8
        ldws,ma -4(%r1), %r9
        ldws,ma -4(%r1), %r10
        ldws,ma -4(%r1), %r11
        ldws,ma -4(%r1), %r12
        ldws,ma -4(%r1), %r13
        ldws,ma -4(%r1), %r14
        ldws,ma -4(%r1), %r15
        ldws,ma -4(%r1), %r16
        ldws,ma -4(%r1), %r17
        ldws,ma -4(%r1), %r18
        fldds,ma -8(%r1), %fr12
        fldds,ma -8(%r1), %fr13
        fldds,ma -8(%r1), %fr14
        fldds,ma -8(%r1), %fr15
        fldds,ma -8(%r1), %fr16
        fldds,ma -8(%r1), %fr17
        fldds,ma -8(%r1), %fr18
        fldds,ma -8(%r1), %fr19
        fldds,ma -8(%r1), %fr20
        fldds,ma -8(%r1), %fr21
        fldds,ma -8(%r1), %fr22
        fldds,ma -8(%r1), %fr23
        fldds,ma -8(%r1), %fr24
        fldds,ma -8(%r1), %fr25
        fldds,ma -8(%r1), %fr26
        fldds,ma -8(%r1), %fr27
        fldds,ma -8(%r1), %fr28
        fldds,ma -8(%r1), %fr29
        fldds,ma -8(%r1), %fr30
        fldds,ma -8(%r1), %fr31

        ldo     -256(%r30), %r30
        ldw     -20(%r30), %r2
        bv      0(%r2)
        nop
        ENDPROC

        .align  CODE_ALIGN
        EXPORT_CODE(caml_c_call)
G(caml_c_call):
        STARTPROC
#ifdef SYS_hpux
        bl $$dyncall, %r0
        nop
#else
        bv      0(%r22)
        nop
#endif
        ENDPROC
