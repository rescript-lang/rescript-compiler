/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

#ifdef __ppc64__
#define X(a,b) b
#else
#define X(a,b) a
#endif

#define WORD X(4,8)
#define lg X(lwz,ld)
#define lgu X(lwzu,ldu)
#define stg X(stw,std)
#define stgu X(stwu,stdu)
#define gdata X(.long,.quad)

.macro Addrglobal       /* reg, glob */
        addis   $0, 0, ha16($1)
        addi    $0, $0, lo16($1)
.endmacro
.macro Loadglobal       /* reg,glob,tmp */
        addis   $2, 0, ha16($1)
        lg      $0, lo16($1)($2)
.endmacro
.macro Storeglobal      /* reg,glob,tmp */
        addis   $2, 0, ha16($1)
        stg     $0, lo16($1)($2)
.endmacro
.macro Loadglobal32       /* reg,glob,tmp */
        addis   $2, 0, ha16($1)
        lwz     $0, lo16($1)($2)
.endmacro
.macro Storeglobal32      /* reg,glob,tmp */
        addis   $2, 0, ha16($1)
        stw     $0, lo16($1)($2)
.endmacro

        .text

        .globl  _caml_system__code_begin
_caml_system__code_begin:

/* Invoke the garbage collector. */

        .globl  _caml_call_gc
_caml_call_gc:
    /* Set up stack frame */
#define FRAMESIZE (32*WORD + 32*8 + 32)
        stwu    r1, -FRAMESIZE(r1)
    /* Record return address into OCaml code */
        mflr    r0
        Storeglobal r0, _caml_last_return_address, r11
    /* Record lowest stack address */
        addi    r0, r1, FRAMESIZE
        Storeglobal r0, _caml_bottom_of_stack, r11
    /* Touch the stack to trigger a recoverable segfault
       if insufficient space remains */
        addi    r1, r1, -4096*WORD
        stg     r0, 0(r1)
        addi    r1, r1, 4096*WORD
    /* Record pointer to register array */
        addi    r0, r1, 8*32 + 32
        Storeglobal r0, _caml_gc_regs, r11
    /* Save current allocation pointer for debugging purposes */
        Storeglobal r31, _caml_young_ptr, r11
    /* Save exception pointer (if e.g. a sighandler raises) */
        Storeglobal r29, _caml_exception_pointer, r11
    /* Save all registers used by the code generator */
        addi    r11, r1, 8*32 + 32 - WORD
        stgu    r3, WORD(r11)
        stgu    r4, WORD(r11)
        stgu    r5, WORD(r11)
        stgu    r6, WORD(r11)
        stgu    r7, WORD(r11)
        stgu    r8, WORD(r11)
        stgu    r9, WORD(r11)
        stgu    r10, WORD(r11)
        stgu    r14, WORD(r11)
        stgu    r15, WORD(r11)
        stgu    r16, WORD(r11)
        stgu    r17, WORD(r11)
        stgu    r18, WORD(r11)
        stgu    r19, WORD(r11)
        stgu    r20, WORD(r11)
        stgu    r21, WORD(r11)
        stgu    r22, WORD(r11)
        stgu    r23, WORD(r11)
        stgu    r24, WORD(r11)
        stgu    r25, WORD(r11)
        stgu    r26, WORD(r11)
        stgu    r27, WORD(r11)
        stgu    r28, WORD(r11)
        addi    r11, r1, 32 - 8
        stfdu   f1, 8(r11)
        stfdu   f2, 8(r11)
        stfdu   f3, 8(r11)
        stfdu   f4, 8(r11)
        stfdu   f5, 8(r11)
        stfdu   f6, 8(r11)
        stfdu   f7, 8(r11)
        stfdu   f8, 8(r11)
        stfdu   f9, 8(r11)
        stfdu   f10, 8(r11)
        stfdu   f11, 8(r11)
        stfdu   f12, 8(r11)
        stfdu   f13, 8(r11)
        stfdu   f14, 8(r11)
        stfdu   f15, 8(r11)
        stfdu   f16, 8(r11)
        stfdu   f17, 8(r11)
        stfdu   f18, 8(r11)
        stfdu   f19, 8(r11)
        stfdu   f20, 8(r11)
        stfdu   f21, 8(r11)
        stfdu   f22, 8(r11)
        stfdu   f23, 8(r11)
        stfdu   f24, 8(r11)
        stfdu   f25, 8(r11)
        stfdu   f26, 8(r11)
        stfdu   f27, 8(r11)
        stfdu   f28, 8(r11)
        stfdu   f29, 8(r11)
        stfdu   f30, 8(r11)
        stfdu   f31, 8(r11)
    /* Call the GC */
        bl      _caml_garbage_collection
    /* Reload new allocation pointer and allocation limit */
        Loadglobal r31, _caml_young_ptr, r11
        Loadglobal r30, _caml_young_limit, r11
    /* Restore all regs used by the code generator */
        addi    r11, r1, 8*32 + 32 - WORD
        lgu     r3, WORD(r11)
        lgu     r4, WORD(r11)
        lgu     r5, WORD(r11)
        lgu     r6, WORD(r11)
        lgu     r7, WORD(r11)
        lgu     r8, WORD(r11)
        lgu     r9, WORD(r11)
        lgu     r10, WORD(r11)
        lgu     r14, WORD(r11)
        lgu     r15, WORD(r11)
        lgu     r16, WORD(r11)
        lgu     r17, WORD(r11)
        lgu     r18, WORD(r11)
        lgu     r19, WORD(r11)
        lgu     r20, WORD(r11)
        lgu     r21, WORD(r11)
        lgu     r22, WORD(r11)
        lgu     r23, WORD(r11)
        lgu     r24, WORD(r11)
        lgu     r25, WORD(r11)
        lgu     r26, WORD(r11)
        lgu     r27, WORD(r11)
        lgu     r28, WORD(r11)
        addi    r11, r1, 32 - 8
        lfdu    f1, 8(r11)
        lfdu    f2, 8(r11)
        lfdu    f3, 8(r11)
        lfdu    f4, 8(r11)
        lfdu    f5, 8(r11)
        lfdu    f6, 8(r11)
        lfdu    f7, 8(r11)
        lfdu    f8, 8(r11)
        lfdu    f9, 8(r11)
        lfdu    f10, 8(r11)
        lfdu    f11, 8(r11)
        lfdu    f12, 8(r11)
        lfdu    f13, 8(r11)
        lfdu    f14, 8(r11)
        lfdu    f15, 8(r11)
        lfdu    f16, 8(r11)
        lfdu    f17, 8(r11)
        lfdu    f18, 8(r11)
        lfdu    f19, 8(r11)
        lfdu    f20, 8(r11)
        lfdu    f21, 8(r11)
        lfdu    f22, 8(r11)
        lfdu    f23, 8(r11)
        lfdu    f24, 8(r11)
        lfdu    f25, 8(r11)
        lfdu    f26, 8(r11)
        lfdu    f27, 8(r11)
        lfdu    f28, 8(r11)
        lfdu    f29, 8(r11)
        lfdu    f30, 8(r11)
        lfdu    f31, 8(r11)
    /* Return to caller, restarting the allocation */
        Loadglobal r0, _caml_last_return_address, r11
        addic   r0, r0, -16     /* Restart the allocation (4 instructions) */
        mtlr    r0
    /* Say we are back into OCaml code */
        li      r12, 0
        Storeglobal r12, _caml_last_return_address, r11
    /* Deallocate stack frame */
        addi    r1, r1, FRAMESIZE
    /* Return */
        blr
#undef FRAMESIZE

/* Call a C function from OCaml */

        .globl  _caml_c_call
_caml_c_call:
    /* Save return address */
        mflr    r25
    /* Get ready to call C function (address in 11) */
        mtctr   r11
    /* Record lowest stack address and return address */
        Storeglobal r1, _caml_bottom_of_stack, r12
        Storeglobal r25, _caml_last_return_address, r12
    /* Touch the stack to trigger a recoverable segfault
       if insufficient space remains */
        addi    r1, r1, -4096*WORD
        stg     r0, 0(r1)
        addi    r1, r1, 4096*WORD
    /* Make the exception handler and alloc ptr available to the C code */
        Storeglobal r31, _caml_young_ptr, r11
        Storeglobal r29, _caml_exception_pointer, r11
    /* Call the function (address in link register) */
        bctrl
    /* Restore return address (in 25, preserved by the C function) */
        mtlr    r25
    /* Reload allocation pointer and allocation limit*/
        Loadglobal r31, _caml_young_ptr, r11
        Loadglobal r30, _caml_young_limit, r11
    /* Say we are back into OCaml code */
        li      r12, 0
        Storeglobal r12, _caml_last_return_address, r11
    /* Return to caller */
        blr

/* Raise an exception from OCaml */
        .globl  _caml_raise_exn
_caml_raise_exn:
        Loadglobal32 r11, _caml_backtrace_active, r11
        cmpwi   r11, 0
        bne     L110
L111:
    /* Pop trap frame */
        lg      r0, 0(r29)
        mr      r1, r29
        mtctr    r0
        lg      r29, WORD(r1)
        addi    r1, r1, 16
    /* Branch to handler */
        bctr
L110:
        li      r0, 0
        Storeglobal32 r0, _caml_backtrace_pos, r11
L114:
        mr      r28, r3        /* preserve exn bucket in callee-save */
                               /* arg 1: exception bucket (already in r3) */
        mflr    r4             /* arg 2: PC of raise */
        mr      r5, r1         /* arg 3: SP of raise */
        mr      r6, r29        /* arg 4: SP of handler */
        addi    r1, r1, -(16*WORD)    /* reserve stack space for C call */
        bl      _caml_stash_backtrace
        mr      r3, r28
        b       L111

        .globl  _caml_reraise_exn
_caml_reraise_exn:
        Loadglobal32 r11, _caml_backtrace_active, r11
        cmpwi   r11, 0
        bne-    L114
    /* Pop trap frame */
        lg      r0, 0(r29)
        mr      r1, r29
        mtctr    r0
        lg      r29, WORD(r1)
        addi    r1, r1, 16
    /* Branch to handler */
        bctr

        /* Raise an exception from C */

        .globl  _caml_raise_exception
_caml_raise_exception:
        Loadglobal32 r11, _caml_backtrace_active, r11
        cmpwi   r11, 0
        bne     L112
L113:
    /* Reload OCaml global registers */
        Loadglobal r1, _caml_exception_pointer, r11
        Loadglobal r31, _caml_young_ptr, r11
        Loadglobal r30, _caml_young_limit, r11
    /* Say we are back into OCaml code */
        li      r0, 0
        Storeglobal r0, _caml_last_return_address, r11
    /* Pop trap frame */
        lg      r0, 0(r1)
        lg      r29, WORD(r1)
        mtctr   r0
        addi    r1, r1, 16
    /* Branch to handler */
        bctr
L112:
        mr      r28, r3        /* preserve exn bucket in callee-save */
                               /* arg 1: exception bucket (already in r3) */
        Loadglobal r4, _caml_last_return_address, r11 /* arg 2: PC of raise */
        Loadglobal r5, _caml_bottom_of_stack, r11 /* arg 3: SP of raise */
        Loadglobal r6, _caml_exception_pointer, r11 /* arg 4: SP of handler */
        addi    r1, r1, -(16*WORD)    /* reserve stack space for C call */
        bl      _caml_stash_backtrace
        mr      r3, r28
        b       L113

/* Start the OCaml program */

        .globl  _caml_start_program
_caml_start_program:
        Addrglobal r12, _caml_program

/* Code shared between caml_start_program and caml_callback */
L102:
    /* Allocate and link stack frame */
#define FRAMESIZE (16 + 20*WORD + 18*8)
        stgu    r1, -FRAMESIZE(r1)
    /* Save return address */
        mflr    r0
        stg     r0,  WORD(r1)
    /* Save all callee-save registers */
    /* GPR14 ... GPR31, then FPR14 ... FPR31 starting at sp+16 */
        addi    r11, r1, 16-WORD
        stgu    r14, WORD(r11)
        stgu    r15, WORD(r11)
        stgu    r16, WORD(r11)
        stgu    r17, WORD(r11)
        stgu    r18, WORD(r11)
        stgu    r19, WORD(r11)
        stgu    r20, WORD(r11)
        stgu    r21, WORD(r11)
        stgu    r22, WORD(r11)
        stgu    r23, WORD(r11)
        stgu    r24, WORD(r11)
        stgu    r25, WORD(r11)
        stgu    r26, WORD(r11)
        stgu    r27, WORD(r11)
        stgu    r28, WORD(r11)
        stgu    r29, WORD(r11)
        stgu    r30, WORD(r11)
        stgu    r31, WORD(r11)
        stfdu   f14, 8(r11)
        stfdu   f15, 8(r11)
        stfdu   f16, 8(r11)
        stfdu   f17, 8(r11)
        stfdu   f18, 8(r11)
        stfdu   f19, 8(r11)
        stfdu   f20, 8(r11)
        stfdu   f21, 8(r11)
        stfdu   f22, 8(r11)
        stfdu   f23, 8(r11)
        stfdu   f24, 8(r11)
        stfdu   f25, 8(r11)
        stfdu   f26, 8(r11)
        stfdu   f27, 8(r11)
        stfdu   f28, 8(r11)
        stfdu   f29, 8(r11)
        stfdu   f30, 8(r11)
        stfdu   f31, 8(r11)
    /* Set up a callback link */
        addi    r1, r1, -32
        Loadglobal r9, _caml_bottom_of_stack, r11
        Loadglobal r10, _caml_last_return_address, r11
        Loadglobal r11, _caml_gc_regs, r11
        stg     r9, 0(r1)
        stg     r10, WORD(r1)
        stg     r11, 2*WORD(r1)
    /* Build an exception handler to catch exceptions escaping out of OCaml */
        bl      L103
        b       L104
L103:
        addi    r1, r1, -16
        mflr    r0
        stg     r0, 0(r1)
        Loadglobal r11, _caml_exception_pointer, r11
        stg     r11, WORD(r1)
        mr      r29, r1
    /* Reload allocation pointers */
        Loadglobal r31, _caml_young_ptr, r11
        Loadglobal r30, _caml_young_limit, r11
    /* Say we are back into OCaml code */
        li      r0, 0
        Storeglobal r0, _caml_last_return_address, r11
    /* Call the OCaml code */
        mtctr    r12
L105:
        bctrl
    /* Pop the trap frame, restoring caml_exception_pointer */
        lg      r9, WORD(r1)
        Storeglobal r9, _caml_exception_pointer, r11
        addi    r1, r1, 16
    /* Pop the callback link, restoring the global variables */
L106:
        lg      r9, 0(r1)
        lg      r10, WORD(r1)
        lg      r11, 2*WORD(r1)
        Storeglobal r9, _caml_bottom_of_stack, r12
        Storeglobal r10, _caml_last_return_address, r12
        Storeglobal r11, _caml_gc_regs, r12
        addi    r1, r1, 32
    /* Update allocation pointer */
        Storeglobal r31, _caml_young_ptr, r11
    /* Restore callee-save registers */
        addi    r11, r1, 16-WORD
        lgu     r14, WORD(r11)
        lgu     r15, WORD(r11)
        lgu     r16, WORD(r11)
        lgu     r17, WORD(r11)
        lgu     r18, WORD(r11)
        lgu     r19, WORD(r11)
        lgu     r20, WORD(r11)
        lgu     r21, WORD(r11)
        lgu     r22, WORD(r11)
        lgu     r23, WORD(r11)
        lgu     r24, WORD(r11)
        lgu     r25, WORD(r11)
        lgu     r26, WORD(r11)
        lgu     r27, WORD(r11)
        lgu     r28, WORD(r11)
        lgu     r29, WORD(r11)
        lgu     r30, WORD(r11)
        lgu     r31, WORD(r11)
        lfdu    f14, 8(r11)
        lfdu    f15, 8(r11)
        lfdu    f16, 8(r11)
        lfdu    f17, 8(r11)
        lfdu    f18, 8(r11)
        lfdu    f19, 8(r11)
        lfdu    f20, 8(r11)
        lfdu    f21, 8(r11)
        lfdu    f22, 8(r11)
        lfdu    f23, 8(r11)
        lfdu    f24, 8(r11)
        lfdu    f25, 8(r11)
        lfdu    f26, 8(r11)
        lfdu    f27, 8(r11)
        lfdu    f28, 8(r11)
        lfdu    f29, 8(r11)
        lfdu    f30, 8(r11)
        lfdu    f31, 8(r11)
    /* Reload return address */
        lg      r0, WORD(r1)
        mtlr    r0
    /* Return */
        addi    r1, r1, FRAMESIZE
        blr

    /* The trap handler: */
L104:
    /* Update caml_exception_pointer */
        Storeglobal r29, _caml_exception_pointer, r11
    /* Encode exception bucket as an exception result and return it */
        ori     r3, r3, 2
        b       L106
#undef FRAMESIZE

/* Callback from C to OCaml */

        .globl  _caml_callback_exn
_caml_callback_exn:
    /* Initial shuffling of arguments */
        mr      r0, r3            /* Closure */
        mr      r3, r4            /* Argument */
        mr      r4, r0
        lg      r12, 0(r4)        /* Code pointer */
        b       L102

        .globl  _caml_callback2_exn
_caml_callback2_exn:
        mr      r0, r3            /* Closure */
        mr      r3, r4            /* First argument */
        mr      r4, r5            /* Second argument */
        mr      r5, r0
        Addrglobal r12, _caml_apply2
        b       L102

        .globl  _caml_callback3_exn
_caml_callback3_exn:
        mr      r0, r3            /* Closure */
        mr      r3, r4            /* First argument */
        mr      r4, r5            /* Second argument */
        mr      r5, r6            /* Third argument */
        mr      r6, r0
        Addrglobal r12, _caml_apply3
        b       L102

        .globl  _caml_system__code_end
_caml_system__code_end:

/* Frame table */

        .const
        .globl  _caml_system__frametable
_caml_system__frametable:
        gdata   1               /* one descriptor */
        gdata   L105 + 4       /* return address into callback */
        .short  -1              /* negative size count => use callback link */
        .short  0               /* no roots here */
        .align  X(2,3)
