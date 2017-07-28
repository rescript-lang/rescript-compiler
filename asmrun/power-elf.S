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

#define Addrglobal(reg,glob) \
        addis   reg, 0, glob@ha; \
        addi    reg, reg, glob@l
#define Loadglobal(reg,glob,tmp) \
        addis   tmp, 0, glob@ha; \
        lwz     reg, glob@l(tmp)
#define Storeglobal(reg,glob,tmp) \
        addis   tmp, 0, glob@ha; \
        stw     reg, glob@l(tmp)

        .section ".text"

/* Invoke the garbage collector. */

        .globl  caml_system__code_begin
caml_system__code_begin:

        .globl  caml_call_gc
        .type   caml_call_gc, @function
caml_call_gc:
    /* Set up stack frame */
        stwu    1, -0x1A0(1)
    /* 0x1A0 = 4*32 (int regs) + 8*32 (float regs) + 32 (space for C call) */
    /* Record return address into OCaml code */
        mflr    0
        Storeglobal(0, caml_last_return_address, 11)
    /* Record lowest stack address */
        addi    0, 1, 0x1A0
        Storeglobal(0, caml_bottom_of_stack, 11)
    /* Record pointer to register array */
        addi    0, 1, 8*32 + 32
        Storeglobal(0, caml_gc_regs, 11)
    /* Save current allocation pointer for debugging purposes */
        Storeglobal(31, caml_young_ptr, 11)
    /* Save exception pointer (if e.g. a sighandler raises) */
        Storeglobal(29, caml_exception_pointer, 11)
    /* Save all registers used by the code generator */
        addi    11, 1, 8*32 + 32 - 4
        stwu    3, 4(11)
        stwu    4, 4(11)
        stwu    5, 4(11)
        stwu    6, 4(11)
        stwu    7, 4(11)
        stwu    8, 4(11)
        stwu    9, 4(11)
        stwu    10, 4(11)
        stwu    14, 4(11)
        stwu    15, 4(11)
        stwu    16, 4(11)
        stwu    17, 4(11)
        stwu    18, 4(11)
        stwu    19, 4(11)
        stwu    20, 4(11)
        stwu    21, 4(11)
        stwu    22, 4(11)
        stwu    23, 4(11)
        stwu    24, 4(11)
        stwu    25, 4(11)
        stwu    26, 4(11)
        stwu    27, 4(11)
        stwu    28, 4(11)
        addi    11, 1, 32 - 8
        stfdu   1, 8(11)
        stfdu   2, 8(11)
        stfdu   3, 8(11)
        stfdu   4, 8(11)
        stfdu   5, 8(11)
        stfdu   6, 8(11)
        stfdu   7, 8(11)
        stfdu   8, 8(11)
        stfdu   9, 8(11)
        stfdu   10, 8(11)
        stfdu   11, 8(11)
        stfdu   12, 8(11)
        stfdu   13, 8(11)
        stfdu   14, 8(11)
        stfdu   15, 8(11)
        stfdu   16, 8(11)
        stfdu   17, 8(11)
        stfdu   18, 8(11)
        stfdu   19, 8(11)
        stfdu   20, 8(11)
        stfdu   21, 8(11)
        stfdu   22, 8(11)
        stfdu   23, 8(11)
        stfdu   24, 8(11)
        stfdu   25, 8(11)
        stfdu   26, 8(11)
        stfdu   27, 8(11)
        stfdu   28, 8(11)
        stfdu   29, 8(11)
        stfdu   30, 8(11)
        stfdu   31, 8(11)
    /* Call the GC */
        bl      caml_garbage_collection
    /* Reload new allocation pointer and allocation limit */
        Loadglobal(31, caml_young_ptr, 11)
        Loadglobal(30, caml_young_limit, 11)
    /* Restore all regs used by the code generator */
        addi    11, 1, 8*32 + 32 - 4
        lwzu    3, 4(11)
        lwzu    4, 4(11)
        lwzu    5, 4(11)
        lwzu    6, 4(11)
        lwzu    7, 4(11)
        lwzu    8, 4(11)
        lwzu    9, 4(11)
        lwzu    10, 4(11)
        lwzu    14, 4(11)
        lwzu    15, 4(11)
        lwzu    16, 4(11)
        lwzu    17, 4(11)
        lwzu    18, 4(11)
        lwzu    19, 4(11)
        lwzu    20, 4(11)
        lwzu    21, 4(11)
        lwzu    22, 4(11)
        lwzu    23, 4(11)
        lwzu    24, 4(11)
        lwzu    25, 4(11)
        lwzu    26, 4(11)
        lwzu    27, 4(11)
        lwzu    28, 4(11)
        addi    11, 1, 32 - 8
        lfdu    1, 8(11)
        lfdu    2, 8(11)
        lfdu    3, 8(11)
        lfdu    4, 8(11)
        lfdu    5, 8(11)
        lfdu    6, 8(11)
        lfdu    7, 8(11)
        lfdu    8, 8(11)
        lfdu    9, 8(11)
        lfdu    10, 8(11)
        lfdu    11, 8(11)
        lfdu    12, 8(11)
        lfdu    13, 8(11)
        lfdu    14, 8(11)
        lfdu    15, 8(11)
        lfdu    16, 8(11)
        lfdu    17, 8(11)
        lfdu    18, 8(11)
        lfdu    19, 8(11)
        lfdu    20, 8(11)
        lfdu    21, 8(11)
        lfdu    22, 8(11)
        lfdu    23, 8(11)
        lfdu    24, 8(11)
        lfdu    25, 8(11)
        lfdu    26, 8(11)
        lfdu    27, 8(11)
        lfdu    28, 8(11)
        lfdu    29, 8(11)
        lfdu    30, 8(11)
        lfdu    31, 8(11)
    /* Return to caller, restarting the allocation */
        Loadglobal(0, caml_last_return_address, 11)
        addic   0, 0, -16     /* Restart the allocation (4 instructions) */
        mtlr    0
    /* Say we are back into OCaml code */
        li      12, 0
        Storeglobal(12, caml_last_return_address, 11)
    /* Deallocate stack frame */
        addi    1, 1, 0x1A0
    /* Return */
        blr

/* Call a C function from OCaml */

        .globl  caml_c_call
        .type   caml_c_call, @function
caml_c_call:
    /* Save return address */
        mflr    25
    /* Get ready to call C function (address in 11) */
        mtctr   11
    /* Record lowest stack address and return address */
        Storeglobal(1, caml_bottom_of_stack, 12)
        Storeglobal(25, caml_last_return_address, 12)
    /* Make the exception handler and alloc ptr available to the C code */
        Storeglobal(31, caml_young_ptr, 11)
        Storeglobal(29, caml_exception_pointer, 11)
    /* Call the function (address in CTR register) */
        bctrl
    /* Restore return address (in 25, preserved by the C function) */
        mtlr    25
    /* Reload allocation pointer and allocation limit*/
        Loadglobal(31, caml_young_ptr, 11)
        Loadglobal(30, caml_young_limit, 11)
    /* Return to caller */
        blr

/* Raise an exception from OCaml */
        .globl  caml_raise_exn
        .type   caml_raise_exn, @function
caml_raise_exn:
        Loadglobal(0, caml_backtrace_active, 11)
        cmpwi   0, 0
        bne     .L111
.L110:
    /* Pop trap frame */
        lwz     0, 0(29)
        mr      1, 29
        mtctr   0
        lwz     29, 4(29)
        addi    1, 1, 16
    /* Branch to handler */
        bctr
.L111:
        li      0, 0
        Storeglobal(0, caml_backtrace_pos, 11)
.L112:
        mr      28, 3           /* preserve exn bucket in callee-save reg */
                                /* arg1: exception bucket, already in r3 */
        mflr    4               /* arg2: PC of raise */
        mr      5, 1            /* arg3: SP of raise */
        mr      6, 29           /* arg4: SP of handler */
        addi    1, 1, -16       /* reserve stack space for C call */
        bl      caml_stash_backtrace
        mr      3, 28           /* restore exn bucket */
        b       .L110           /* raise the exn */

        .globl  caml_reraise_exn
        .type   caml_reraise_exn, @function
caml_reraise_exn:
        Loadglobal(0, caml_backtrace_active, 11)
        cmpwi   0, 0
        bne-    .L112
    /* Pop trap frame */
        lwz     0, 0(29)
        mr      1, 29
        mtctr   0
        lwz     29, 4(29)
        addi    1, 1, 16
    /* Branch to handler */
        bctr

/* Raise an exception from C */

        .globl  caml_raise_exception
        .type   caml_raise_exception, @function
caml_raise_exception:
        Loadglobal(0, caml_backtrace_active, 11)
        cmpwi   0, 0
        bne     .L121
.L120:
    /* Reload OCaml global registers */
        Loadglobal(1, caml_exception_pointer, 11)
        Loadglobal(31, caml_young_ptr, 11)
        Loadglobal(30, caml_young_limit, 11)
    /* Pop trap frame */
        lwz     0, 0(1)
        lwz     29, 4(1)
        mtctr   0
        addi    1, 1, 16
    /* Branch to handler */
        bctr
.L121:
        li      0, 0
        Storeglobal(0, caml_backtrace_pos, 11)
        mr      28, 3           /* preserve exn bucket in callee-save reg */
                                /* arg1: exception bucket, already in r3 */
        Loadglobal(4, caml_last_return_address, 11) /* arg2: PC of raise */
        Loadglobal(5, caml_bottom_of_stack, 11)     /* arg3: SP of raise */
        Loadglobal(6, caml_exception_pointer, 11)   /* arg4: SP of handler */
        addi    1, 1, -16       /* reserve stack space for C call */
        bl      caml_stash_backtrace
        mr      3, 28           /* restore exn bucket */
        b       .L120           /* raise the exn */


/* Start the OCaml program */

        .globl  caml_start_program
        .type   caml_start_program, @function
caml_start_program:
        Addrglobal(12, caml_program)

/* Code shared between caml_start_program and caml_callback */
.L102:
    /* Allocate and link stack frame */
        stwu    1, -256(1)
    /* Save return address */
        mflr    0
        stw     0, 256+4(1)
    /* Save all callee-save registers */
    /* GPR 14 at sp+16 ... GPR 31 at sp+84
       FPR 14 at sp+92 ... FPR 31 at sp+228 */
        addi    11, 1, 16-4
        stwu    14, 4(11)
        stwu    15, 4(11)
        stwu    16, 4(11)
        stwu    17, 4(11)
        stwu    18, 4(11)
        stwu    19, 4(11)
        stwu    20, 4(11)
        stwu    21, 4(11)
        stwu    22, 4(11)
        stwu    23, 4(11)
        stwu    24, 4(11)
        stwu    25, 4(11)
        stwu    26, 4(11)
        stwu    27, 4(11)
        stwu    28, 4(11)
        stwu    29, 4(11)
        stwu    30, 4(11)
        stwu    31, 4(11)
        stfdu   14, 8(11)
        stfdu   15, 8(11)
        stfdu   16, 8(11)
        stfdu   17, 8(11)
        stfdu   18, 8(11)
        stfdu   19, 8(11)
        stfdu   20, 8(11)
        stfdu   21, 8(11)
        stfdu   22, 8(11)
        stfdu   23, 8(11)
        stfdu   24, 8(11)
        stfdu   25, 8(11)
        stfdu   26, 8(11)
        stfdu   27, 8(11)
        stfdu   28, 8(11)
        stfdu   29, 8(11)
        stfdu   30, 8(11)
        stfdu   31, 8(11)
    /* Set up a callback link */
        addi    1, 1, -16
        Loadglobal(9, caml_bottom_of_stack, 11)
        Loadglobal(10, caml_last_return_address, 11)
        Loadglobal(11, caml_gc_regs, 11)
        stw     9, 0(1)
        stw     10, 4(1)
        stw     11, 8(1)
    /* Build an exception handler to catch exceptions escaping out of OCaml */
        bl      .L103
        b       .L104
.L103:
        addi    1, 1, -16
        mflr    0
        stw     0, 0(1)
        Loadglobal(11, caml_exception_pointer, 11)
        stw     11, 4(1)
        mr      29, 1
    /* Reload allocation pointers */
        Loadglobal(31, caml_young_ptr, 11)
        Loadglobal(30, caml_young_limit, 11)
    /* Say we are back into OCaml code */
        li      0, 0
        Storeglobal(0, caml_last_return_address, 11)
    /* Call the OCaml code */
        mtlr    12
.L105:
        blrl
    /* Pop the trap frame, restoring caml_exception_pointer */
        lwz     9, 4(1)
        Storeglobal(9, caml_exception_pointer, 11)
        addi    1, 1, 16
    /* Pop the callback link, restoring the global variables */
.L106:
        lwz     9, 0(1)
        lwz     10, 4(1)
        lwz     11, 8(1)
        Storeglobal(9, caml_bottom_of_stack, 12)
        Storeglobal(10, caml_last_return_address, 12)
        Storeglobal(11, caml_gc_regs, 12)
        addi    1, 1, 16
    /* Update allocation pointer */
        Storeglobal(31, caml_young_ptr, 11)
    /* Restore callee-save registers */
        addi    11, 1, 16-4
        lwzu    14, 4(11)
        lwzu    15, 4(11)
        lwzu    16, 4(11)
        lwzu    17, 4(11)
        lwzu    18, 4(11)
        lwzu    19, 4(11)
        lwzu    20, 4(11)
        lwzu    21, 4(11)
        lwzu    22, 4(11)
        lwzu    23, 4(11)
        lwzu    24, 4(11)
        lwzu    25, 4(11)
        lwzu    26, 4(11)
        lwzu    27, 4(11)
        lwzu    28, 4(11)
        lwzu    29, 4(11)
        lwzu    30, 4(11)
        lwzu    31, 4(11)
        lfdu    14, 8(11)
        lfdu    15, 8(11)
        lfdu    16, 8(11)
        lfdu    17, 8(11)
        lfdu    18, 8(11)
        lfdu    19, 8(11)
        lfdu    20, 8(11)
        lfdu    21, 8(11)
        lfdu    22, 8(11)
        lfdu    23, 8(11)
        lfdu    24, 8(11)
        lfdu    25, 8(11)
        lfdu    26, 8(11)
        lfdu    27, 8(11)
        lfdu    28, 8(11)
        lfdu    29, 8(11)
        lfdu    30, 8(11)
        lfdu    31, 8(11)
    /* Reload return address */
        lwz     0, 256+4(1)
        mtlr    0
    /* Return */
        addi    1, 1, 256
        blr

    /* The trap handler: */
.L104:
    /* Update caml_exception_pointer */
        Storeglobal(29, caml_exception_pointer, 11)
    /* Encode exception bucket as an exception result and return it */
        ori     3, 3, 2
        b       .L106

/* Callback from C to OCaml */

        .globl  caml_callback_exn
        .type   caml_callback_exn, @function
caml_callback_exn:
    /* Initial shuffling of arguments */
        mr      0, 3            /* Closure */
        mr      3, 4            /* Argument */
        mr      4, 0
        lwz     12, 0(4)        /* Code pointer */
        b       .L102

        .globl  caml_callback2_exn
        .type   caml_callback2_exn, @function
caml_callback2_exn:
        mr      0, 3            /* Closure */
        mr      3, 4            /* First argument */
        mr      4, 5            /* Second argument */
        mr      5, 0
        Addrglobal(12, caml_apply2)
        b       .L102

        .globl  caml_callback3_exn
        .type   caml_callback3_exn, @function
caml_callback3_exn:
        mr      0, 3            /* Closure */
        mr      3, 4            /* First argument */
        mr      4, 5            /* Second argument */
        mr      5, 6            /* Third argument */
        mr      6, 0
        Addrglobal(12, caml_apply3)
        b       .L102

        .globl  caml_system__code_end
caml_system__code_end:

/* Frame table */

        .section ".data"
        .globl  caml_system__frametable
        .type   caml_system__frametable, @object
caml_system__frametable:
        .long   1               /* one descriptor */
        .long   .L105 + 4       /* return address into callback */
        .short  -1              /* negative size count => use callback link */
        .short  0               /* no roots here */
