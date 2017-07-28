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

#define ST8OFF(a,b,d) st8 [a] = b, d
#define LD8OFF(a,b,d) ld8 a = [b], d
#define STFDOFF(a,b,d) stfd [a] = b, d
#define LDFDOFF(a,b,d) ldfd a = [b], d
#define STFSPILLOFF(a,b,d) stf.spill [a] = b, d
#define LDFFILLOFF(a,b,d) ldf.fill a = [b], d

        .text
        .align 16

        .global call_gen_code#
        .proc   call_gen_code#

call_gen_code:
        /* Allocate 64 "out" registers (for the OCaml code) and no locals */
        alloc   r3 = ar.pfs, 0, 0, 64, 0

        /* Save PFS, return address and GP on stack */
        add     sp = -368, sp ;;
        add     r2 = 16, sp ;;
        ST8OFF(r2,r3,8) ;;
        mov     r3 = b0 ;;
        ST8OFF(r2,r3,8) ;;
        ST8OFF(r2,gp,8) ;;

        /* Save predicates on stack */
        mov     r3 = pr ;;
        st8     [r2] = r3

        /* Save callee-save floating-point registers on stack */
        add     r2 = 48, sp
        add     r3 = 64, sp ;;
        STFSPILLOFF(r2,f2,16) ;;
        STFSPILLOFF(r3,f3,16) ;;
        STFSPILLOFF(r2,f4,16) ;;
        STFSPILLOFF(r3,f5,16) ;;
        STFSPILLOFF(r2,f16,16) ;;
        STFSPILLOFF(r3,f17,16) ;;
        STFSPILLOFF(r2,f18,16) ;;
        STFSPILLOFF(r3,f19,16) ;;
        STFSPILLOFF(r2,f20,16) ;;
        STFSPILLOFF(r3,f21,16) ;;
        STFSPILLOFF(r2,f22,16) ;;
        STFSPILLOFF(r3,f23,16) ;;
        STFSPILLOFF(r2,f24,16) ;;
        STFSPILLOFF(r3,f25,16) ;;
        STFSPILLOFF(r2,f26,16) ;;
        STFSPILLOFF(r3,f27,16) ;;
        STFSPILLOFF(r2,f28,16) ;;
        STFSPILLOFF(r3,f29,16) ;;
        STFSPILLOFF(r2,f30,16) ;;
        STFSPILLOFF(r3,f31,16) ;;

        /* Recover entry point and gp from the function pointer in in0 */
        LD8OFF(r2,r32,8) ;;
        ld8     r3 = [r32] ;;
        mov     b6 = r2
        mov     gp = r3 ;;

        /* Shift arguments r33 ... r35 to r32 ... r34 */
        mov     r32 = r33
        mov     r33 = r34
        mov     r34 = r35

        /* Do the call */
        br.call.sptk b0 = b6 ;;

        /* Restore the saved floating-point registers */
        add     r2 = 48, sp
        add     r3 = 64, sp ;;
        LDFFILLOFF(f2,r2,16) ;;
        LDFFILLOFF(f3,r3,16) ;;
        LDFFILLOFF(f4,r2,16) ;;
        LDFFILLOFF(f5,r3,16) ;;
        LDFFILLOFF(f16,r2,16) ;;
        LDFFILLOFF(f17,r3,16) ;;
        LDFFILLOFF(f18,r2,16) ;;
        LDFFILLOFF(f19,r3,16) ;;
        LDFFILLOFF(f20,r2,16) ;;
        LDFFILLOFF(f21,r3,16) ;;
        LDFFILLOFF(f22,r2,16) ;;
        LDFFILLOFF(f23,r3,16) ;;
        LDFFILLOFF(f24,r2,16) ;;
        LDFFILLOFF(f25,r3,16) ;;
        LDFFILLOFF(f26,r2,16) ;;
        LDFFILLOFF(f27,r3,16) ;;
        LDFFILLOFF(f28,r2,16) ;;
        LDFFILLOFF(f29,r3,16) ;;
        LDFFILLOFF(f30,r2,16) ;;
        LDFFILLOFF(f31,r3,16) ;;

        /* Restore gp, predicates and return */
        add     r2 = 16, sp ;;
        LD8OFF(r3,r2,8) ;;
        mov     ar.pfs = r3
        LD8OFF(r3,r2,8) ;;
        mov     b0 = r3
        LD8OFF(gp,r2,8) ;;
        LD8OFF(r3,r2,8) ;;
        mov     pr = r3, -1

        br.ret.sptk.many b0 ;;

        .endp   call_gen_code#
