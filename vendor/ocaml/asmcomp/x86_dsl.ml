(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*         Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt         *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Helpers for Intel code generators *)

(* The DSL* modules expose functions to emit x86/x86_64 instructions
   using a syntax close to AT&T (in particular, arguments are reversed compared
   to the official Intel syntax).

   Some notes:

     - Unary floating point instructions such as fadd/fmul/fstp/fld/etc.
       come with a single version supporting both the single and double
       precision instructions.  (As with Intel syntax.)

     - A legacy bug in GAS:
   https://sourceware.org/binutils/docs-2.22/as/i386_002dBugs.html#i386_002dBugs
       is not replicated here.  It is managed by X86_gas.
*)


open X86_ast
open X86_proc

let sym s = Sym s

let nat n = Imm (Int64.of_nativeint n)
let int n = Imm (Int64.of_int n)

let const_32 n = Const (Int64.of_int32 n)
let const_nat n = Const (Int64.of_nativeint n)
let const n = Const (Int64.of_int n)

let al  = Reg8L RAX
let ah  = Reg8H AH
let cl  = Reg8L RCX
let ax  = Reg16 RAX
let rax = Reg64 RAX
let r10 = Reg64 R10
let r11 = Reg64 R11
let r13 = Reg64 R13
let r14 = Reg64 R14
let r15 = Reg64 R15
let rsp = Reg64 RSP
let rbp = Reg64 RBP
let xmm15 = Regf (XMM 15)
let eax = Reg32 RAX
let ebx = Reg32 RBX
let ecx = Reg32 RCX
let edx = Reg32 RDX
let ebp = Reg32 RBP
let esp = Reg32 RSP
let st0 = Regf (ST 0)
let st1 = Regf (ST 1)

let mem32 typ ?(scale = 1) ?base ?sym displ idx =
  assert(scale >= 0);
  Mem {arch = X86; typ; idx; scale; base; sym; displ}

let mem64 typ ?(scale = 1) ?base ?sym displ idx =
  assert(scale > 0);
  Mem {arch = X64; typ; idx; scale; base; sym; displ}

let mem64_rip typ ?(ofs = 0) s =
  Mem64_RIP (typ, s, ofs)

module D = struct
  let section segment flags args = directive (Section (segment, flags, args))
  let align n = directive (Align (false, n))
  let byte n = directive (Byte n)
  let bytes s = directive (Bytes s)
  let cfi_adjust_cfa_offset n = directive (Cfi_adjust_cfa_offset n)
  let cfi_endproc () = directive Cfi_endproc
  let cfi_startproc () = directive Cfi_startproc
  let comment s = directive (Comment s)
  let data () = section [ ".data" ] None []
  let extrn s ptr = directive (External (s, ptr))
  let file ~file_num ~file_name = directive (File (file_num, file_name))
  let global s = directive (Global s)
  let indirect_symbol s = directive (Indirect_symbol s)
  let label ?(typ = NONE) s = directive (NewLabel (s, typ))
  let loc ~file_num ~line ~col = directive (Loc (file_num, line, col))
  let long cst = directive (Long cst)
  let mode386 () = directive Mode386
  let model name = directive (Model name)
  let private_extern s = directive (Private_extern s)
  let qword cst = directive (Quad cst)
  let setvar (x, y) = directive (Set (x, y))
  let size name cst = directive (Size (name, cst))
  let space n = directive (Space n)
  let text () = section [ ".text" ] None []
  let type_ name typ = directive (Type (name, typ))
  let word cst = directive (Word cst)
end

module I = struct
  let add x y = emit (ADD (x, y))
  let addsd x y = emit (ADDSD (x, y))
  let and_ x y= emit (AND (x, y))
  let andpd x y = emit (ANDPD (x, y))
  let bswap x = emit (BSWAP x)
  let call x = emit (CALL x)
  let cdq () = emit CDQ
  let cmp x y = emit (CMP (x, y))
  let comisd x y = emit (COMISD (x, y))
  let cqo () = emit CQO
  let cvtsd2ss x y = emit (CVTSD2SS (x, y))
  let cvtsi2sd x y = emit (CVTSI2SD (x, y))
  let cvtss2sd x y = emit (CVTSS2SD (x, y))
  let cvttsd2si x y = emit (CVTTSD2SI (x, y))
  let dec x = emit (DEC x)
  let divsd x y = emit (DIVSD (x, y))
  let fabs () = emit FABS
  let fadd x = emit (FADD x)
  let faddp x y = emit (FADDP (x, y))
  let fchs () = emit FCHS
  let fcomp x = emit (FCOMP x)
  let fcompp () = emit FCOMPP
  let fcos () = emit FCOS
  let fdiv x = emit (FDIV x)
  let fdivp x y = emit (FDIVP (x, y))
  let fdivr x = emit (FDIVR x)
  let fdivrp x y = emit (FDIVRP (x, y))
  let fild x = emit (FILD x)
  let fistp x = emit (FISTP x)
  let fld x = emit (FLD x)
  let fld1 () = emit FLD1
  let fldcw x = emit (FLDCW x)
  let fldlg2 () = emit FLDLG2
  let fldln2 () = emit FLDLN2
  let fldz () = emit FLDZ
  let fmul x = emit (FMUL x)
  let fmulp x y = emit (FMULP (x, y))
  let fnstcw x = emit (FNSTCW x)
  let fnstsw x = emit (FNSTSW x)
  let fpatan () = emit FPATAN
  let fptan () = emit FPTAN
  let fsin () = emit FSIN
  let fsqrt () = emit FSQRT
  let fstp x = emit (FSTP x)
  let fsub x = emit (FSUB x)
  let fsubp x y = emit (FSUBP (x, y))
  let fsubr x = emit (FSUBR x)
  let fsubrp x y = emit (FSUBRP (x, y))
  let fxch x = emit (FXCH x)
  let fyl2x () = emit FYL2X
  let hlt () = emit HLT
  let idiv x = emit (IDIV x)
  let imul x y = emit (IMUL (x, y))
  let inc x = emit (INC x)
  let j cond x = emit (J (cond, x))
  let ja = j A
  let jae = j AE
  let jb = j B
  let jbe = j BE
  let je = j E
  let jg = j G
  let jmp x = emit (JMP x)
  let jne = j NE
  let jp = j P
  let lea x y = emit (LEA (x, y))
  let mov x y = emit (MOV (x, y))
  let movapd x y = emit (MOVAPD (x, y))
  let movsd x y = emit (MOVSD (x, y))
  let movss x y = emit (MOVSS (x, y))
  let movsx x y = emit (MOVSX (x, y))
  let movsxd x y = emit (MOVSXD  (x, y))
  let movzx x y = emit (MOVZX (x, y))
  let mulsd x y = emit (MULSD (x, y))
  let nop () = emit NOP
  let or_ x y = emit (OR (x, y))
  let pop x = emit (POP x)
  let push x = emit (PUSH x)
  let ret () = emit RET
  let sal x y = emit (SAL (x, y))
  let sar x y = emit (SAR (x, y))
  let set cond x = emit (SET (cond, x))
  let shr x y = emit (SHR (x, y))
  let sqrtsd x y = emit (SQRTSD (x, y))
  let sub x y = emit (SUB (x, y))
  let subsd  x y = emit (SUBSD (x, y))
  let test x y= emit (TEST (x, y))
  let ucomisd x y = emit (UCOMISD (x, y))
  let xchg x y = emit (XCHG (x, y))
  let xor x y= emit (XOR (x, y))
  let xorpd x y = emit (XORPD (x, y))
end
