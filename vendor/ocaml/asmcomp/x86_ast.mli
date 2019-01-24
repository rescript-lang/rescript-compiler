(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Structured representation of Intel assembly language (32 and 64 bit). *)

type condition =
  | L | GE     (* signed comparisons: less/greater *)
  | LE | G
  | B | AE     (* unsigned comparisons: below/above *)
  | BE | A
  | E | NE     (* equal *)
  | O | NO     (* overflow *)
  | S | NS     (* sign *)
  | P | NP     (* parity *)

type rounding =
  | RoundUp
  | RoundDown
  | RoundNearest
  | RoundTruncate

type constant =
  | Const of int64
  | ConstThis
  | ConstLabel of string
  | ConstAdd of constant * constant
  | ConstSub of constant * constant

(* data_type is used mainly on memory addressing to specify
   the size of the addressed memory chunk.  It is directly
   used by the MASM emitter and indirectly by the GAS emitter
   to infer the instruction suffix. *)

type data_type =
  | NONE
  | REAL4 | REAL8 (* floating point values *)
  | BYTE | WORD | DWORD | QWORD | OWORD (* integer values *)
  | NEAR | PROC

type reg64 =
  | RAX | RBX | RCX | RDX | RSP | RBP | RSI | RDI
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

type reg8h =
  | AH | BH | CH | DH


type registerf = XMM of int | TOS | ST of int

type arch = X64 | X86

type addr =
  {
    arch: arch;
    typ: data_type;
    idx: reg64;
    scale: int;
    base: reg64 option;
    sym: string option;
    displ: int;
  }
  (** Addressing modes:
      displ + sym + base + idx * scale
      (if scale = 0, idx is ignored and base must be None)
  *)

type arg =
  | Imm of int64
  (** Operand is an immediate constant integer *)

  | Sym of  string
  (** Address of a symbol (absolute address except for call/jmp target
      where it is interpreted as a relative displacement *)

  | Reg8L of reg64
  | Reg8H of reg8h
  | Reg16 of reg64
  | Reg32 of reg64
  | Reg64 of reg64
  | Regf of registerf

  | Mem of addr
  | Mem64_RIP of data_type * string * int

type instruction =
  | ADD of arg * arg
  | ADDSD of arg * arg
  | AND of arg * arg
  | ANDPD of arg * arg
  | BSWAP of arg
  | CALL of arg
  | CDQ
  | CMOV of condition * arg * arg
  | CMP of arg * arg
  | COMISD of arg * arg
  | CQO
  | CVTSD2SI of arg * arg
  | CVTSD2SS of arg * arg
  | CVTSI2SD of arg * arg
  | CVTSS2SD of arg * arg
  | CVTTSD2SI of arg * arg
  | DEC of arg
  | DIVSD of arg * arg
  | FABS
  | FADD of arg
  | FADDP of arg * arg
  | FCHS
  | FCOMP of arg
  | FCOMPP
  | FCOS
  | FDIV of arg
  | FDIVP of arg * arg
  | FDIVR of arg
  | FDIVRP of arg * arg
  | FILD of arg
  | FISTP of arg
  | FLD of arg
  | FLD1
  | FLDCW of arg
  | FLDLG2
  | FLDLN2
  | FLDZ
  | FMUL of arg
  | FMULP of arg * arg
  | FNSTCW of arg
  | FNSTSW of arg
  | FPATAN
  | FPTAN
  | FSIN
  | FSQRT
  | FSTP of arg
  | FSUB of arg
  | FSUBP of arg * arg
  | FSUBR of arg
  | FSUBRP of arg * arg
  | FXCH of arg
  | FYL2X
  | HLT
  | IDIV of arg
  | IMUL of arg * arg option
  | INC of arg
  | J of condition * arg
  | JMP of arg
  | LEA of arg * arg
  | LEAVE
  | MOV of arg * arg
  | MOVAPD of arg * arg
  | MOVLPD of arg * arg
  | MOVSD of arg * arg
  | MOVSS of arg * arg
  | MOVSX of arg * arg
  | MOVSXD of arg * arg
  | MOVZX of arg * arg
  | MULSD of arg * arg
  | NEG of arg
  | NOP
  | OR of arg * arg
  | POP of arg
  | PUSH of arg
  | RET
  | ROUNDSD of rounding * arg * arg
  | SAL of arg * arg
  | SAR of arg * arg
  | SET of condition * arg
  | SHR of arg * arg
  | SQRTSD of arg * arg
  | SUB of arg * arg
  | SUBSD of arg * arg
  | TEST of arg * arg
  | UCOMISD of arg * arg
  | XCHG of arg * arg
  | XOR of arg * arg
  | XORPD of arg * arg

type asm_line =
  | Ins of instruction

  | Align of bool * int
  | Byte of constant
  | Bytes of string
  | Comment of string
  | Global of string
  | Long of constant
  | NewLabel of string * data_type
  | Quad of constant
  | Section of string list * string option * string list
  | Space of int
  | Word of constant

  (* masm only (the gas emitter will fail on them) *)
  | External of string * data_type
  | Mode386
  | Model of string

  (* gas only (the masm emitter will fail on them) *)
  | Cfi_adjust_cfa_offset of int
  | Cfi_endproc
  | Cfi_startproc
  | File of int * string (* (file_num, file_name) *)
  | Indirect_symbol of string
  | Loc of int * int * int (* (file_num, line, col) *)
  | Private_extern of string
  | Set of string * constant
  | Size of string * constant
  | Type of string * string

type asm_program = asm_line list
