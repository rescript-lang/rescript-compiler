(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*         Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt         *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open X86_ast
open X86_proc

let bprintf = Printf.bprintf

let string_of_datatype = function
  | QWORD -> "QWORD"
  | OWORD -> "OWORD"
  | NONE -> assert false
  | REAL4 -> "REAL4"
  | REAL8 -> "REAL8"
  | BYTE -> "BYTE"
  | WORD -> "WORD"
  | DWORD -> "DWORD"
  | NEAR -> "NEAR"
  | PROC -> "PROC"


let string_of_datatype_ptr = function
  | QWORD -> "QWORD PTR "
  | OWORD -> "OWORD PTR "
  | NONE -> ""
  | REAL4 -> "REAL4 PTR "
  | REAL8 -> "REAL8 PTR "
  | BYTE -> "BYTE PTR "
  | WORD -> "WORD PTR "
  | DWORD -> "DWORD PTR "
  | NEAR -> "NEAR PTR "
  | PROC -> "PROC PTR "

let arg_mem b {arch; typ; idx; scale; base; sym; displ} =
  let string_of_register =
    match arch with
    | X86 -> string_of_reg32
    | X64 -> string_of_reg64
  in
  Buffer.add_string b (string_of_datatype_ptr typ);
  Buffer.add_char b '[';
  begin match sym with
  | None -> ()
  | Some s -> Buffer.add_string b s
  end;
  if scale <> 0 then begin
    if sym <> None then Buffer.add_char b '+';
    Buffer.add_string b (string_of_register idx);
    if scale <> 1 then bprintf b "*%d" scale;
  end;
  begin match base with
  | None -> ()
  | Some r ->
      assert(scale > 0);
      Buffer.add_char b '+';
      Buffer.add_string b (string_of_register r);
  end;
  begin if displ > 0 then bprintf b "+%d" displ
    else if displ < 0 then bprintf b "%d" displ
  end;
  Buffer.add_char b ']'

let arg b = function
  | Sym s -> bprintf b "OFFSET %s" s
  | Imm n when n <= 0x7FFF_FFFFL && n >= -0x8000_0000L -> bprintf b "%Ld" n
  | Imm int -> bprintf b "0%LxH" int (* force ml64 to use mov reg, imm64 *)
  | Reg8L x -> Buffer.add_string b (string_of_reg8l x)
  | Reg8H x -> Buffer.add_string b (string_of_reg8h x)
  | Reg16 x -> Buffer.add_string b (string_of_reg16 x)
  | Reg32 x -> Buffer.add_string b (string_of_reg32 x)
  | Reg64 x -> Buffer.add_string b (string_of_reg64 x)
  | Regf x -> Buffer.add_string b (string_of_registerf x)

  (* We don't need to specify RIP on Win64, since EXTERN will provide
     the list of external symbols that need this addressing mode, and
     MASM will automatically use RIP addressing when needed. *)
  | Mem64_RIP (typ, s, displ) ->
      bprintf b "%s%s" (string_of_datatype_ptr typ) s;
      if displ > 0 then bprintf b "+%d" displ
      else if displ < 0 then bprintf b "%d" displ
  | Mem addr -> arg_mem b addr

let rec cst b = function
  | ConstLabel _ | Const _ | ConstThis as c -> scst b c
  | ConstAdd (c1, c2) -> bprintf b "%a + %a" scst c1 scst c2
  | ConstSub (c1, c2) -> bprintf b "%a - %a" scst c1 scst c2

and scst b = function
  | ConstThis -> Buffer.add_string b "THIS BYTE"
  | ConstLabel l -> Buffer.add_string b l
  | Const n when n <= 0x7FFF_FFFFL && n >= -0x8000_0000L ->
      Buffer.add_string b (Int64.to_string n)
  | Const n -> bprintf b "0%LxH" n
  | ConstAdd (c1, c2) -> bprintf b "(%a + %a)" scst c1 scst c2
  | ConstSub (c1, c2) -> bprintf b "(%a - %a)" scst c1 scst c2

let i0 b s = bprintf b "\t%s" s
let i1 b s x = bprintf b "\t%s\t%a" s arg x
let i2 b s x y = bprintf b "\t%s\t%a, %a" s arg y arg x

let i1_call_jmp b s = function
  | Sym x -> bprintf b "\t%s\t%s" s x
  | x -> i1 b s x

let print_instr b = function
  | ADD (arg1, arg2) -> i2 b "add" arg1 arg2
  | ADDSD (arg1, arg2) -> i2 b "addsd" arg1 arg2
  | AND (arg1, arg2) -> i2 b "and" arg1 arg2
  | ANDPD (arg1, arg2) -> i2 b "andpd" arg1 arg2
  | BSWAP arg -> i1 b "bswap" arg
  | CALL arg  -> i1_call_jmp b "call" arg
  | CDQ -> i0 b "cdq"
  | CMOV (c, arg1, arg2) -> i2 b ("cmov" ^ string_of_condition c) arg1 arg2
  | CMP (arg1, arg2) -> i2 b "cmp" arg1 arg2
  | COMISD (arg1, arg2) -> i2 b "comisd" arg1 arg2
  | CQO -> i0 b "cqo"
  | CVTSD2SI (arg1, arg2) -> i2 b "cvtsd2si" arg1 arg2
  | CVTSD2SS (arg1, arg2) -> i2 b "cvtsd2ss" arg1 arg2
  | CVTSI2SD (arg1, arg2) -> i2 b "cvtsi2sd" arg1 arg2
  | CVTSS2SD (arg1, arg2) -> i2 b "cvtss2sd" arg1 arg2
  | CVTTSD2SI (arg1, arg2) -> i2 b "cvttsd2si" arg1 arg2
  | DEC arg -> i1 b "dec" arg
  | DIVSD (arg1, arg2) -> i2 b "divsd" arg1 arg2
  | FABS -> i0 b "fabs"
  | FADD arg -> i1 b "fadd" arg
  | FADDP (arg1, arg2)  -> i2 b "faddp" arg1 arg2
  | FCHS -> i0 b "fchs"
  | FCOMP arg -> i1 b "fcomp" arg
  | FCOMPP -> i0 b "fcompp"
  | FCOS -> i0 b "fcos"
  | FDIV arg -> i1 b "fdiv" arg
  | FDIVP (arg1, arg2)  -> i2 b "fdivp" arg1 arg2
  | FDIVR arg -> i1 b "fdivr" arg
  | FDIVRP (arg1, arg2)  -> i2 b "fdivrp" arg1 arg2
  | FILD arg -> i1 b "fild" arg
  | FISTP arg -> i1 b "fistp" arg
  | FLD arg -> i1 b "fld" arg
  | FLD1 -> i0 b "fld1"
  | FLDCW arg -> i1 b "fldcw" arg
  | FLDLG2 -> i0 b "fldlg2"
  | FLDLN2 -> i0 b "fldln2"
  | FLDZ -> i0 b "fldz"
  | FMUL arg -> i1 b "fmul" arg
  | FMULP (arg1, arg2)  -> i2 b "fmulp" arg1 arg2
  | FNSTCW arg -> i1 b "fnstcw" arg
  | FNSTSW arg -> i1 b "fnstsw" arg
  | FPATAN -> i0 b "fpatan"
  | FPTAN -> i0 b "fptan"
  | FSIN -> i0 b "fsin"
  | FSQRT -> i0 b "fsqrt"
  | FSTP arg -> i1 b "fstp" arg
  | FSUB arg -> i1 b "fsub" arg
  | FSUBP (arg1, arg2)  -> i2 b "fsubp" arg1 arg2
  | FSUBR arg -> i1 b "fsubr" arg
  | FSUBRP (arg1, arg2)  -> i2 b "fsubrp" arg1 arg2
  | FXCH arg -> i1 b "fxch" arg
  | FYL2X -> i0 b "fyl2x"
  | HLT -> assert false
  | IDIV arg -> i1 b "idiv" arg
  | IMUL (arg, None) -> i1 b "imul" arg
  | IMUL (arg1, Some arg2) -> i2 b "imul" arg1 arg2
  | INC arg -> i1 b "inc" arg
  | J (c, arg) -> i1_call_jmp b ("j" ^ string_of_condition c) arg
  | JMP arg -> i1_call_jmp b "jmp" arg
  | LEA (arg1, arg2) -> i2 b "lea" arg1 arg2
  | LEAVE -> i0 b "leave"
  | MOV (Imm n as arg1, Reg64 r) when
      n >= 0x8000_0000L && n <= 0xFFFF_FFFFL ->
      (* Work-around a bug in ml64.  Use a mov to the corresponding
         32-bit lower register when the constant fits in 32-bit.
         The associated higher 32-bit register will be zeroed. *)
      i2 b "mov" arg1 (Reg32 r)
  | MOV (arg1, arg2) -> i2 b "mov" arg1 arg2
  | MOVAPD (arg1, arg2) -> i2 b "movapd" arg1 arg2
  | MOVLPD (arg1, arg2) -> i2 b "movlpd" arg1 arg2
  | MOVSD (arg1, arg2) -> i2 b "movsd" arg1 arg2
  | MOVSS (arg1, arg2) -> i2 b "movss" arg1 arg2
  | MOVSX (arg1, arg2) -> i2 b "movsx" arg1 arg2
  | MOVSXD (arg1, arg2) -> i2 b "movsxd" arg1 arg2
  | MOVZX (arg1, arg2) -> i2 b "movzx" arg1 arg2
  | MULSD (arg1, arg2) -> i2 b "mulsd" arg1 arg2
  | NEG arg -> i1 b "neg" arg
  | NOP -> i0 b "nop"
  | OR (arg1, arg2) -> i2 b "or" arg1 arg2
  | POP arg -> i1 b "pop" arg
  | PUSH arg -> i1 b "push" arg
  | RET -> i0 b "ret"
  | ROUNDSD (r, arg1, arg2) -> i2 b (string_of_rounding r) arg1 arg2
  | SAL (arg1, arg2) -> i2 b "sal" arg1 arg2
  | SAR (arg1, arg2) -> i2 b "sar" arg1 arg2
  | SET (c, arg) -> i1 b ("set" ^ string_of_condition c) arg
  | SHR (arg1, arg2) -> i2 b "shr" arg1 arg2
  | SQRTSD (arg1, arg2) -> i2 b "sqrtsd" arg1 arg2
  | SUB (arg1, arg2) -> i2 b "sub" arg1 arg2
  | SUBSD (arg1, arg2) -> i2 b "subsd" arg1 arg2
  | TEST (arg1, arg2) -> i2 b "test" arg1 arg2
  | UCOMISD (arg1, arg2) -> i2 b "ucomisd" arg1 arg2
  | XCHG (arg1, arg2) -> i2 b "xchg" arg1 arg2
  | XOR (arg1, arg2) -> i2 b "xor" arg1 arg2
  | XORPD (arg1, arg2) -> i2 b "xorpd" arg1 arg2


let print_line b = function
  | Ins instr -> print_instr b instr

  | Align (_data,n) -> bprintf b "\tALIGN\t%d" n
  | Byte n -> bprintf b "\tBYTE\t%a" cst n
  | Bytes s -> buf_bytes_directive b "BYTE" s
  | Comment s -> bprintf b " ; %s " s
  | Global s -> bprintf b "\tPUBLIC\t%s" s
  | Long n -> bprintf b "\tDWORD\t%a" cst n
  | NewLabel (s, NONE) -> bprintf b "%s:" s
  | NewLabel (s, ptr) -> bprintf b "%s LABEL %s" s (string_of_datatype ptr)
  | Quad n -> bprintf b "\tQWORD\t%a" cst n
  | Section ([".data"], None, []) -> bprintf b "\t.DATA"
  | Section ([".text"], None, []) -> bprintf b "\t.CODE"
  | Section _ -> assert false
  | Space n -> bprintf b "\tBYTE\t%d DUP (?)" n
  | Word n -> bprintf b "\tWORD\t%a" cst n

  (* windows only *)
  | External (s, ptr) -> bprintf b "\tEXTRN\t%s: %s" s (string_of_datatype ptr)
  | Mode386 -> bprintf b "\t.386"
  | Model name -> bprintf b "\t.MODEL %s" name (* name = FLAT *)

  (* gas only *)
  | Cfi_adjust_cfa_offset _
  | Cfi_endproc
  | Cfi_startproc
  | File _
  | Indirect_symbol _
  | Loc _
  | Private_extern _
  | Set _
  | Size _
  | Type _
    -> assert false

let generate_asm oc lines =
  let b = Buffer.create 10000 in
  List.iter
    (fun i ->
       Buffer.clear b;
       print_line b i;
       Buffer.add_char b '\n';
       Buffer.output_buffer oc b
    )
    lines;
  output_string oc "\tEND\n"
