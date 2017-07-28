(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                  Benedikt Meurer, University of Siegen              *)
(*                                                                     *)
(*    Copyright 1998 Institut National de Recherche en Informatique    *)
(*    et en Automatique. Copyright 2012 Benedikt Meurer. All rights    *)
(*    reserved.  This file is distributed  under the terms of the Q    *)
(*    Public License version 1.0.                                      *)
(*                                                                     *)
(***********************************************************************)

(* Description of the ARM processor *)

open Misc
open Cmm
open Reg
open Arch
open Mach

(* Instruction selection *)

let word_addressed = false

(* Registers available for register allocation *)

(* Integer register map:
    r0 - r3               general purpose (not preserved)
    r4 - r7               general purpose (preserved)
    r8                    trap pointer (preserved)
    r9                    platform register, usually reserved
    r10                   allocation pointer (preserved)
    r11                   allocation limit (preserved)
    r12                   intra-procedural scratch register (not preserved)
    r13                   stack pointer
    r14                   return address
    r15                   program counter
   Floating-point register map (VFPv{2,3}):
    d0 - d7               general purpose (not preserved)
    d8 - d15              general purpose (preserved)
    d16 - d31             generat purpose (not preserved), VFPv3 only
*)

let int_reg_name =
  [| "r0"; "r1"; "r2"; "r3"; "r4"; "r5"; "r6"; "r7"; "r12" |]

let float_reg_name =
  [| "d0";  "d1";  "d2";  "d3";  "d4";  "d5";  "d6";  "d7";
     "d8";  "d9";  "d10"; "d11"; "d12"; "d13"; "d14"; "d15";
     "d16"; "d17"; "d18"; "d19"; "d20"; "d21"; "d22"; "d23";
     "d24"; "d25"; "d26"; "d27"; "d28"; "d29"; "d30"; "d31" |]

(* We have three register classes:
    0 for integer registers
    1 for VFPv2 and VFPv3-D16
    2 for VFPv3
   This way we can choose between VFPv2/VFPv3-D16 and VFPv3
   at (ocamlopt) runtime using command line switches.
*)

let num_register_classes = 3

let register_class r =
  match (r.typ, !fpu) with
    (Int | Addr), _  -> 0
  | Float, VFPv2     -> 1
  | Float, VFPv3_D16 -> 1
  | Float, _         -> 2

let num_available_registers =
  [| 9; 16; 32 |]

let first_available_register =
  [| 0; 100; 100 |]

let register_name r =
  if r < 100 then int_reg_name.(r) else float_reg_name.(r - 100)

let rotate_registers = true

(* Representation of hard registers by pseudo-registers *)

let hard_int_reg =
  let v = Array.make 9 Reg.dummy in
  for i = 0 to 8 do
    v.(i) <- Reg.at_location Int (Reg i)
  done;
  v

let hard_float_reg =
  let v = Array.make 32 Reg.dummy in
  for i = 0 to 31 do
    v.(i) <- Reg.at_location Float (Reg(100 + i))
  done;
  v

let all_phys_regs =
  Array.append hard_int_reg hard_float_reg

let phys_reg n =
  if n < 100 then hard_int_reg.(n) else hard_float_reg.(n - 100)

let stack_slot slot ty =
  Reg.at_location ty (Stack slot)

(* Calling conventions *)

let calling_conventions
    first_int last_int first_float last_float make_stack arg =
  let loc = Array.make (Array.length arg) Reg.dummy in
  let int = ref first_int in
  let float = ref first_float in
  let ofs = ref 0 in
  for i = 0 to Array.length arg - 1 do
    match arg.(i).typ with
      Int | Addr as ty ->
        if !int <= last_int then begin
          loc.(i) <- phys_reg !int;
          incr int
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) ty;
          ofs := !ofs + size_int
        end
    | Float ->
        assert (abi = EABI_HF);
        assert (!fpu >= VFPv2);
        if !float <= last_float then begin
          loc.(i) <- phys_reg !float;
          incr float
        end else begin
          ofs := Misc.align !ofs size_float;
          loc.(i) <- stack_slot (make_stack !ofs) Float;
          ofs := !ofs + size_float
        end
  done;
  (loc, Misc.align !ofs 8)  (* keep stack 8-aligned *)

let incoming ofs = Incoming ofs
let outgoing ofs = Outgoing ofs
let not_supported ofs = fatal_error "Proc.loc_results: cannot call"

(* OCaml calling convention:
     first integer args in r0...r7
     first float args in d0...d15 (EABI+VFP)
     remaining args on stack.
   Return values in r0...r7 or d0...d15. *)

let loc_arguments arg =
  calling_conventions 0 7 100 115 outgoing arg
let loc_parameters arg =
  let (loc, _) = calling_conventions 0 7 100 115 incoming arg in loc
let loc_results res =
  let (loc, _) = calling_conventions 0 7 100 115 not_supported res in loc

(* C calling convention:
     first integer args in r0...r3
     first float args in d0...d7 (EABI+VFP)
     remaining args on stack.
   Return values in r0...r1 or d0. *)

let loc_external_arguments arg =
  calling_conventions 0 3 100 107 outgoing arg
let loc_external_results res =
  let (loc, _) = calling_conventions 0 1 100 100 not_supported res in loc

let loc_exn_bucket = phys_reg 0

(* Volatile registers: none *)

let regs_are_volatile rs = false

(* Registers destroyed by operations *)

let destroyed_at_alloc =            (* r0-r6, d0-d15 preserved *)
  Array.of_list (List.map
                   phys_reg
                   [7;8;
                    116;117;118;119;120;121;122;123;
                    124;125;126;127;128;129;130;131])

let destroyed_at_c_call =
  Array.of_list (List.map
                   phys_reg
                   (match abi with
                      EABI ->       (* r4-r7 preserved *)
                        [0;1;2;3;8;
                         100;101;102;103;104;105;106;107;
                         108;109;110;111;112;113;114;115;
                         116;117;118;119;120;121;122;123;
                         124;125;126;127;128;129;130;131]
                    | EABI_HF ->    (* r4-r7, d8-d15 preserved *)
                        [0;1;2;3;8;
                         100;101;102;103;104;105;106;107;
                         116;117;118;119;120;121;122;123;
                         124;125;126;127;128;129;130;131]))

let destroyed_at_oper = function
    Iop(Icall_ind | Icall_imm _)
  | Iop(Iextcall(_, true)) ->
      all_phys_regs
  | Iop(Iextcall(_, false)) ->
      destroyed_at_c_call
  | Iop(Ialloc _) ->
      destroyed_at_alloc
  | Iop(Iconst_symbol _) when !pic_code ->
      [| phys_reg 3; phys_reg 8 |]  (* r3 and r12 destroyed *)
  | Iop(Iintop Imulh) when !arch < ARMv6 ->
      [| phys_reg 8 |]              (* r12 destroyed *)
  | Iop(Iintoffloat | Ifloatofint | Iload(Single, _) | Istore(Single, _, _)) ->
      [| phys_reg 107 |]            (* d7 (s14-s15) destroyed *)
  | _ -> [||]

let destroyed_at_raise = all_phys_regs

(* Maximal register pressure *)

let safe_register_pressure = function
    Iextcall(_, _) -> if abi = EABI then 0 else 4
  | Ialloc _ -> if abi = EABI then 0 else 7
  | Iconst_symbol _ when !pic_code -> 7
  | Iintop Imulh when !arch < ARMv6 -> 8
  | _ -> 9

let max_register_pressure = function
    Iextcall(_, _) -> if abi = EABI then [| 4; 0; 0 |] else [| 4; 8; 8 |]
  | Ialloc _ -> if abi = EABI then [| 7; 0; 0 |] else [| 7; 8; 8 |]
  | Iconst_symbol _ when !pic_code -> [| 7; 16; 32 |]
  | Iintoffloat | Ifloatofint
  | Iload(Single, _) | Istore(Single, _, _) -> [| 9; 15; 31 |]
  | Iintop Imulh when !arch < ARMv6 -> [| 8; 16; 32 |]
  | _ -> [| 9; 16; 32 |]

(* Pure operations (without any side effect besides updating their result
   registers). *)

let op_is_pure = function
  | Icall_ind | Icall_imm _ | Itailcall_ind | Itailcall_imm _
  | Iextcall _ | Istackoffset _ | Istore _ | Ialloc _
  | Iintop(Icheckbound) | Iintop_imm(Icheckbound, _)
  | Ispecific(Ishiftcheckbound _) -> false
  | _ -> true

(* Layout of the stack *)

let num_stack_slots = [| 0; 0; 0 |]
let contains_calls = ref false

(* Calling the assembler *)

let assemble_file infile outfile =
  Ccomp.command (Config.asm ^ " -o " ^
                 Filename.quote outfile ^ " " ^ Filename.quote infile)


let init () = ()
