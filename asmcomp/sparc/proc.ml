(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Description of the Sparc processor *)

open Misc
open Cmm
open Reg
open Arch
open Mach

(* Instruction selection *)

let word_addressed = false

(* Registers available for register allocation *)

(* Register map:
    %o0 - %o5   0 - 5       function results, C functions args / res
    %i0 - %i5   6 - 11      function arguments, preserved by C
    %l0 - %l4   12 - 16     general purpose, preserved by C
    %g3 - %g4   17 - 18     general purpose, not preserved by C

    %l5                     exception pointer
    %l6                     allocation pointer
    %l7                     address of allocation limit

    %g0                     always zero
    %g1 - %g2               temporaries
    %g5 - %g7               reserved for system libraries

    %f0 - %f10  100 - 105   function arguments and results
    %f12 - %f28 106 - 114   general purpose
    %f30                    temporary *)

let int_reg_name = [|
  (* 0-5 *)   "%o0"; "%o1"; "%o2"; "%o3"; "%o4"; "%o5";
  (* 6-11 *)  "%i0"; "%i1"; "%i2"; "%i3"; "%i4"; "%i5";
  (* 12-16 *) "%l0"; "%l1"; "%l2"; "%l3"; "%l4";
  (* 17-18 *) "%g3"; "%g4"
|]

let float_reg_name = [|
  (* 100-105 *) "%f0"; "%f2"; "%f4"; "%f6"; "%f8"; "%f10";
  (* 106-109 *) "%f12"; "%f14"; "%f16"; "%f18";
  (* 110-114 *) "%f20"; "%f22"; "%f24"; "%f26"; "%f28";
  (* 115 *)     "%f30";
  (* Odd parts of register pairs *)
  (* 116-121 *) "%f1"; "%f3"; "%f5"; "%f7"; "%f9"; "%f11";
  (* 122-125 *) "%f13"; "%f15"; "%f17"; "%f19";
  (* 126-130 *) "%f21"; "%f23"; "%f25"; "%f27"; "%f29";
  (* 131 *)     "%f31"
|]

let num_register_classes = 2

let register_class r =
  match r.typ with
    Int -> 0
  | Addr -> 0
  | Float -> 1

let num_available_registers = [| 19; 15 |]

let first_available_register = [| 0; 100 |]

let register_name r =
  if r < 100 then int_reg_name.(r) else float_reg_name.(r - 100)

let rotate_registers = true

(* Representation of hard registers by pseudo-registers *)

let hard_int_reg =
  let v = Array.make 19 Reg.dummy in
  for i = 0 to 18 do v.(i) <- Reg.at_location Int (Reg i) done;
  v

let hard_float_reg =
  let v = Array.make 32 Reg.dummy in
  for i = 0 to 31 do v.(i) <- Reg.at_location Float (Reg(100 + i)) done;
  v

let all_phys_regs =
  Array.append hard_int_reg (Array.sub hard_float_reg 0 15)
  (* No need to include the odd parts of float register pairs,
     nor the temporary register %f30 *)

let phys_reg n =
  if n < 100 then hard_int_reg.(n) else hard_float_reg.(n - 100)

let stack_slot slot ty =
  Reg.at_location ty (Stack slot)

(* Calling conventions *)

let calling_conventions first_int last_int first_float last_float make_stack
                        arg =
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
        if !float <= last_float then begin
          loc.(i) <- phys_reg !float;
          incr float
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) Float;
          ofs := !ofs + size_float
        end
  done;
  (loc, Misc.align !ofs 8)         (* Keep stack 8-aligned *)

let incoming ofs = Incoming ofs
let outgoing ofs = Outgoing ofs
let not_supported ofs = fatal_error "Proc.loc_results: cannot call"

let loc_arguments arg =
  calling_conventions 6 15 100 105 outgoing arg
let loc_parameters arg =
  let (loc, ofs) = calling_conventions 6 15 100 105 incoming arg in loc
let loc_results res =
  let (loc, ofs) = calling_conventions 0 5 100 105 not_supported res in loc

(* On the Sparc, all arguments to C functions, even floating-point arguments,
   are passed in %o0..%o5, then on the stack *)

let loc_external_arguments arg =
  let loc = ref [] in
  let reg = ref 0 (* %o0 *) in
  let ofs = ref (-4) in              (* start at sp + 92 = sp + 96 - 4 *)
  for i = 0 to Array.length arg - 1 do
    if !reg <= 5 (* %o5 *) then begin
      match arg.(i).typ with
        Int | Addr ->
          loc := phys_reg !reg :: !loc;
          incr reg
      | Float ->
          if !reg = 5 then fatal_error "Proc_sparc: cannot call";
          loc := phys_reg (!reg + 1) :: phys_reg !reg :: !loc;
          reg := !reg + 2
    end else begin
      loc := stack_slot (outgoing !ofs) arg.(i).typ :: !loc;
      ofs := !ofs + size_component arg.(i).typ
    end
  done;
  (* Keep stack 8-aligned *)
  (Array.of_list(List.rev !loc), Misc.align (!ofs + 4) 8)

let loc_external_results res =
  let (loc, ofs) = calling_conventions 0 0 100 100 not_supported res in loc

let loc_exn_bucket = phys_reg 0         (* $o0 *)

(* Volatile registers: none *)

let regs_are_volatile rs = false

(* Registers destroyed by operations *)

let destroyed_at_c_call = (* %l0-%l4, %i0-%i5 preserved *)
  Array.of_list(List.map phys_reg
    [0; 1; 2; 3; 4; 5; 17; 18;
     100; 101; 102; 103; 104; 105; 106; 107;
     108; 109; 110; 111; 112; 113; 114])

let destroyed_at_oper = function
    Iop(Icall_ind | Icall_imm _ | Iextcall(_, true)) -> all_phys_regs
  | Iop(Iextcall(_, false)) -> destroyed_at_c_call
  | _ -> [||]

let destroyed_at_raise = all_phys_regs

(* Maximal register pressure *)

let safe_register_pressure = function
    Iextcall(_, _) -> 0
  | _ -> 15

let max_register_pressure = function
    Iextcall(_, _) -> [| 11; 0 |]
  | _ -> [| 19; 15 |]

(* Pure operations (without any side effect besides updating their result
   registers). *)

let op_is_pure = function
  | Icall_ind | Icall_imm _ | Itailcall_ind | Itailcall_imm _
  | Iextcall _ | Istackoffset _ | Istore _ | Ialloc _
  | Iintop(Icheckbound) | Iintop_imm(Icheckbound, _) -> false
  | _ -> true

(* Layout of the stack *)

let num_stack_slots = [| 0; 0 |]
let contains_calls = ref false

(* Calling the assembler and the archiver *)

let assemble_file infile outfile =
  let asflags = begin match !arch_version with
    SPARC_V7 -> " -o "
  | SPARC_V8 -> " -xarch=v8 -o "
  | SPARC_V9 -> " -xarch=v8plus -o "
  end in
  Ccomp.command (Config.asm ^ asflags ^
                 Filename.quote outfile ^ " " ^ Filename.quote infile)

let init () = ()
