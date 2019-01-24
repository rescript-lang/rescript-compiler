(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Description of the Power PC *)

open Misc
open Cmm
open Reg
open Arch
open Mach

(* Instruction selection *)

let word_addressed = false

(* Registers available for register allocation *)

(* Integer register map:
    0                   temporary, null register for some operations
    1                   stack pointer
    2                   pointer to table of contents
    3 - 10              function arguments and results
    11 - 12             temporaries
    13                  pointer to small data area
    14 - 28             general purpose, preserved by C
    29                  trap pointer
    30                  allocation limit
    31                  allocation pointer
  Floating-point register map:
    0                   temporary
    1 - 13              function arguments and results
    14 - 31             general purpose, preserved by C
*)

let int_reg_name =
  [| "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10";
     "14"; "15"; "16"; "17"; "18"; "19"; "20"; "21";
     "22"; "23"; "24"; "25"; "26"; "27"; "28" |]

let float_reg_name =
  [| "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8";
     "9"; "10"; "11"; "12"; "13"; "14"; "15"; "16";
     "17"; "18"; "19"; "20"; "21"; "22"; "23"; "24";
     "25"; "26"; "27"; "28"; "29"; "30"; "31" |]

let num_register_classes = 2

let register_class r =
  match r.typ with
  | Val | Int | Addr -> 0
  | Float -> 1

let num_available_registers = [| 23; 31 |]

let first_available_register = [| 0; 100 |]

let register_name r =
  if r < 100 then int_reg_name.(r) else float_reg_name.(r - 100)

let rotate_registers = true

(* Representation of hard registers by pseudo-registers *)

let hard_int_reg =
  let v = Array.make 23 Reg.dummy in
  for i = 0 to 22 do v.(i) <- Reg.at_location Int (Reg i) done; v

let hard_float_reg =
  let v = Array.make 31 Reg.dummy in
  for i = 0 to 30 do v.(i) <- Reg.at_location Float (Reg(100 + i)) done; v

let all_phys_regs =
  Array.append hard_int_reg hard_float_reg

let phys_reg n =
  if n < 100 then hard_int_reg.(n) else hard_float_reg.(n - 100)

let stack_slot slot ty =
  Reg.at_location ty (Stack slot)

let loc_spacetime_node_hole = Reg.dummy  (* Spacetime unsupported *)

(* Calling conventions *)

let calling_conventions
    first_int last_int first_float last_float
    make_stack stack_ofs reg_use_stack arg =
  let loc = Array.make (Array.length arg) [| Reg.dummy |] in
  let int = ref first_int in
  let float = ref first_float in
  let ofs = ref stack_ofs in
  for i = 0 to Array.length arg - 1 do
    match arg.(i) with
    | [| arg |] ->
      begin match arg.typ with
      | Val | Int | Addr as ty ->
          if !int <= last_int then begin
            loc.(i) <- [| phys_reg !int |];
            incr int;
            if reg_use_stack then ofs := !ofs + size_int
          end else begin
            loc.(i) <- [| stack_slot (make_stack !ofs) ty |];
            ofs := !ofs + size_int
          end
      | Float ->
          if !float <= last_float then begin
            loc.(i) <- [| phys_reg !float |];
            incr float;
            (* On 64-bit platforms, passing a float in a float register
               reserves a normal register as well *)
            if size_int = 8 then incr int;
            if reg_use_stack then ofs := !ofs + size_float
          end else begin
            ofs := Misc.align !ofs size_float;
            loc.(i) <- [| stack_slot (make_stack !ofs) Float |];
            ofs := !ofs + size_float
          end
      end
    | [| arg1; arg2 |] ->
      (* Passing of 64-bit quantities to external functions
         on 32-bit platform. *)
      assert (size_int = 4);
      begin match arg1.typ, arg2.typ with
      | Int, Int ->
          (* 64-bit quantities split across two registers must either be in a
             consecutive pair of registers where the lowest numbered is an
             even-numbered register; or in a stack slot that is 8-byte
             aligned. *)
          int := Misc.align !int 2;
          if !int <= last_int - 1 then begin
            let reg_lower = phys_reg !int in
            let reg_upper = phys_reg (!int + 1) in
            loc.(i) <- [| reg_lower; reg_upper |];
            int := !int + 2
          end else begin
            let size_int64 = 8 in
            ofs := Misc.align !ofs size_int64;
            let ofs_lower = !ofs in
            let ofs_upper = !ofs + size_int in
            let stack_lower = stack_slot (make_stack ofs_lower) Int in
            let stack_upper = stack_slot (make_stack ofs_upper) Int in
            loc.(i) <- [| stack_lower; stack_upper |];
            ofs := !ofs + size_int64
          end
      | _, _ ->
        let f = function Int -> "I" | Addr -> "A" | Val -> "V" | Float -> "F" in
        fatal_error (Printf.sprintf "Proc.calling_conventions: bad register \
            type(s) for multi-register argument: %s, %s"
          (f arg1.typ) (f arg2.typ))
      end
    | _ ->
      fatal_error "Proc.calling_conventions: bad number of registers for \
                   multi-register argument"
  done;
  (loc, Misc.align !ofs 16)
  (* Keep stack 16-aligned. *)

let incoming ofs = Incoming ofs
let outgoing ofs = Outgoing ofs
let not_supported _ofs = fatal_error "Proc.loc_results: cannot call"

let single_regs arg = Array.map (fun arg -> [| arg |]) arg
let ensure_single_regs res =
  Array.map (function
      | [| res |] -> res
      | _ -> failwith "Proc.ensure_single_regs")
    res

let max_arguments_for_tailcalls = 8

let loc_arguments arg =
  let (loc, ofs) =
    calling_conventions 0 7 100 112 outgoing 0 false (single_regs arg)
  in
  (ensure_single_regs loc, ofs)
let loc_parameters arg =
  let (loc, _ofs) =
    calling_conventions 0 7 100 112 incoming 0 false (single_regs arg)
  in
  ensure_single_regs loc
let loc_results res =
  let (loc, _ofs) =
    calling_conventions 0 7 100 112 not_supported 0 false (single_regs res)
  in
  ensure_single_regs loc

(* C calling conventions for ELF32:
     use GPR 3-10 and FPR 1-8 just like ML calling conventions.
     Using a float register does not affect the int registers.
     Always reserve 8 bytes at bottom of stack, plus whatever is needed
     to hold the overflow arguments.
   C calling conventions for ELF64v1:
     Use GPR 3-10 for the first integer arguments.
     Use FPR 1-13 for the first float arguments.
     Always reserve stack space for all arguments, even when passed in
     registers.
     Always reserve at least 8 words (64 bytes) for the arguments.
     Always reserve 48 bytes at bottom of stack, plus whatever is needed
     to hold the arguments.
     The reserved 48 bytes are automatically added in emit.mlp
     and need not appear here.
   C calling conventions for ELF64v2:
     Use GPR 3-10 for the first integer arguments.
     Use FPR 1-13 for the first float arguments.
     If all arguments fit in registers, don't reserve stack space.
     Otherwise, reserve stack space for all arguments.
     Always reserve 32 bytes at bottom of stack, plus whatever is needed
     to hold the arguments.
     The reserved 32 bytes are automatically added in emit.mlp
     and need not appear here.
*)

let loc_external_arguments =
  match abi with
  | ELF32 ->
      calling_conventions 0 7 100 107 outgoing 8 false
  | ELF64v1 ->
      fun args ->
      let (loc, ofs) =
        calling_conventions 0 7 100 112 outgoing 0 true args in
      (loc, max ofs 64)
  | ELF64v2 ->
      fun args ->
      let (loc, ofs) =
        calling_conventions 0 7 100 112 outgoing 0 true args in
      if Array.fold_left
           (fun stk r ->
              assert (Array.length r = 1);
              match r.(0).loc with
              | Stack _ -> true
              | _ -> stk)
           false loc
      then (loc, ofs)
      else (loc, 0)

(* Results are in GPR 3 and FPR 1 *)

let loc_external_results res =
  let (loc, _ofs) =
    calling_conventions 0 1 100 100 not_supported 0 false (single_regs res)
  in
  ensure_single_regs loc

(* Exceptions are in GPR 3 *)

let loc_exn_bucket = phys_reg 0

(* Volatile registers: none *)

let regs_are_volatile _rs = false

(* Registers destroyed by operations *)

let destroyed_at_c_call =
  Array.of_list(List.map phys_reg
    [0; 1; 2; 3; 4; 5; 6; 7;
     100; 101; 102; 103; 104; 105; 106; 107; 108; 109; 110; 111; 112])

let destroyed_at_oper = function
    Iop(Icall_ind _ | Icall_imm _ | Iextcall { alloc = true; _ }) ->
    all_phys_regs
  | Iop(Iextcall { alloc = false; _ }) -> destroyed_at_c_call
  | _ -> [||]

let destroyed_at_raise = all_phys_regs

(* Maximal register pressure *)

let safe_register_pressure = function
    Iextcall _ -> 15
  | _ -> 23

let max_register_pressure = function
    Iextcall _ -> [| 15; 18 |]
  | _ -> [| 23; 30 |]

(* Pure operations (without any side effect besides updating their result
   registers). *)

let op_is_pure = function
  | Icall_ind _ | Icall_imm _ | Itailcall_ind _ | Itailcall_imm _
  | Iextcall _ | Istackoffset _ | Istore _ | Ialloc _
  | Iintop(Icheckbound _) | Iintop_imm(Icheckbound _, _) -> false
  | Ispecific(Imultaddf | Imultsubf) -> true
  | Ispecific _ -> false
  | _ -> true

(* Layout of the stack *)

let num_stack_slots = [| 0; 0 |]
let contains_calls = ref false

(* Calling the assembler *)

let assemble_file infile outfile =
  Ccomp.command (Config.asm ^ " -o " ^
                 Filename.quote outfile ^ " " ^ Filename.quote infile)

let init () = ()
