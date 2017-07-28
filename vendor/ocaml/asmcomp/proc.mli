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

(* Processor descriptions *)

(* Instruction selection *)
val word_addressed: bool

(* Registers available for register allocation *)
val num_register_classes: int
val register_class: Reg.t -> int
val num_available_registers: int array
val first_available_register: int array
val register_name: int -> string
val phys_reg: int -> Reg.t
val rotate_registers: bool

(* Calling conventions *)
val loc_arguments: Reg.t array -> Reg.t array * int
val loc_results: Reg.t array -> Reg.t array
val loc_parameters: Reg.t array -> Reg.t array
val loc_external_arguments: Reg.t array -> Reg.t array * int
val loc_external_results: Reg.t array -> Reg.t array
val loc_exn_bucket: Reg.t

(* Maximal register pressures for pre-spilling *)
val safe_register_pressure: Mach.operation -> int
val max_register_pressure: Mach.operation -> int array

(* Registers destroyed by operations *)
val destroyed_at_oper: Mach.instruction_desc -> Reg.t array
val destroyed_at_raise: Reg.t array

(* Volatile registers: those that change value when read *)
val regs_are_volatile: Reg.t array -> bool

(* Pure operations *)
val op_is_pure: Mach.operation -> bool

(* Info for laying out the stack frame *)
val num_stack_slots: int array
val contains_calls: bool ref

(* Calling the assembler *)
val assemble_file: string -> string -> int

(* Called before translating a fundecl. *)
val init : unit -> unit
