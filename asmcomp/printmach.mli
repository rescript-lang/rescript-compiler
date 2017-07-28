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

(* Pretty-printing of pseudo machine code *)

open Format

val reg: formatter -> Reg.t -> unit
val regs: formatter -> Reg.t array -> unit
val regset: formatter -> Reg.Set.t -> unit
val regsetaddr: formatter -> Reg.Set.t -> unit
val operation: Mach.operation -> Reg.t array -> formatter -> Reg.t array -> unit
val test: Mach.test -> formatter -> Reg.t array -> unit
val instr: formatter -> Mach.instruction -> unit
val fundecl: formatter -> Mach.fundecl -> unit
val phase: string -> formatter -> Mach.fundecl -> unit
val interferences: formatter -> unit -> unit
val preferences: formatter -> unit -> unit

val print_live: bool ref
