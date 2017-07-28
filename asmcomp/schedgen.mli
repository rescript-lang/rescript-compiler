(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Instruction scheduling *)

type code_dag_node =
  { instr: Linearize.instruction;
    delay: int;
    mutable sons: (code_dag_node * int) list;
    mutable date: int;
    mutable length: int;
    mutable ancestors: int;
    mutable emitted_ancestors: int }

class virtual scheduler_generic : object
  (* Can be overridden by processor description *)
  method virtual oper_issue_cycles : Mach.operation -> int
      (* Number of cycles needed to issue the given operation *)
  method virtual oper_latency : Mach.operation -> int
      (* Number of cycles needed to complete the given operation *)
  method reload_retaddr_issue_cycles : int
      (* Number of cycles needed to issue a Lreloadretaddr operation *)
  method reload_retaddr_latency : int
      (* Number of cycles needed to complete a Lreloadretaddr operation *)
  method oper_in_basic_block : Mach.operation -> bool
      (* Says whether the given operation terminates a basic block *)
  method is_store : Mach.operation -> bool
      (* Says whether the given operation is a memory store *)
  method is_load : Mach.operation -> bool
      (* Says whether the given operation is a memory load *)
  method is_checkbound : Mach.operation -> bool
      (* Says whether the given operation is a checkbound *)
  (* Entry point *)
  method schedule_fundecl : Linearize.fundecl -> Linearize.fundecl
end

val reset : unit -> unit
