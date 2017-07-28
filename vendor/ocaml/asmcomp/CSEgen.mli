(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Common subexpression elimination by value numbering over extended
   basic blocks. *)

type op_class =
  | Op_pure     (* pure, produce one result *)
  | Op_checkbound     (* checkbound-style: no result, can raise an exn *)
  | Op_load           (* memory load *)
  | Op_store of bool  (* memory store, false = init, true = assign *)
  | Op_other   (* anything else that does not allocate nor store in memory *)

class cse_generic : object
  (* The following methods can be overriden to handle processor-specific
     operations. *)

  method class_of_operation: Mach.operation -> op_class

  method is_cheap_operation: Mach.operation -> bool
    (* Operations that are so cheap that it isn't worth factoring them. *)

  (* The following method is the entry point and should not be overridden *)
  method fundecl: Mach.fundecl -> Mach.fundecl

end
