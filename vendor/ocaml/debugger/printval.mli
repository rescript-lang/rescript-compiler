(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          OCaml port by John Malecki and Xavier Leroy                *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Format

val max_printer_depth : int ref
val max_printer_steps : int ref

val print_exception: formatter -> Debugcom.Remote_value.t -> unit
val print_named_value :
  int -> Parser_aux.expression -> Env.t ->
    Debugcom.Remote_value.t -> formatter -> Types.type_expr ->
    unit

val reset_named_values : unit -> unit
val find_named_value : int -> Debugcom.Remote_value.t * Types.type_expr

val install_printer :
  Path.t -> Types.type_expr -> formatter ->
    (formatter -> Obj.t -> unit) -> unit
val remove_printer : Path.t -> unit
