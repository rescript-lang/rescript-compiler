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

(*** Debugging. ***)

val debug_loading : bool ref

(*** Load program ***)

(* Function used for launching the program. *)
val launching_func : (unit -> unit) ref

val load_program : unit -> unit

type launching_function = (unit -> unit)

val loading_modes : (string * launching_function) list
val set_launching_function : launching_function -> unit

(** Connection **)
val connection : Primitives.io_channel ref
val connection_opened : bool ref
