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

open Instruct

val get_pos : debug_event -> Lexing.position;;

(** Current events. **)

(* The event at current position. *)
val current_event : debug_event option ref

(* Current position in source. *)
(* Raise `Not_found' if not on an event (beginning or end of program). *)
val get_current_event : unit -> debug_event

val current_event_is_before : unit -> bool
