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

(* Modules used by the program. *)
val modules : string list ref

(* Absolute directories containing source code on machine where source was
 * compiled *)
val program_source_dirs : string list ref

(* Read debugging info from executable file *)
val read_symbols : string -> unit

(* Flip "event" bit on all instructions *)
val set_all_events : unit -> unit

(* Return event at given PC, or raise Not_found *)
(* Can also return pseudo-event at beginning of functions *)
val any_event_at_pc : int -> Instruct.debug_event

(* Return event at given PC, or raise Not_found *)
val event_at_pc : int -> Instruct.debug_event
(* Set event at given PC *)
val set_event_at_pc : int -> unit

(* List the events in `module'. *)
val events_in_module : string -> Instruct.debug_event list

(* First event after the given position. *)
(* --- Raise `Not_found' if no such event. *)
val event_at_pos : string -> int -> Instruct.debug_event

(* Closest event from given position. *)
(* --- Raise `Not_found' if no such event. *)
val event_near_pos : string -> int -> Instruct.debug_event

(* Recompute the current event *)
val update_current_event : unit -> unit
