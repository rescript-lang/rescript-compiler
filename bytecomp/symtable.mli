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

(* Assign locations and numbers to globals and primitives *)

open Cmo_format

(* Functions for batch linking *)

val init: unit -> unit
val patch_object: bytes -> (reloc_info * int) list -> unit
val ls_patch_object: Misc.LongString.t -> (reloc_info * int) list -> unit
val require_primitive: string -> unit
val initial_global_table: unit -> Obj.t array
val output_global_map: out_channel -> unit
val output_primitive_names: out_channel -> unit
val output_primitive_table: out_channel -> unit
val data_global_map: unit -> Obj.t
val data_primitive_names: unit -> string

(* Functions for the toplevel *)

val init_toplevel: unit -> (string * Digest.t option) list
val update_global_table: unit -> unit
val get_global_value: Ident.t -> Obj.t
val is_global_defined: Ident.t -> bool
val assign_global_value: Ident.t -> Obj.t -> unit
val get_global_position: Ident.t -> int
val check_global_initialized: (reloc_info * int) list -> unit

type global_map

val current_state: unit -> global_map
val restore_state: global_map -> unit
val hide_additions: global_map -> unit
val filter_global_map: (Ident.t -> bool) -> global_map -> global_map

(* Error report *)

type error =
    Undefined_global of string
  | Unavailable_primitive of string
  | Wrong_vm of string
  | Uninitialized_global of string

exception Error of error

open Format

val report_error: formatter -> error -> unit

val reset: unit -> unit
