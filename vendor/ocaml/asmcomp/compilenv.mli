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

(* Compilation environments for compilation units *)

open Cmx_format

val reset: ?packname:string -> string -> unit
        (* Reset the environment and record the name of the unit being
           compiled (arg).  Optional argument is [-for-pack] prefix. *)

val current_unit_infos: unit -> unit_infos
        (* Return the infos for the unit being compiled *)

val current_unit_name: unit -> string
        (* Return the name of the unit being compiled *)

val make_symbol: ?unitname:string -> string option -> string
        (* [make_symbol ~unitname:u None] returns the asm symbol that
           corresponds to the compilation unit [u] (default: the current unit).
           [make_symbol ~unitname:u (Some id)] returns the asm symbol that
           corresponds to symbol [id] in the compilation unit [u]
           (or the current unit). *)

val symbol_in_current_unit: string -> bool
        (* Return true if the given asm symbol belongs to the
           current compilation unit, false otherwise. *)

val symbol_for_global: Ident.t -> string
        (* Return the asm symbol that refers to the given global identifier *)

val global_approx: Ident.t -> Clambda.value_approximation
        (* Return the approximation for the given global identifier *)
val set_global_approx: Clambda.value_approximation -> unit
        (* Record the approximation of the unit being compiled *)
val record_global_approx_toplevel: unit -> unit
        (* Record the current approximation for the current toplevel phrase *)


val need_curry_fun: int -> unit
val need_apply_fun: int -> unit
val need_send_fun: int -> unit
        (* Record the need of a currying (resp. application,
           message sending) function with the given arity *)

val new_const_symbol : unit -> string
val new_const_label : unit -> int

val new_structured_constant:
  Clambda.ustructured_constant ->
  shared:bool -> (* can be shared with another structually equal constant *)
  string
val structured_constants:
  unit -> (string * bool * Clambda.ustructured_constant) list
val add_exported_constant: string -> unit

type structured_constants
val snapshot: unit -> structured_constants
val backtrack: structured_constants -> unit


val read_unit_info: string -> unit_infos * Digest.t
        (* Read infos and MD5 from a [.cmx] file. *)
val write_unit_info: unit_infos -> string -> unit
        (* Save the given infos in the given file *)
val save_unit_info: string -> unit
        (* Save the infos for the current unit in the given file *)
val cache_unit_info: unit_infos -> unit
        (* Enter the given infos in the cache.  The infos will be
           honored by [symbol_for_global] and [global_approx]
           without looking at the corresponding .cmx file. *)

val read_library_info: string -> library_infos

type error =
    Not_a_unit_info of string
  | Corrupted_unit_info of string
  | Illegal_renaming of string * string * string

exception Error of error

val report_error: Format.formatter -> error -> unit
