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

(* Printing functions *)

open Format
open Types
open Outcometree

val longident: formatter -> Longident.t -> unit
val ident: formatter -> Ident.t -> unit
val tree_of_path: Path.t -> out_ident
val path: formatter -> Path.t -> unit
val string_of_path: Path.t -> string
val raw_type_expr: formatter -> type_expr -> unit

val wrap_printing_env: Env.t -> (unit -> 'a) -> 'a
    (* Call the function using the environment for type path shortening *)
    (* This affects all the printing functions below *)

val reset: unit -> unit
val mark_loops: type_expr -> unit
val reset_and_mark_loops: type_expr -> unit
val reset_and_mark_loops_list: type_expr list -> unit
val type_expr: formatter -> type_expr -> unit
val tree_of_type_scheme: type_expr -> out_type
val type_sch : formatter -> type_expr -> unit
val type_scheme: formatter -> type_expr -> unit
(* Maxence *)
val reset_names: unit -> unit
val type_scheme_max: ?b_reset_names: bool ->
        formatter -> type_expr -> unit
(* Fin Maxence *)
val tree_of_value_description: Ident.t -> value_description -> out_sig_item
val value_description: Ident.t -> formatter -> value_description -> unit
val tree_of_type_declaration:
    Ident.t -> type_declaration -> rec_status -> out_sig_item
val type_declaration: Ident.t -> formatter -> type_declaration -> unit
val tree_of_extension_constructor:
    Ident.t -> extension_constructor -> ext_status -> out_sig_item
val extension_constructor:
    Ident.t -> formatter -> extension_constructor -> unit
val tree_of_module: Ident.t -> module_type -> rec_status -> out_sig_item
val modtype: formatter -> module_type -> unit
val signature: formatter -> signature -> unit
val tree_of_modtype_declaration:
    Ident.t -> modtype_declaration -> out_sig_item
val tree_of_signature: Types.signature -> out_sig_item list
val tree_of_typexp: bool -> type_expr -> out_type
val modtype_declaration: Ident.t -> formatter -> modtype_declaration -> unit
val class_type: formatter -> class_type -> unit
val tree_of_class_declaration:
    Ident.t -> class_declaration -> rec_status -> out_sig_item
val class_declaration: Ident.t -> formatter -> class_declaration -> unit
val tree_of_cltype_declaration:
    Ident.t -> class_type_declaration -> rec_status -> out_sig_item
val cltype_declaration: Ident.t -> formatter -> class_type_declaration -> unit
val type_expansion: type_expr -> Format.formatter -> type_expr -> unit
val prepare_expansion: type_expr * type_expr -> type_expr * type_expr
val trace:
    bool -> bool-> string -> formatter -> (type_expr * type_expr) list -> unit
val report_unification_error:
    formatter -> Env.t -> ?unif:bool -> (type_expr * type_expr) list ->
    (formatter -> unit) -> (formatter -> unit) ->
    unit

#if undefined BS_NO_COMPILER_PATCH then
val super_report_unification_error:
    formatter -> Env.t -> ?unif:bool -> (type_expr * type_expr) list ->
    (formatter -> unit) -> (formatter -> unit) ->
    unit
#end

val report_subtyping_error:
    formatter -> Env.t -> (type_expr * type_expr) list ->
    string -> (type_expr * type_expr) list -> unit
val report_ambiguous_type_error:
    formatter -> Env.t -> (Path.t * Path.t) -> (Path.t * Path.t) list ->
    (formatter -> unit) -> (formatter -> unit) -> (formatter -> unit) -> unit

(* for toploop *)
val hide_rec_items: signature_item list -> unit
