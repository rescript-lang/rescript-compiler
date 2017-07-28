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

(* Type-checking of the module language *)

open Types
open Format

val type_module:
        Env.t -> Parsetree.module_expr -> Typedtree.module_expr
val type_structure:
        Env.t -> Parsetree.structure -> Location.t ->
         Typedtree.structure * Types.signature * Env.t
val type_toplevel_phrase:
        Env.t -> Parsetree.structure ->
         Typedtree.structure * Types.signature * Env.t
val type_implementation_more:
  string -> string -> string -> Env.t -> Parsetree.structure ->
  Typedtree.structure * Typedtree.module_coercion * Env.t * Types.signature

val type_implementation:
  string -> string -> string -> Env.t -> Parsetree.structure ->
  Typedtree.structure * Typedtree.module_coercion

val type_interface:
        Env.t -> Parsetree.signature -> Typedtree.signature
val transl_signature:
        Env.t -> Parsetree.signature -> Typedtree.signature
val check_nongen_schemes:
        Env.t -> Typedtree.structure_item list -> unit
val type_open_:
        ?toplevel:bool -> Asttypes.override_flag ->
        Env.t -> Location.t -> Longident.t Asttypes.loc -> Path.t * Env.t
val modtype_of_package:
        Env.t -> Location.t ->
        Path.t -> Longident.t list -> type_expr list -> module_type
val simplify_signature: signature -> signature

val path_of_module : Typedtree.module_expr -> Path.t option

val save_signature:
  string -> Typedtree.signature -> string -> string ->
  Env.t -> Types.signature_item list -> unit

val package_units:
  Env.t -> string list -> string -> string -> Typedtree.module_coercion

type error =
    Cannot_apply of module_type
  | Not_included of Includemod.error list
  | Cannot_eliminate_dependency of module_type
  | Signature_expected
  | Structure_expected of module_type
  | With_no_component of Longident.t
  | With_mismatch of Longident.t * Includemod.error list
  | Repeated_name of string * string
  | Non_generalizable of type_expr
  | Non_generalizable_class of Ident.t * class_declaration
  | Non_generalizable_module of module_type
  | Implementation_is_required of string
  | Interface_not_compiled of string
  | Not_allowed_in_functor_body
  | With_need_typeconstr
  | Not_a_packed_module of type_expr
  | Incomplete_packed_module of type_expr
  | Scoping_pack of Longident.t * type_expr
  | Recursive_module_require_explicit_type
  | Apply_generative

exception Error of Location.t * Env.t * error
exception Error_forward of Location.error

val report_error: Env.t -> formatter -> error -> unit
