(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                        Alain Frisch, LexiFi                         *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Asttypes
open Typedtree

class iter: object
  method binding: value_binding -> unit
  method bindings: (rec_flag * value_binding list) -> unit
  method case: case -> unit
  method cases: case list -> unit
  method class_description: class_description -> unit
  method class_expr: class_expr -> unit
  method class_field: class_field -> unit
  method class_signature: class_signature -> unit
  method class_structure: class_structure -> unit
  method class_type: class_type -> unit
  method class_type_declaration: class_type_declaration -> unit
  method class_type_field: class_type_field -> unit
  method core_type: core_type -> unit
  method expression: expression -> unit
  method extension_constructor: extension_constructor -> unit
  method module_binding: module_binding -> unit
  method module_expr: module_expr -> unit
  method module_type: module_type -> unit
  method package_type: package_type -> unit
  method pattern: pattern -> unit
  method row_field: row_field -> unit
  method signature: signature -> unit
  method signature_item: signature_item -> unit
  method structure: structure -> unit
  method structure_item: structure_item -> unit
  method type_declaration: type_declaration -> unit
  method type_extension: type_extension -> unit
  method value_description: value_description -> unit
  method with_constraint: with_constraint -> unit
end
(** Recursive iterator class. By inheriting from it and
    overriding selected methods, it is possible to implement
    custom behavior for specific kinds of nodes. *)

(** {2 One-level iterators} *)

(** The following functions apply the provided iterator to each
    sub-component of the argument. *)

val binding: iter -> value_binding -> unit
val bindings: iter -> (rec_flag * value_binding list) -> unit
val class_description: iter -> class_description -> unit
val class_expr: iter -> class_expr -> unit
val class_field: iter -> class_field -> unit
val class_signature: iter -> class_signature -> unit
val class_structure: iter -> class_structure -> unit
val class_type: iter -> class_type -> unit
val class_type_declaration: iter -> class_type_declaration -> unit
val class_type_field: iter -> class_type_field -> unit
val core_type: iter -> core_type -> unit
val expression: iter -> expression -> unit
val extension_constructor: iter -> extension_constructor -> unit
val module_binding: iter -> module_binding -> unit
val module_expr: iter -> module_expr -> unit
val module_type: iter -> module_type -> unit
val package_type: iter -> package_type -> unit
val pattern: iter -> pattern -> unit
val row_field: iter -> row_field -> unit
val signature: iter -> signature -> unit
val signature_item: iter -> signature_item -> unit
val structure: iter -> structure -> unit
val structure_item: iter -> structure_item -> unit
val type_declaration: iter -> type_declaration -> unit
val type_extension: iter -> type_extension -> unit
val value_description: iter -> value_description -> unit
val with_constraint: iter -> with_constraint -> unit
