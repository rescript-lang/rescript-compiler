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

(* Substitutions *)

open Types

type t

(*
   Substitutions are used to translate a type from one context to
   another.  This requires substituing paths for identifiers, and
   possibly also lowering the level of non-generic variables so that
   it be inferior to the maximum level of the new context.

   Substitutions can also be used to create a "clean" copy of a type.
   Indeed, non-variable node of a type are duplicated, with their
   levels set to generic level.  That way, the resulting type is
   well-formed (decreasing levels), even if the original one was not.
*)

val identity: t

val add_type: Ident.t -> Path.t -> t -> t
val add_module: Ident.t -> Path.t -> t -> t
val add_modtype: Ident.t -> module_type -> t -> t
val for_saving: t -> t
val reset_for_saving: unit -> unit

val module_path: t -> Path.t -> Path.t
val type_path: t -> Path.t -> Path.t

val type_expr: t -> type_expr -> type_expr
val class_type: t -> class_type -> class_type
val value_description: t -> value_description -> value_description
val type_declaration: t -> type_declaration -> type_declaration
val extension_constructor:
        t -> extension_constructor -> extension_constructor
val class_declaration: t -> class_declaration -> class_declaration
val cltype_declaration: t -> class_type_declaration -> class_type_declaration
val modtype: t -> module_type -> module_type
val signature: t -> signature -> signature
val modtype_declaration: t -> modtype_declaration -> modtype_declaration
val module_declaration: t -> module_declaration -> module_declaration
val typexp : t -> Types.type_expr -> Types.type_expr
val class_signature: t -> class_signature -> class_signature

(* Composition of substitutions:
     apply (compose s1 s2) x = apply s2 (apply s1 x) *)
val compose: t -> t -> t
