(***********************************************************************)
(*                                                                     *)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Environment for finding complete names from relative names. *)

(** An environment of known names,
   from simple name to complete name. *)
type env

(** The empty environment. *)
val empty : env

(** Extending an environment *)

val add_signature : env -> string -> ?rel:string -> Types.signature -> env
val add_extension : env -> Odoc_name.t -> env
val add_type : env -> Odoc_name.t -> env
val add_value : env -> Odoc_name.t -> env
val add_module : env -> Odoc_name.t -> env
val add_module_type : env -> Odoc_name.t -> env
val add_class : env -> Odoc_name.t -> env
val add_class_type : env -> Odoc_name.t -> env

(** Retrieving fully qualified names from an environment *)

(** Get the fully qualified module name from a name.*)
val full_module_name : env -> Odoc_name.t -> Odoc_name.t

(** Get the fully qualified module type name from a name.*)
val full_module_type_name : env -> Odoc_name.t -> Odoc_name.t

(** Get the fully qualified module or module type name from a name.
   We look for a module type if we don't find a module.*)
val full_module_or_module_type_name : env -> Odoc_name.t -> Odoc_name.t

(** Get the fully qualified type name from a name.*)
val full_type_name : env -> Odoc_name.t -> Odoc_name.t

(** Get the fully qualified value name from a name.*)
val full_value_name : env -> Odoc_name.t -> Odoc_name.t

(** Get the fully qualified extension name from a name.*)
val full_extension_constructor_name : env -> Odoc_name.t -> Odoc_name.t

(** Get the fully qualified class name from a name.*)
val full_class_name : env -> Odoc_name.t -> Odoc_name.t

(** Get the fully qualified class type name from a name.*)
val full_class_type_name : env -> Odoc_name.t -> Odoc_name.t

(** Get the fully qualified class or class type name from a name.*)
val full_class_or_class_type_name : env -> Odoc_name.t -> Odoc_name.t

(** Substitutions *)

(** Replace the [Path.t] by a complete [Path.t] in a [Types.type_expr].*)
val subst_type : env -> Types.type_expr -> Types.type_expr

(** Replace the [Path.t] by a complete [Path.t] in a [Types.module_type].*)
val subst_module_type : env -> Types.module_type -> Types.module_type

(** Replace the [Path.t] by a complete [Path.t] in a [Types.class_type].
   Also empty the structures to get only [object end] when the type
   is printed.
*)
val subst_class_type : env -> Types.class_type -> Types.class_type
