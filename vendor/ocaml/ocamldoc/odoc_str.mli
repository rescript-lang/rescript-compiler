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

(** The functions to get a string from different kinds of elements (types, modules, ...). *)

(** @return the variance string for the given type and (covariant, contravariant) information. *)
val string_of_variance : Odoc_type.t_type -> (bool * bool) -> string

(** This function returns a string to represent the given list of types,
   with a given separator.
   @param par can be used to force the addition or not of parentheses around the returned string.
*)
val string_of_type_list : ?par: bool -> string -> Types.type_expr list -> string

(** This function returns a string to represent the list of type parameters
   for the given type. *)
val string_of_type_param_list : Odoc_type.t_type -> string

(** This function returns a string to represent the list of type parameters
   for the given type extension. *)
val string_of_type_extension_param_list : Odoc_extension.t_type_extension -> string

(** This function returns a string to represent the given list of
   type parameters of a class or class type,
   with a given separator. *)
val string_of_class_type_param_list : Types.type_expr list -> string

(** @return a string to describe the given type. *)
val string_of_type : Odoc_type.t_type -> string

(** @return a string to display the parameters of the given class,
   in the same form as the compiler. *)
val string_of_class_params : Odoc_class.t_class -> string

(** @return a string to describe the given type extension. *)
val string_of_type_extension : Odoc_extension.t_type_extension -> string

(** @return a string to describe the given exception. *)
val string_of_exception : Odoc_exception.t_exception -> string

(** @return a string to describe the given value. *)
val string_of_value : Odoc_value.t_value -> string

(** @return a string to describe the given attribute. *)
val string_of_attribute : Odoc_value.t_attribute -> string

(** @return a string to describe the given method. *)
val string_of_method : Odoc_value.t_method -> string
