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

(** Research of elements through modules. *)

(** The type for an element of the result of a research. *)
type result_element =
    Res_module of Odoc_module.t_module
  | Res_module_type of Odoc_module.t_module_type
  | Res_class of Odoc_class.t_class
  | Res_class_type of Odoc_class.t_class_type
  | Res_value of Odoc_value.t_value
  | Res_type of Odoc_type.t_type
  | Res_extension of Odoc_extension.t_extension_constructor
  | Res_exception of Odoc_exception.t_exception
  | Res_attribute of Odoc_value.t_attribute
  | Res_method of Odoc_value.t_method
  | Res_section of string * Odoc_types.text
  | Res_recfield of  Odoc_type.t_type * Odoc_type.record_field
  | Res_const of  Odoc_type.t_type * Odoc_type.variant_constructor

(** The type representing a research result.*)
type result = result_element list

(** The type of modules which contain the predicates used during the research.
   Some functions return a couple of booleans ; the first indicates if we
   must go deeper in the analysed element, the second if the element satisfies
   the predicate.
*)
module type Predicates =
  sig
    type t
    val p_module : Odoc_module.t_module -> t -> bool * bool
    val p_module_type : Odoc_module.t_module_type -> t -> bool * bool
    val p_class : Odoc_class.t_class -> t -> bool * bool
    val p_class_type : Odoc_class.t_class_type -> t -> bool * bool
    val p_value : Odoc_value.t_value -> t -> bool
    val p_recfield : Odoc_type.t_type -> Odoc_type.record_field -> t -> bool
    val p_const : Odoc_type.t_type -> Odoc_type.variant_constructor -> t -> bool
    val p_type : Odoc_type.t_type -> t -> (bool * bool)
    val p_extension :
      Odoc_extension.t_extension_constructor -> t -> bool
    val p_exception : Odoc_exception.t_exception -> t -> bool
    val p_attribute : Odoc_value.t_attribute -> t -> bool
    val p_method : Odoc_value.t_method -> t -> bool
    val p_section : string -> t -> bool
  end

(** Search for elements verifying the predicates in the module in parameter.*)
module Search :
  functor (P : Predicates) ->
    sig
      (** search in a section title *)
      val search_section : Odoc_types.text -> string -> P.t -> result_element list

      (** search in a value *)
      val search_value : Odoc_value.t_value -> P.t -> result_element list

      (** search in a record field *)
      val search_recfield :
        Odoc_type.t_type -> Odoc_type.record_field -> P.t -> result_element list

      (** search in a variant constructor *)
      val search_const :
        Odoc_type.t_type -> Odoc_type.variant_constructor -> P.t -> result_element list

      (** search in a type *)
      val search_type : Odoc_type.t_type -> P.t -> result_element list

      (** search in an extension constructor *)
      val search_extension_constructor :
          Odoc_extension.t_extension_constructor -> P.t -> result_element list

      (** search in a type extension *)
      val search_type_extension :
          Odoc_extension.t_type_extension -> P.t -> result_element list

      (** search in an exception *)
      val search_exception :
          Odoc_exception.t_exception -> P.t -> result_element list

      (** search in an attribute *)
      val search_attribute :
          Odoc_value.t_attribute -> P.t -> result_element list

      (** search in a method *)
      val search_method : Odoc_value.t_method -> P.t -> result_element list

      (** search in a class *)
      val search_class : Odoc_class.t_class -> P.t -> result_element list

      (** search in a class type *)
      val search_class_type :
          Odoc_class.t_class_type -> P.t -> result_element list

      (** search in a module type *)
      val search_module_type :
          Odoc_module.t_module_type -> P.t -> result_element list

      (** search in a module *)
      val search_module : Odoc_module.t_module -> P.t -> result_element list

      (** search in a list of modules *)
      val search : Odoc_module.t_module list -> P.t -> result_element list
    end

(** A module of predicates to search elements by name (and accepting regexps).*)
module P_name :
  sig
    type t = Str.regexp
    val ( =~ ) : string -> Str.regexp -> bool
    val p_module : Odoc_module.t_module -> Str.regexp -> bool * bool
    val p_module_type :
      Odoc_module.t_module_type -> Str.regexp -> bool * bool
    val p_class : Odoc_class.t_class -> Str.regexp -> bool * bool
    val p_class_type : Odoc_class.t_class_type -> Str.regexp -> bool * bool
    val p_value : Odoc_value.t_value -> Str.regexp -> bool
    val p_recfield : Odoc_type.t_type -> Odoc_type.record_field -> Str.regexp -> bool
    val p_const : Odoc_type.t_type -> Odoc_type.variant_constructor -> Str.regexp -> bool
    val p_type : Odoc_type.t_type -> Str.regexp -> (bool * bool)
    val p_extension :
      Odoc_extension.t_extension_constructor -> Str.regexp -> bool
    val p_exception : Odoc_exception.t_exception -> Str.regexp -> bool
    val p_attribute : Odoc_value.t_attribute -> Str.regexp -> bool
    val p_method : Odoc_value.t_method -> Str.regexp -> bool
  end

(** A module to search elements by name. *)
module Search_by_name :
  sig
    val search_section : Odoc_types.text -> string -> P_name.t -> result_element list
    val search_value : Odoc_value.t_value -> P_name.t -> result_element list
    val search_recfield : Odoc_type.t_type -> Odoc_type.record_field -> P_name.t -> result_element list
    val search_const : Odoc_type.t_type -> Odoc_type.variant_constructor -> P_name.t -> result_element list
    val search_type : Odoc_type.t_type -> P_name.t -> result_element list
    val search_extension_constructor :
      Odoc_extension.t_extension_constructor -> P_name.t -> result_element list
    val search_type_extension :
      Odoc_extension.t_type_extension -> P_name.t -> result_element list
    val search_exception :
      Odoc_exception.t_exception -> P_name.t -> result_element list
    val search_attribute :
      Odoc_value.t_attribute -> P_name.t -> result_element list
    val search_method :
      Odoc_value.t_method -> P_name.t -> result_element list
    val search_class : Odoc_class.t_class -> P_name.t -> result_element list
    val search_class_type :
      Odoc_class.t_class_type -> P_name.t -> result_element list
    val search_module_type :
      Odoc_module.t_module_type -> P_name.t -> result_element list
    val search_module :
      Odoc_module.t_module -> P_name.t -> result_element list
    val search : Odoc_module.t_module list -> P_name.t -> result_element list
  end

(** A function to search all the values in a list of modules. *)
val values : Odoc_module.t_module list -> Odoc_value.t_value list

(** A function to search all the extension constructors in a list of modules. *)
val extensions :
  Odoc_module.t_module list -> Odoc_extension.t_extension_constructor list

(** A function to search all the exceptions in a list of modules. *)
val exceptions : Odoc_module.t_module list -> Odoc_exception.t_exception list

(** A function to search all the types in a list of modules. *)
val types : Odoc_module.t_module list -> Odoc_type.t_type list

(** A function to search all the class attributes in a list of modules. *)
val attributes : Odoc_module.t_module list -> Odoc_value.t_attribute list

(** A function to search all the class methods in a list of modules. *)
val methods : Odoc_module.t_module list -> Odoc_value.t_method list

(** A function to search all the classes in a list of modules. *)
val classes : Odoc_module.t_module list -> Odoc_class.t_class list

(** A function to search all the class types in a list of modules. *)
val class_types : Odoc_module.t_module list -> Odoc_class.t_class_type list

(** A function to search all the modules in a list of modules. *)
val modules : Odoc_module.t_module list -> Odoc_module.t_module list

(** A function to search all the module types in a list of modules. *)
val module_types : Odoc_module.t_module list -> Odoc_module.t_module_type list

(** Return [true] if a type with the given complete name (regexp) exists
   in the given module list.*)
val type_exists : Odoc_module.t_module list -> Str.regexp -> bool

(** Return [true] if a value with the given complete name (regexp) exists
   in the given module list.*)
val value_exists : Odoc_module.t_module list -> Str.regexp -> bool

(** Return [true] if a module with the given complete name (regexp) exists
   in the given module list.*)
val module_exists : Odoc_module.t_module list -> Str.regexp -> bool

(** Return [true] if a module type with the given complete name (regexp) exists
   in the given module list.*)
val module_type_exists : Odoc_module.t_module list -> Str.regexp -> bool

(** Return [true] if a class with the given complete name (regexp) exists
   in the given module list.*)
val class_exists : Odoc_module.t_module list -> Str.regexp -> bool

(** Return [true] if a class type  with the given complete name (regexp) exists
   in the given module list.*)
val class_type_exists : Odoc_module.t_module list -> Str.regexp -> bool

(** Return [true] if an extension with the given complete name (regexp) exists
   in the given module list.*)
val extension_exists : Odoc_module.t_module list -> Str.regexp -> bool

(** Return [true] if a exception with the given complete name (regexp) exists
   in the given module list.*)
val exception_exists : Odoc_module.t_module list -> Str.regexp -> bool

(** Return [true] if an attribute with the given complete name (regexp) exists
   in the given module list.*)
val attribute_exists : Odoc_module.t_module list -> Str.regexp -> bool

(** Return [true] if a method with the given complete name (regexp) exists
   in the given module list.*)
val method_exists : Odoc_module.t_module list -> Str.regexp -> bool

(** Return the [text] of the section with the given complete name (regexp)
   in the given module list.
   @raise Not_found if the section was not found.*)
val find_section : Odoc_module.t_module list -> Str.regexp -> Odoc_types.text
