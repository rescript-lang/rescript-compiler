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

(** The module for analysing the typed abstract syntax tree and source code and creating modules, classes, ..., elements.*)

type typedtree = Typedtree.structure * Typedtree.module_coercion

(** This module is used to search for structure items by name in a [Typedtree.structure]. *)
module Typedtree_search :
    sig
      type ele

      type tab = (ele, Typedtree.structure_item_desc) Hashtbl.t
      type tab_values = (Odoc_name.t, Typedtree.pattern * Typedtree.expression) Hashtbl.t

      (** Create hash tables used to search by some of the functions below. *)
      val tables : Typedtree.structure_item list -> tab * tab_values

      (** This function returns the [Typedtree.module_expr] associated to the given module name,
         in the given table.
         @raise Not_found if the module was not found.*)
      val search_module : tab -> string -> Typedtree.module_expr

      (** This function returns the [Typedtree.module_type] associated to the given module type name,
         in the given table.
         @raise Not_found if the module type was not found.*)
      val search_module_type : tab -> string -> Typedtree.module_type_declaration

      (** This function returns the [Typedtree.type_extension] associated to the given extension name,
         in the given table.
         @raise Not_found if the extension was not found.*)
      val search_extension : tab -> string -> Typedtree.type_extension

      (** This function returns the [Typedtree.type_declaration] associated to the given type name,
         in the given table.
         @raise Not_found if the type was not found. *)
      val search_type_declaration : tab -> string -> Typedtree.type_declaration

      (** This function returns the [Typedtree.class_expr] and type parameters
         associated to the given class name, in the given table.
         @raise Not_found if the class was not found. *)
      val search_class_exp : tab -> string -> (Typedtree.class_expr * (Types.type_expr list))

      (** This function returns the [Typedtree.class_type_declaration] associated to the given class type name,
         in the given table.
         @raise Not_found if the class type was not found. *)
      val search_class_type_declaration : tab -> string -> Typedtree.class_type_declaration

      (** This function returns the couple (pat, exp) for the given value name, in the
         given table of values.
         @raise Not found if no value matches the name.*)
      val search_value : tab_values -> string -> Typedtree.pattern * Typedtree.expression

      (** This function returns the [type_expr] for the given primitive name, in the
         given table.
         @raise Not found if no value matches the name.*)
      val search_primitive : tab -> string -> Types.type_expr

      (** This function returns the [Typedtree.class_expr] associated to
         the n'th inherit in the given class structure of typed tree.
         @raise Not_found if the class expression could not be found.*)
      val get_nth_inherit_class_expr :
          Typedtree.class_structure -> int -> Typedtree.class_expr

      (** This function returns the [Types.type_expr] of the attribute
         whose name is given, in a given class structure.
         @raise Not_found if the class attribute could not be found.*)
      val search_attribute_type :
          Typedtree.class_structure -> string -> Types.type_expr

      (** This function returns the [Types.expression] of the method whose name is given, in a given class structure.
         @raise Not_found if the class method could not be found.*)
      val search_method_expression :
          Typedtree.class_structure -> string -> Typedtree.expression
    end

(** The module which performs the analysis of a typed tree.
   The module uses the module {!Odoc_sig.Analyser}.
   @param My_ir The module used to retrieve comments and special comments.*)
module Analyser :
  functor (My_ir : Odoc_sig.Info_retriever) ->
    sig
      (** This function takes a file name, a file containg the code and
         the typed tree obtained from the compiler.
         It goes through the tree, creating values for encountered
         functions, modules, ..., and looking in the source file for comments.*)
      val analyse_typed_tree :
        string -> string -> Parsetree.structure -> typedtree -> Odoc_module.t_module
    end
