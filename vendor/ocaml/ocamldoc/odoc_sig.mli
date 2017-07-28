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

(** The module for analysing a signature and source code and creating modules, classes, ..., elements.*)

(** The functions used to retrieve information from a signature. *)
module Signature_search :
    sig
      type ele
      type tab = (ele, Types.signature_item) Hashtbl.t

      (** Create a table from a signature. This table is used by some
         of the search functions below. *)
      val table : Types.signature -> tab

      (** This function returns the type expression for the value whose name is given,
         in the given signature.
         @raise Not_found if error.*)
      val search_value : tab -> string -> Types.type_expr

      (** This function returns the Types.extension_constructor for the extension whose name is given,
         in the given table.
         @raise Not_found if error.*)
      val search_extension : tab -> string -> Types.extension_constructor

      (** This function returns the Types.type_declaration  for the type whose name is given,
         in the given table.
         @raise Not_found if error.*)
      val search_type : tab -> string -> Types.type_declaration

      (** This function returns the Types.class_declaration  for the class whose name is given,
         in the given table.
         @raise Not_found if error.*)
      val search_class : tab -> string -> Types.class_declaration

      (** This function returns the Types.class_type_declaration  for the class type whose name is given,
         in the given table.
         @raise Not_found if error.*)
      val search_class_type : tab -> string -> Types.class_type_declaration

      (** This function returns the Types.module_type  for the module whose name is given,
         in the given table.
         @raise Not_found if error.*)
      val search_module : tab -> string -> Types.module_type

      (** This function returns the optional Types.module_type  for the module type whose name is given,
         in the given table.
         @raise Not_found if error.*)
      val search_module_type : tab -> string -> Types.module_type option

      (** This function returns the Types.type_expr  for the given val name
         in the given class signature.
         @raise Not_found if error.*)
      val search_attribute_type :
          Types.Vars.key -> Types.class_signature -> Types.type_expr

     (** This function returns the Types.type_expr  for the given method name
        in the given class signature.
        @raise Not_found if error.*)
      val search_method_type :
          string -> Types.class_signature -> Types.type_expr
    end

(** Functions to retrieve simple and special comments from strings. *)
module type Info_retriever =
  sig
    (** Return the couple [(n, list)] where [n] is the number of
       characters read to retrieve [list], which is the list
       of special comments found in the string. *)
    val all_special :
        string -> string -> int * Odoc_types.info list

    (** Return true if the given string contains a blank line. *)
    val blank_line_outside_simple :
        string -> string -> bool

   (** [just_after_special file str] return the pair ([length], [info_opt])
      where [info_opt] is the first optional special comment found
      in [str], without any blank line before. [length] is the number
      of chars from the beginning of [str] to the end of the special comment. *)
    val just_after_special :
        string -> string -> (int * Odoc_types.info option)

   (** [first_special file str] return the pair ([length], [info_opt])
      where [info_opt] is the first optional special comment found
      in [str]. [length] is the number of chars from the beginning of [str]
      to the end of the special comment. *)
    val first_special :
        string -> string -> (int * Odoc_types.info option)

    (** Return a pair [(comment_opt, element_comment_list)], where [comment_opt] is the last special
       comment found in the given string and not followed by a blank line,
       and [element_comment_list] the list of values built from the other
       special comments found and the given function. *)
    val get_comments :
        (Odoc_types.text -> 'a) -> string -> string -> (Odoc_types.info option * 'a list)

  end

module Analyser :
  functor (My_ir : Info_retriever) ->
    sig
      (** This variable is used to load a file as a string and retrieve characters from it.*)
      val file : string ref

      (** The name of the analysed file. *)
      val file_name : string ref

      (** This function takes two indexes (start and end) and return the string
         corresponding to the indexes in the file global variable. The function
         prepare_file must have been called to fill the file global variable.*)
      val get_string_of_file : int -> int -> string

      (** [prepare_file f input_f] sets [file_name] with [f] and loads the file
         [input_f] into [file].*)
      val prepare_file : string -> string -> unit

      (** The function used to get the comments in a class. *)
      val get_comments_in_class : int -> int ->
        (Odoc_types.info option * Odoc_class.class_element list)

      (** The function used to get the comments in a module. *)
      val get_comments_in_module : int -> int ->
        (Odoc_types.info option * Odoc_module.module_element list)

      (** [name_comment_from_type_kind pos_end pos_limit type_kind].
         This function takes a [Parsetree.type_kind] and returns the list of
         (name, optional comment) for the various fields/constructors of the type,
         or an empty list for an abstract type.
         [pos_end] is last char of the complete type definition.
         [pos_limit] is the position of the last char we could use to look for a comment,
         i.e. usually the beginning on the next element.*)
      val name_comment_from_type_decl :
          int -> int -> Parsetree.type_declaration -> int * (string * Odoc_types.info option) list

      (** This function converts a [Types.type_expr] into a [Odoc_type.type_kind],
         by associating the comment found in the parstree of each object field, if any. *)
      val manifest_structure :
          Odoc_env.env -> (string * Odoc_types.info option) list ->
            Types.type_expr -> Odoc_type.type_manifest

      (** This function converts a [Types.type_kind] into a [Odoc_type.type_kind],
         by associating the comment found in the parsetree of each constructor/field, if any.*)
      val get_type_kind :
          Odoc_env.env -> (string * Odoc_types.info option) list ->
            Types.type_kind -> Odoc_type.type_kind

      (** This function merge two optional info structures. *)
      val merge_infos :
          Odoc_types.info option -> Odoc_types.info option ->
            Odoc_types.info option

      (** Return a module_type_kind from a Parsetree.module_type and a Types.module_type *)
      val analyse_module_type_kind :
          ?erased:Odoc_name.Set.t -> Odoc_env.env -> Odoc_name.t ->
            Parsetree.module_type -> Types.module_type ->
              Odoc_module.module_type_kind

      (** Analysis of a Parsetree.class_type and a Types.class_type to
         return a class_type_kind.*)
      val analyse_class_type_kind : Odoc_env.env ->
        Odoc_name.t -> int -> Parsetree.class_type -> Types.class_type ->
          Odoc_class.class_type_kind

      (** This function takes an interface file name, a file containg the code, a parse tree
         and the signature obtained from the compiler.
         It goes through the parse tree, creating values for encountered
         functions, modules, ..., looking in the source file for comments,
         and in the signature for types information. *)
      val analyse_signature :
        string -> string ->
        Parsetree.signature -> Types.signature -> Odoc_module.t_module
    end
