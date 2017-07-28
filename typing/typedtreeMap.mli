(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                  Fabrice Le Fessant, INRIA Saclay                   *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Typedtree

module type MapArgument = sig
  val enter_structure : structure -> structure
  val enter_value_description : value_description -> value_description
  val enter_type_declaration : type_declaration -> type_declaration
  val enter_type_extension : type_extension -> type_extension
  val enter_extension_constructor :
    extension_constructor -> extension_constructor
  val enter_pattern : pattern -> pattern
  val enter_expression : expression -> expression
  val enter_package_type : package_type -> package_type
  val enter_signature : signature -> signature
  val enter_signature_item : signature_item -> signature_item
  val enter_module_type_declaration : module_type_declaration -> module_type_declaration
  val enter_module_type : module_type -> module_type
  val enter_module_expr : module_expr -> module_expr
  val enter_with_constraint : with_constraint -> with_constraint
  val enter_class_expr : class_expr -> class_expr
  val enter_class_signature : class_signature -> class_signature
  val enter_class_declaration : class_declaration -> class_declaration
  val enter_class_description : class_description -> class_description
  val enter_class_type_declaration :
    class_type_declaration -> class_type_declaration
  val enter_class_type : class_type -> class_type
  val enter_class_type_field : class_type_field -> class_type_field
  val enter_core_type : core_type -> core_type
  val enter_class_structure : class_structure -> class_structure
  val enter_class_field : class_field -> class_field
  val enter_structure_item : structure_item -> structure_item

  val leave_structure : structure -> structure
  val leave_value_description : value_description -> value_description
  val leave_type_declaration : type_declaration -> type_declaration
  val leave_type_extension : type_extension -> type_extension
  val leave_extension_constructor :
    extension_constructor -> extension_constructor
  val leave_pattern : pattern -> pattern
  val leave_expression : expression -> expression
  val leave_package_type : package_type -> package_type
  val leave_signature : signature -> signature
  val leave_signature_item : signature_item -> signature_item
  val leave_module_type_declaration : module_type_declaration -> module_type_declaration
  val leave_module_type : module_type -> module_type
  val leave_module_expr : module_expr -> module_expr
  val leave_with_constraint : with_constraint -> with_constraint
  val leave_class_expr : class_expr -> class_expr
  val leave_class_signature : class_signature -> class_signature
  val leave_class_declaration : class_declaration -> class_declaration
  val leave_class_description : class_description -> class_description
  val leave_class_type_declaration :
    class_type_declaration -> class_type_declaration
  val leave_class_type : class_type -> class_type
  val leave_class_type_field : class_type_field -> class_type_field
  val leave_core_type : core_type -> core_type
  val leave_class_structure : class_structure -> class_structure
  val leave_class_field : class_field -> class_field
  val leave_structure_item : structure_item -> structure_item

end

module MakeMap :
  functor
    (Iter : MapArgument) ->
sig
  val map_structure : structure -> structure
  val map_pattern : pattern -> pattern
  val map_structure_item : structure_item -> structure_item
  val map_expression : expression -> expression
  val map_class_expr : class_expr -> class_expr

  val map_signature : signature -> signature
  val map_signature_item : signature_item -> signature_item
  val map_module_type : module_type -> module_type
end

module DefaultMapArgument : MapArgument
