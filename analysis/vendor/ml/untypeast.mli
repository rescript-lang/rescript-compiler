(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*      Thomas Gazagnaire (OCamlPro), Fabrice Le Fessant (INRIA Saclay)   *)
(*                                                                        *)
(*   Copyright 2007 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Parsetree

val lident_of_path : Path.t -> Longident.t

type mapper = {
  attribute: mapper -> Typedtree.attribute -> attribute;
  attributes: mapper -> Typedtree.attribute list -> attribute list;
  case: mapper -> Typedtree.case -> case;
  cases: mapper -> Typedtree.case list -> case list;
  class_signature: mapper -> Typedtree.class_signature -> class_signature;
  class_type: mapper -> Typedtree.class_type -> class_type;
  class_type_declaration: mapper -> Typedtree.class_type_declaration
                          -> class_type_declaration;
  class_type_field: mapper -> Typedtree.class_type_field -> class_type_field;
  constructor_declaration: mapper -> Typedtree.constructor_declaration
                           -> constructor_declaration;
  expr: mapper -> Typedtree.expression -> expression;
  extension_constructor: mapper -> Typedtree.extension_constructor
                         -> extension_constructor;
  include_declaration:
    mapper -> Typedtree.include_declaration -> include_declaration;
  include_description:
    mapper -> Typedtree.include_description -> include_description;
  label_declaration:
    mapper -> Typedtree.label_declaration -> label_declaration;
  location: mapper -> Location.t -> Location.t;
  module_binding: mapper -> Typedtree.module_binding -> module_binding;
  module_declaration:
    mapper -> Typedtree.module_declaration -> module_declaration;
  module_expr: mapper -> Typedtree.module_expr -> module_expr;
  module_type: mapper -> Typedtree.module_type -> module_type;
  module_type_declaration:
    mapper -> Typedtree.module_type_declaration -> module_type_declaration;
  package_type: mapper -> Typedtree.package_type -> package_type;
  open_description: mapper -> Typedtree.open_description -> open_description;
  pat: mapper -> Typedtree.pattern -> pattern;
  row_field: mapper -> Typedtree.row_field -> row_field;
  object_field: mapper -> Typedtree.object_field -> object_field;
  signature: mapper -> Typedtree.signature -> signature;
  signature_item: mapper -> Typedtree.signature_item -> signature_item;
  structure: mapper -> Typedtree.structure -> structure;
  structure_item: mapper -> Typedtree.structure_item -> structure_item;
  typ: mapper -> Typedtree.core_type -> core_type;
  type_declaration: mapper -> Typedtree.type_declaration -> type_declaration;
  type_extension: mapper -> Typedtree.type_extension -> type_extension;
  type_kind: mapper -> Typedtree.type_kind -> type_kind;
  value_binding: mapper -> Typedtree.value_binding -> value_binding;
  value_description: mapper -> Typedtree.value_description -> value_description;
  with_constraint:
    mapper -> (Path.t * Longident.t Location.loc * Typedtree.with_constraint)
    -> with_constraint;
}

val default_mapper : mapper

val untype_structure : ?mapper:mapper -> Typedtree.structure -> structure
val untype_signature : ?mapper:mapper -> Typedtree.signature -> signature

val constant : Asttypes.constant -> Parsetree.constant
