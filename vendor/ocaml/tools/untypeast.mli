(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*      Thomas Gazagnaire (OCamlPro), Fabrice Le Fessant (INRIA Saclay)   *)
(*                                                                        *)
(*   Copyright 2007 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

val untype_structure : Typedtree.structure -> Parsetree.structure
val untype_signature : Typedtree.signature -> Parsetree.signature
val untype_expression : Typedtree.expression -> Parsetree.expression
val untype_type_declaration :
  Typedtree.type_declaration -> Parsetree.type_declaration
val untype_module_type : Typedtree.module_type -> Parsetree.module_type

val lident_of_path : Path.t -> Longident.t
