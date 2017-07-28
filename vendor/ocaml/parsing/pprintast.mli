(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Hongbo Zhang (University of Pennsylvania)                *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

type space_formatter = (unit, Format.formatter, unit) format
class printer :
  unit ->
  object ('b)
    val pipe : bool
    val semi : bool
    method binding :
      Format.formatter -> Parsetree.value_binding -> unit
    method bindings:
        Format.formatter ->
          Asttypes.rec_flag * Parsetree.value_binding list ->
            unit
    method case_list :
      Format.formatter -> Parsetree.case list -> unit
    method class_expr : Format.formatter -> Parsetree.class_expr -> unit
    method class_field : Format.formatter -> Parsetree.class_field -> unit
    method class_params_def :
      Format.formatter -> (Parsetree.core_type * Asttypes.variance) list -> unit
    method class_signature :
      Format.formatter -> Parsetree.class_signature -> unit
    method class_structure :
      Format.formatter -> Parsetree.class_structure -> unit
    method class_type : Format.formatter -> Parsetree.class_type -> unit
    method class_type_declaration_list :
      Format.formatter -> Parsetree.class_type_declaration list -> unit
    method constant : Format.formatter -> Asttypes.constant -> unit
    method constant_string : Format.formatter -> string -> unit
    method core_type : Format.formatter -> Parsetree.core_type -> unit
    method core_type1 : Format.formatter -> Parsetree.core_type -> unit
    method direction_flag :
      Format.formatter -> Asttypes.direction_flag -> unit
    method directive_argument :
      Format.formatter -> Parsetree.directive_argument -> unit
    method exception_declaration :
      Format.formatter -> Parsetree.extension_constructor -> unit
    method expression : Format.formatter -> Parsetree.expression -> unit
    method expression1 : Format.formatter -> Parsetree.expression -> unit
    method expression2 : Format.formatter -> Parsetree.expression -> unit
    method extension_constructor :
      Format.formatter -> Parsetree.extension_constructor -> unit
    method label_exp :
      Format.formatter ->
      Asttypes.label * Parsetree.expression option * Parsetree.pattern ->
      unit
    method label_x_expression_param :
      Format.formatter -> Asttypes.label * Parsetree.expression -> unit
    method list :
      ?sep:space_formatter ->
      ?first:space_formatter ->
      ?last:space_formatter ->
      (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
    method longident : Format.formatter -> Longident.t -> unit
    method longident_loc :
      Format.formatter -> Longident.t Asttypes.loc -> unit
    method module_expr : Format.formatter -> Parsetree.module_expr -> unit
    method module_type : Format.formatter -> Parsetree.module_type -> unit
    method mutable_flag : Format.formatter -> Asttypes.mutable_flag -> unit
    method option :
      ?first:space_formatter ->
      ?last:space_formatter ->
      (Format.formatter -> 'a -> unit) ->
      Format.formatter -> 'a option -> unit
    method paren :
        ?first:space_formatter -> ?last:space_formatter -> bool ->
          (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit
    method pattern : Format.formatter -> Parsetree.pattern -> unit
    method pattern1 : Format.formatter -> Parsetree.pattern -> unit
    method payload : Format.formatter -> Parsetree.payload -> unit
    method private_flag : Format.formatter -> Asttypes.private_flag -> unit
    method rec_flag : Format.formatter -> Asttypes.rec_flag -> unit
    method nonrec_flag : Format.formatter -> Asttypes.rec_flag -> unit

    method reset : 'b
    method reset_semi : 'b
    method reset_ifthenelse : 'b
    method reset_pipe : 'b

    method signature :
      Format.formatter -> Parsetree.signature_item list -> unit
    method signature_item :
      Format.formatter -> Parsetree.signature_item -> unit
    method simple_expr : Format.formatter -> Parsetree.expression -> unit
    method simple_pattern : Format.formatter -> Parsetree.pattern -> unit
    method string_quot : Format.formatter -> Asttypes.label -> unit
    method structure :
      Format.formatter -> Parsetree.structure_item list -> unit
    method structure_item :
      Format.formatter -> Parsetree.structure_item -> unit
    method sugar_expr : Format.formatter -> Parsetree.expression -> bool
    method toplevel_phrase :
      Format.formatter -> Parsetree.toplevel_phrase -> unit
    method type_declaration :
      Format.formatter -> Parsetree.type_declaration -> unit
    method type_def_list :
      Format.formatter -> Parsetree.type_declaration list -> unit
    method type_extension :
      Format.formatter -> Parsetree.type_extension -> unit
    method type_param :
      Format.formatter -> Parsetree.core_type * Asttypes.variance -> unit
    method type_params :
      Format.formatter -> (Parsetree.core_type * Asttypes.variance) list -> unit
    method type_with_label :
      Format.formatter -> Asttypes.label * Parsetree.core_type -> unit
    method tyvar : Format.formatter -> string -> unit
    method under_pipe : 'b
    method under_semi : 'b
    method under_ifthenelse : 'b
    method value_description :
      Format.formatter -> Parsetree.value_description -> unit
    method virtual_flag : Format.formatter -> Asttypes.virtual_flag -> unit
    method attribute : Format.formatter -> Parsetree.attribute -> unit
    method item_attribute : Format.formatter -> Parsetree.attribute -> unit
    method floating_attribute : Format.formatter -> Parsetree.attribute -> unit
    method attributes : Format.formatter -> Parsetree.attributes -> unit
    method item_attributes : Format.formatter -> Parsetree.attributes -> unit
    method extension : Format.formatter -> Parsetree.extension -> unit
    method item_extension : Format.formatter -> Parsetree.extension -> unit
  end
val default : printer
val toplevel_phrase : Format.formatter -> Parsetree.toplevel_phrase -> unit
val expression : Format.formatter -> Parsetree.expression -> unit
val string_of_expression : Parsetree.expression -> string
val top_phrase: Format.formatter -> Parsetree.toplevel_phrase -> unit
val core_type: Format.formatter -> Parsetree.core_type -> unit
val pattern: Format.formatter -> Parsetree.pattern -> unit
val signature: Format.formatter -> Parsetree.signature -> unit
val structure: Format.formatter -> Parsetree.structure -> unit
val string_of_structure: Parsetree.structure -> string
