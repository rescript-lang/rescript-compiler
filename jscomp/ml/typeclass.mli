(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Types
open Format

type 'a class_info = {
  cls_id : Ident.t;
  cls_id_loc : string loc;
  cls_decl : class_declaration;
  cls_ty_id : Ident.t;
  cls_ty_decl : class_type_declaration;
  cls_obj_id : Ident.t;
  cls_obj_abbr : type_declaration;
  cls_typesharp_id : Ident.t;
  cls_abbr : type_declaration;
  cls_arity : int;
  cls_pub_methods : string list;
  cls_info : 'a;
}

type class_type_info = {
  clsty_ty_id : Ident.t;
  clsty_id_loc : string loc;
  clsty_ty_decl : class_type_declaration;
  clsty_obj_id : Ident.t;
  clsty_obj_abbr : type_declaration;
  clsty_typesharp_id : Ident.t;
  clsty_abbr : type_declaration;
  clsty_info : Typedtree.class_type_declaration;
}

val class_declarations:
  Env.t -> Parsetree.class_declaration list ->
  Typedtree.class_declaration class_info list * Env.t

(*
and class_declaration =
  (class_expr, Types.class_declaration) class_infos
*)

val class_descriptions:
  Env.t -> Parsetree.class_description list ->
  Typedtree.class_description class_info list * Env.t

(*
and class_description =
  (class_type, unit) class_infos
*)

val class_type_declarations:
  Env.t -> Parsetree.class_description list -> class_type_info list * Env.t

(*
and class_type_declaration =
  (class_type, Types.class_type_declaration) class_infos
*)

val approx_class_declarations:
  Env.t -> Parsetree.class_description list -> class_type_info list

val virtual_methods: Types.class_signature -> label list

(*
val type_classes :
           bool ->
           ('a -> Types.type_expr) ->
           (Env.t -> 'a -> 'b * Types.class_type) ->
           Env.t ->
           'a Parsetree.class_infos list ->
  (  Ident.t * Types.class_declaration *
     Ident.t * Types.class_type_declaration *
     Ident.t * Types.type_declaration *
     Ident.t * Types.type_declaration *
     int * string list * 'b * 'b Typedtree.class_infos)
           list * Env.t
*)

type error =
    Unconsistent_constraint of (type_expr * type_expr) list
  | Field_type_mismatch of string * string * (type_expr * type_expr) list
  | Structure_expected of class_type
  | Cannot_apply of class_type
  | Apply_wrong_label of arg_label
  | Pattern_type_clash of type_expr
  | Repeated_parameter
  | Unbound_class_2 of Longident.t
  | Unbound_class_type_2 of Longident.t
  | Abbrev_type_clash of type_expr * type_expr * type_expr
  | Constructor_type_mismatch of string * (type_expr * type_expr) list
  | Virtual_class of bool * bool * string list * string list
  | Parameter_arity_mismatch of Longident.t * int * int
  | Parameter_mismatch of (type_expr * type_expr) list
  | Bad_parameters of Ident.t * type_expr * type_expr
  | Class_match_failure of Ctype.class_match_failure list
  | Unbound_val of string
  | Unbound_type_var of (formatter -> unit) * Ctype.closed_class_failure
  | Make_nongen_seltype of type_expr
  | Non_generalizable_class of Ident.t * Types.class_declaration
  | Cannot_coerce_self of type_expr
  | Non_collapsable_conjunction of
      Ident.t * Types.class_declaration * (type_expr * type_expr) list
  | Final_self_clash of (type_expr * type_expr) list
  | Mutability_mismatch of string * mutable_flag
  | No_overriding of string * string
  | Duplicate of string * string

exception Error of Location.t * Env.t * error
exception Error_forward of Location.error

val report_error : Env.t -> formatter -> error -> unit
