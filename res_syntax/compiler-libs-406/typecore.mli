(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Type inference for the core language *)

open Asttypes
open Types
open Format

val is_nonexpansive: Typedtree.expression -> bool

val type_binding:
        Env.t -> rec_flag ->
          Parsetree.value_binding list ->
          Annot.ident option ->
          Typedtree.value_binding list * Env.t
val type_let:
        Env.t -> rec_flag ->
          Parsetree.value_binding list ->
          Annot.ident option ->
          Typedtree.value_binding list * Env.t
val type_expression:
        Env.t -> Parsetree.expression -> Typedtree.expression
val type_class_arg_pattern:
        string -> Env.t -> Env.t -> arg_label -> Parsetree.pattern ->
        Typedtree.pattern * (Ident.t * string loc * Ident.t * type_expr) list *
        Env.t * Env.t
val type_self_pattern:
        string -> type_expr -> Env.t -> Env.t -> Env.t -> Parsetree.pattern ->
        Typedtree.pattern *
        (Ident.t * type_expr) Meths.t ref *
        (Ident.t * Asttypes.mutable_flag * Asttypes.virtual_flag * type_expr)
            Vars.t ref *
        Env.t * Env.t * Env.t
val check_partial:
        ?lev:int -> Env.t -> type_expr ->
        Location.t -> Typedtree.case list -> Typedtree.partial
val type_expect:
        ?in_function:(Location.t * type_expr) ->
        Env.t -> Parsetree.expression -> type_expr -> Typedtree.expression
val type_exp:
        Env.t -> Parsetree.expression -> Typedtree.expression
val type_approx:
        Env.t -> Parsetree.expression -> type_expr
val type_argument:
        Env.t -> Parsetree.expression ->
        type_expr -> type_expr -> Typedtree.expression

val option_some: Typedtree.expression -> Typedtree.expression
val option_none: type_expr -> Location.t -> Typedtree.expression
val extract_option_type: Env.t -> type_expr -> type_expr
val iter_pattern: (Typedtree.pattern -> unit) -> Typedtree.pattern -> unit
val generalizable: int -> type_expr -> bool
val reset_delayed_checks: unit -> unit
val force_delayed_checks: unit -> unit

val name_pattern : string -> Typedtree.case list -> Ident.t

val self_coercion : (Path.t * Location.t list ref) list ref

type error =
    Polymorphic_label of Longident.t
  | Constructor_arity_mismatch of Longident.t * int * int
  | Label_mismatch of Longident.t * (type_expr * type_expr) list
  | Pattern_type_clash of (type_expr * type_expr) list
  | Or_pattern_type_clash of Ident.t * (type_expr * type_expr) list
  | Multiply_bound_variable of string
  | Orpat_vars of Ident.t * Ident.t list
  | Expr_type_clash of (type_expr * type_expr) list
  | Apply_non_function of type_expr
  | Apply_wrong_label of arg_label * type_expr
  | Label_multiply_defined of string
  | Label_missing of Ident.t list
  | Label_not_mutable of Longident.t
  | Wrong_name of string * type_expr * string * Path.t * string * string list
  | Name_type_mismatch of
      string * Longident.t * (Path.t * Path.t) * (Path.t * Path.t) list
  | Invalid_format of string
  | Undefined_method of type_expr * string * string list option
  | Undefined_inherited_method of string * string list
  | Virtual_class of Longident.t
  | Private_type of type_expr
  | Private_label of Longident.t * type_expr
  | Unbound_instance_variable of string * string list
  | Instance_variable_not_mutable of bool * string
  | Not_subtype of (type_expr * type_expr) list * (type_expr * type_expr) list
  | Outside_class
  | Value_multiply_overridden of string
  | Coercion_failure of
      type_expr * type_expr * (type_expr * type_expr) list * bool
  | Too_many_arguments of bool * type_expr
  | Abstract_wrong_label of arg_label * type_expr
  | Scoping_let_module of string * type_expr
  | Masked_instance_variable of Longident.t
  | Not_a_variant_type of Longident.t
  | Incoherent_label_order
  | Less_general of string * (type_expr * type_expr) list
  | Modules_not_allowed
  | Cannot_infer_signature
  | Not_a_packed_module of type_expr
  | Recursive_local_constraint of (type_expr * type_expr) list
  | Unexpected_existential
  | Unqualified_gadt_pattern of Path.t * string
  | Invalid_interval
  | Invalid_for_loop_index
  | No_value_clauses
  | Exception_pattern_below_toplevel
  | Inlined_record_escape
  | Inlined_record_expected
  | Unrefuted_pattern of Typedtree.pattern
  | Invalid_extension_constructor_payload
  | Not_an_extension_constructor
  | Literal_overflow of string
  | Unknown_literal of string * char
  | Illegal_letrec_pat
  | Illegal_letrec_expr
  | Illegal_class_expr

exception Error of Location.t * Env.t * error
exception Error_forward of Location.error

val report_error: Env.t -> formatter -> error -> unit
 (* Deprecated.  Use Location.{error_of_exn, report_error}. *)

(* Forward declaration, to be filled in by Typemod.type_module *)
val type_module: (Env.t -> Parsetree.module_expr -> Typedtree.module_expr) ref
(* Forward declaration, to be filled in by Typemod.type_open *)
val type_open:
  (?used_slot:bool ref -> override_flag -> Env.t -> Location.t ->
   Longident.t loc -> Path.t * Env.t)
    ref
(* Forward declaration, to be filled in by Typeclass.class_structure *)
val type_object:
  (Env.t -> Location.t -> Parsetree.class_structure ->
   Typedtree.class_structure * Types.class_signature * string list) ref
val type_package:
  (Env.t -> Parsetree.module_expr -> Path.t -> Longident.t list ->
  Typedtree.module_expr * type_expr list) ref

val create_package_type : Location.t -> Env.t ->
  Longident.t * (Longident.t * Parsetree.core_type) list ->
  Path.t * (Longident.t * Typedtree.core_type) list * Types.type_expr

val constant: Parsetree.constant -> (Asttypes.constant, error) result

val check_recursive_bindings : Env.t -> Typedtree.value_binding list -> unit
val check_recursive_class_bindings :
  Env.t -> Ident.t list -> Typedtree.class_expr list -> unit
