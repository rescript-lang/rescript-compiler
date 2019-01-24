(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** Exported information (that is to say, information written into a .cmx
    file) about a compilation unit. *)

type value_string_contents =
  | Contents of string
  | Unknown_or_mutable

type value_string = {
  contents : value_string_contents;
  size : int;
}

type value_float_array_contents =
  | Contents of float option array
  | Unknown_or_mutable

type value_float_array = {
  contents : value_float_array_contents;
  size : int;
}

type descr =
  | Value_block of Tag.t * approx array
  | Value_mutable_block of Tag.t * int
  | Value_int of int
  | Value_char of char
  | Value_constptr of int
  | Value_float of float
  | Value_float_array of value_float_array
  | Value_boxed_int : 'a Simple_value_approx.boxed_int * 'a -> descr
  | Value_string of value_string
  | Value_closure of value_closure
  | Value_set_of_closures of value_set_of_closures

and value_closure = {
  closure_id : Closure_id.t;
  set_of_closures : value_set_of_closures;
}

and value_set_of_closures = {
  set_of_closures_id : Set_of_closures_id.t;
  bound_vars : approx Var_within_closure.Map.t;
  results : approx Closure_id.Map.t;
  aliased_symbol : Symbol.t option;
}

(* CR-soon mshinwell: Fix the export information so we can correctly
   propagate "unresolved due to..." in the manner of [Simple_value_approx].
   Unfortunately this seems to be complicated by the fact that, during
   [Import_approx], resolution can fail not only due to missing symbols but
   also due to missing export IDs.  The argument type of
   [Simple_value_approx.t] may need updating to reflect this (make the
   symbol optional?  It's only for debugging anyway.) *)
and approx =
  | Value_unknown
  | Value_id of Export_id.t
  | Value_symbol of Symbol.t

(** A structure that describes what a single compilation unit exports. *)
type t = private {
  sets_of_closures : Flambda.function_declarations Set_of_closures_id.Map.t;
  (** Code of exported functions indexed by set of closures IDs. *)
  closures : Flambda.function_declarations Closure_id.Map.t;
  (** Code of exported functions indexed by closure IDs. *)
  values : descr Export_id.Map.t Compilation_unit.Map.t;
  (** Structure of exported values. *)
  symbol_id : Export_id.t Symbol.Map.t;
  (** Associates symbols and values. *)
  offset_fun : int Closure_id.Map.t;
  (** Positions of function pointers in their closures. *)
  offset_fv : int Var_within_closure.Map.t;
  (** Positions of value pointers in their closures. *)
  constant_sets_of_closures : Set_of_closures_id.Set.t;
  (* CR-soon mshinwell for pchambart: Add comment *)
  invariant_params : Variable.Set.t Variable.Map.t Set_of_closures_id.Map.t;
  (* Function parameters known to be invariant (see [Invariant_params])
     indexed by set of closures ID. *)
}

(** Export information for a compilation unit that exports nothing. *)
val empty : t

(** Create a new export information structure. *)
val create
   : sets_of_closures:Flambda.function_declarations Set_of_closures_id.Map.t
  -> closures:Flambda.function_declarations Closure_id.Map.t
  -> values:descr Export_id.Map.t Compilation_unit.Map.t
  -> symbol_id:Export_id.t Symbol.Map.t
  -> offset_fun:int Closure_id.Map.t
  -> offset_fv:int Var_within_closure.Map.t
  -> constant_sets_of_closures:Set_of_closures_id.Set.t
  -> invariant_params:Variable.Set.t Variable.Map.t Set_of_closures_id.Map.t
  -> t

(* CR-someday pchambart: Should we separate [t] in 2 types: one created by the
   current [create] function, returned by [Build_export_info]. And
   another built using t and offset_informations returned by
   [flambda_to_clambda] ?
   mshinwell: I think we should, but after we've done the first release.
*)
(** Record information about the layout of closures and which sets of
    closures are constant.  These are all worked out during the
    [Flambda_to_clambda] pass. *)
val add_clambda_info
   : t
  -> offset_fun:int Closure_id.Map.t
  -> offset_fv:int Var_within_closure.Map.t
  -> constant_sets_of_closures:Set_of_closures_id.Set.t
  -> t

(** Union of export information.  Verifies that there are no identifier
    clashes. *)
val merge : t -> t -> t

(** Look up the description of an exported value given its export ID. *)
val find_description
   : t
  -> Export_id.t
  -> descr

(** Partition a mapping from export IDs by compilation unit. *)
val nest_eid_map
   : 'a Export_id.Map.t
  -> 'a Export_id.Map.t Compilation_unit.Map.t

(**/**)
(* Debug printing functions. *)
val print_approx : Format.formatter -> t * Symbol.t list -> unit
val print_functions : Format.formatter -> t -> unit
val print_offsets : Format.formatter -> t -> unit
val print_all : Format.formatter -> t * Symbol.t list -> unit
