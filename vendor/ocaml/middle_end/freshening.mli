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

(** Freshening of various identifiers. *)

(** A table used for freshening variables and static exception identifiers. *)
type t
type subst = t

(** The freshening that does nothing.  This is the unique inactive
    freshening. *)
val empty : t

(** Activate the freshening.  Without activation, operations to request
    freshenings have no effect (cf. the documentation below for
    [add_variable]).  As such, the inactive renaming is unique. *)
val activate : t -> t

(** Given the inactive freshening, return the same; otherwise, return an
    empty active freshening. *)
val empty_preserving_activation_state : t -> t

(** [add_variable t var]
    If [t] is active:
      It returns a fresh variable [new_var] and adds [var] -> [new_var]
      to the freshening.
      If a renaming [other_var] -> [var] or [symbol] -> [var] was already
      present in [t], it will also add [other_var] -> [new_var] and
      [symbol] -> [new_var].
    If [t] is inactive, this is the identity.
*)
val add_variable : t -> Variable.t -> Variable.t * t

(** Like [add_variable], but for multiple variables, each freshened
    separately. *)
val add_variables'
   : t
  -> Variable.t list
  -> Variable.t list * t

(** Like [add_variables'], but passes through the second component of the
    input list unchanged. *)
val add_variables
   : t
  -> (Variable.t * 'a) list
  -> (Variable.t * 'a) list * t

(** Like [add_variable], but for mutable variables. *)
val add_mutable_variable : t -> Mutable_variable.t -> Mutable_variable.t * t

(** As for [add_variable], but for static exception identifiers. *)
val add_static_exception : t -> Static_exception.t -> Static_exception.t * t

(** [apply_variable t var] applies the freshening [t] to [var].
    If no renaming is specified in [t] for [var] it is returned unchanged. *)
val apply_variable : t -> Variable.t -> Variable.t

(** As for [apply_variable], but for mutable variables. *)
val apply_mutable_variable : t -> Mutable_variable.t -> Mutable_variable.t

(** As for [apply_variable], but for static exception identifiers. *)
val apply_static_exception : t -> Static_exception.t -> Static_exception.t

(** Replace recursive accesses to the closures in the set through
    [Symbol] by the corresponding [Var]. This is used to recover
    the recursive call when importing code from another compilation unit.

    If the renaming is inactive, this is the identity.
*)
val rewrite_recursive_calls_with_symbols
   : t
  -> Flambda.function_declarations
  -> make_closure_symbol:(Closure_id.t -> Symbol.t)
  -> Flambda.function_declarations

(* CR-soon mshinwell for mshinwell: maybe inaccurate module name, it freshens
   closure IDs as well.  Check use points though *)
module Project_var : sig
  (** A table used for freshening of identifiers in [Project_closure] and
      [Move_within_set_of_closures] ("ids of closures"); and [Project_var]
      ("bound vars of closures") expressions.

      This information is propagated bottom up and populated when inlining a
      function containing a closure declaration.

      For instance,
        [let f x =
           let g y = ... x ... in
           ... g.x ...           (Project_var x)
           ... g 1 ...           (Apply (Project_closure g ...))
           ]

      If f is inlined, g is renamed. The approximation of g will carry this
      table such that later the access to the field x of g and selection of
      g in the closure can be substituted.
   *)
  type t

  (* The freshening that does nothing. *)
  val empty : t

  (** Composition of two freshenings. *)
  val compose : earlier:t -> later:t -> t

  (** Freshen a closure ID based on the given renaming.  The same ID is
      returned if the renaming does not affect it.
      If dealing with approximations, you probably want to use
      [Simple_value_approx.freshen_and_check_closure_id] instead of this
      function.
  *)
  val apply_closure_id : t -> Closure_id.t -> Closure_id.t

  (** Like [apply_closure_id], but for variables within closures. *)
  val apply_var_within_closure
     : t
    -> Var_within_closure.t
    -> Var_within_closure.t

  val print : Format.formatter -> t -> unit
end

(* CR-soon mshinwell for mshinwell: add comment *)
val apply_function_decls_and_free_vars
   : t
  -> (Flambda.specialised_to * 'a) Variable.Map.t
  -> Flambda.function_declarations
  -> only_freshen_parameters:bool
  -> (Flambda.specialised_to * 'a) Variable.Map.t
    * Flambda.function_declarations
    * t
    * Project_var.t

val does_not_freshen : t -> Variable.t list -> bool

val print : Format.formatter -> t -> unit

(** N.B. This does not freshen the domain of the supplied map, only the
    range. *)
(* CR-someday mshinwell: consider fixing that *)
val freshen_projection_relation
   : Flambda.specialised_to Variable.Map.t
  -> freshening:t
  -> closure_freshening:Project_var.t
  -> Flambda.specialised_to Variable.Map.t

val freshen_projection_relation'
   : (Flambda.specialised_to * 'a) Variable.Map.t
  -> freshening:t
  -> closure_freshening:Project_var.t
  -> (Flambda.specialised_to * 'a) Variable.Map.t
