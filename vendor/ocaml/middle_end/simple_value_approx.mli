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

(** Simple approximations to the runtime results of computations.
    This pass is designed for speed rather than accuracy; the performance
    is important since it is used heavily during inlining. *)

type 'a boxed_int =
  | Int32 : int32 boxed_int
  | Int64 : int64 boxed_int
  | Nativeint : nativeint boxed_int

type value_string = {
  contents : string option;  (* [None] if unknown or mutable *)
  size : int;
}

type unresolved_value =
  | Set_of_closures_id of Set_of_closures_id.t
  | Symbol of Symbol.t

type unknown_because_of =
  | Unresolved_value of unresolved_value
  | Other

(** A value of type [t] corresponds to an "approximation" of the result of
    a computation in the program being compiled.  That is to say, it
    represents what knowledge we have about such a result at compile time.
    The simplification pass exploits this information to partially evaluate
    computations.

    At a high level, an approximation for a value [v] has three parts:
    - the "description" (for example, "the constant integer 42");
    - an optional variable;
    - an optional symbol or symbol field.
    If the variable (resp. symbol) is present then that variable (resp.
    symbol) may be used to obtain the value [v].

    The exact semantics of the variable and symbol fields follows.

    Approximations are deduced at particular points in an expression tree,
    but may subsequently be propagated to other locations.

    At the point at which an approximation is built for some value [v], we can
    construct a set of variables (call the set [S]) that are known to alias the
    same value [v].  Each member of [S] will have the same or a more precise
    [descr] field in its approximation relative to the approximation for [v].
    (An increase in precision may currently be introduced for pattern
    matches.)  If [S] is non-empty then it is guaranteed that there is a
    unique member of [S] that was declared in a scope further out ("earlier")
    than all other members of [S].  If such a member exists then it is
    recorded in the [var] field.  Otherwise [var] is [None].

    Analogous to the construction of the set [S], we can construct a set [T]
    consisting of all symbols that are known to alias the value whose
    approximation is being constructed.  If [T] is non-empty then the
    [symbol] field is set to some member of [T]; it does not matter which
    one.  (There is no notion of scope for symbols.)

    Note about mutable blocks:

    Mutable blocks are always represented by [Value_unknown] or
    [Value_bottom].  Any other approximation could leave the door open to
    a miscompilation.   Such bad scenarios are most likely a user using
    [Obj.magic] or [Obj.set_field] in an inappropriate situation.
    Such a situation might be:
    [let x = (1, 1) in
     Obj.set_field (Obj.repr x) 0 (Obj.repr 2);
     assert(fst x = 2)]
    The user would probably expect the assertion to be true, but the
    compiler could in fact propagate the value of [x] across the
    [Obj.set_field].

    Insisting that mutable blocks have [Value_unknown] or [Value_bottom]
    approximations certainly won't always prevent this kind of error, but
    should help catch many of them.

    It is possible that there may be some false positives, with correct
    but unreachable code causing this check to fail.  However the likelihood
    of this seems sufficiently low, especially compared to the advantages
    gained by performing the check, that we include it.

    An example of a pattern that might trigger a false positive is:
    [type a = { a : int }
     type b = { mutable b : int }
     type _ t =
       | A : a t
       | B : b t
     let f (type x) (v:x t) (r:x) =
       match v with
       | A -> r.a
       | B -> r.b <- 2; 3

    let v =
    let r =
      ref A in
      r := A; (* Some pattern that the compiler can't understand *)
      f !r { a = 1 }]
    When inlining [f], the B branch is unreachable, yet the compiler
    cannot prove it and must therefore keep it.
*)
type t = private {
  descr : descr;
  var : Variable.t option;
  symbol : (Symbol.t * int option) option;
}

and descr = private
  | Value_block of Tag.t * t array
  | Value_int of int
  | Value_char of char
  | Value_constptr of int
  | Value_float of float option
  | Value_boxed_int : 'a boxed_int * 'a -> descr
  | Value_set_of_closures of value_set_of_closures
  | Value_closure of value_closure
  | Value_string of value_string
  | Value_float_array of value_float_array
  | Value_unknown of unknown_because_of
  | Value_bottom
  | Value_extern of Export_id.t
  | Value_symbol of Symbol.t
  | Value_unresolved of unresolved_value
    (* No description was found for this value *)

and value_closure = {
  set_of_closures : t;
  closure_id : Closure_id.t;
}

(* CR-soon mshinwell: add support for the approximations of the results, so we
   can do all of the tricky higher-order cases. *)
and value_set_of_closures = private {
  function_decls : Flambda.function_declarations;
  bound_vars : t Var_within_closure.Map.t;
  invariant_params : Variable.Set.t Variable.Map.t lazy_t;
  size : int option Variable.Map.t lazy_t;
  (** For functions that are very likely to be inlined, the size of the
      function's body. *)
  specialised_args : Flambda.specialised_to Variable.Map.t;
  (* Any freshening that has been applied to [function_decls]. *)
  freshening : Freshening.Project_var.t;
  direct_call_surrogates : Closure_id.t Closure_id.Map.t;
}

and value_float_array_contents =
  | Contents of t array
  | Unknown_or_mutable

and value_float_array = {
  contents : value_float_array_contents;
  size : int;
}

(** Extraction of the description of approximation(s). *)
val descr : t -> descr
val descrs : t list -> descr list

(** Pretty-printing of approximations to a formatter. *)
val print : Format.formatter -> t -> unit
val print_descr : Format.formatter -> descr -> unit
val print_value_set_of_closures
   : Format.formatter
  -> value_set_of_closures
  -> unit

val create_value_set_of_closures
   : function_decls:Flambda.function_declarations
  -> bound_vars:t Var_within_closure.Map.t
  -> invariant_params:Variable.Set.t Variable.Map.t lazy_t
  -> specialised_args:Flambda.specialised_to Variable.Map.t
  -> freshening:Freshening.Project_var.t
  -> direct_call_surrogates:Closure_id.t Closure_id.Map.t
  -> value_set_of_closures

val update_freshening_of_value_set_of_closures
   : value_set_of_closures
  -> freshening:Freshening.Project_var.t
  -> value_set_of_closures

(** Basic construction of approximations. *)
val value_unknown : unknown_because_of -> t
val value_int : int -> t
val value_char : char -> t
val value_float : float -> t
val value_any_float : t
val value_mutable_float_array : size:int -> t
val value_immutable_float_array : t array -> t
val value_string : int -> string option -> t
val value_boxed_int : 'i boxed_int -> 'i -> t
val value_constptr : int -> t
val value_block : Tag.t -> t array -> t
val value_extern : Export_id.t -> t
val value_symbol : Symbol.t -> t
val value_bottom : t
val value_unresolved : unresolved_value -> t

(** Construct a closure approximation given the approximation of the
    corresponding set of closures and the closure ID of the closure to
    be projected from such set.  [closure_var] and/or [set_of_closures_var]
    may be specified to augment the approximation with variables that may
    be used to access the closure value itself, so long as they are in
    scope at the proposed point of use. *)
val value_closure
   : ?closure_var:Variable.t
  -> ?set_of_closures_var:Variable.t
  -> ?set_of_closures_symbol:Symbol.t
  -> value_set_of_closures
  -> Closure_id.t
  -> t

(** Construct a set of closures approximation.  [set_of_closures_var] is as for
    the parameter of the same name in [value_closure], above. *)
val value_set_of_closures
   : ?set_of_closures_var:Variable.t
  -> value_set_of_closures
  -> t

(** Take the given constant and produce an appropriate approximation for it
    together with an Flambda expression representing it. *)
val make_const_int : int -> Flambda.t * t
val make_const_char : char -> Flambda.t * t
val make_const_ptr : int -> Flambda.t * t
val make_const_bool : bool -> Flambda.t * t
val make_const_float : float -> Flambda.t * t
val make_const_boxed_int : 'i boxed_int -> 'i -> Flambda.t * t

val make_const_int_named : int -> Flambda.named * t
val make_const_char_named : char -> Flambda.named * t
val make_const_ptr_named : int -> Flambda.named * t
val make_const_bool_named : bool -> Flambda.named * t
val make_const_float_named : float -> Flambda.named * t
val make_const_boxed_int_named : 'i boxed_int -> 'i -> Flambda.named * t

(** Augment an approximation with a given variable (see comment above).
    If the approximation was already augmented with a variable, the one
    passed to this function replaces it within the approximation. *)
val augment_with_variable : t -> Variable.t -> t

(** Like [augment_with_variable], but for symbol information. *)
val augment_with_symbol : t -> Symbol.t -> t

(** Like [augment_with_symbol], but for symbol field information. *)
val augment_with_symbol_field : t -> Symbol.t -> int -> t

(** Replace the description within an approximation. *)
val replace_description : t -> descr -> t

(** Improve the description by taking the kind into account *)
val augment_with_kind : t -> Lambda.value_kind -> t

(** Improve the kind by taking the description into account *)
val augment_kind_with_approx : t -> Lambda.value_kind -> Lambda.value_kind

val equal_boxed_int : 'a boxed_int -> 'a -> 'b boxed_int -> 'b -> bool

(* CR-soon mshinwell for pchambart: Add comment describing semantics.  (Maybe
   we should move the comment from the .ml file into here.) *)
val meet : really_import_approx:(t -> t) -> t -> t -> t

(** An approximation is "known" iff it is not [Value_unknown]. *)
val known : t -> bool

(** An approximation is "useful" iff it is neither unknown nor bottom. *)
val useful : t -> bool

(** Whether all approximations in the given list do *not* satisfy [useful]. *)
val all_not_useful : t list -> bool

(** Whether to warn on attempts to mutate a value.
    It must have been resolved (it cannot be [Value_extern] or
    [Value_symbol]).  (See comment above for further explanation.) *)
val warn_on_mutation : t -> bool

type simplification_summary =
  | Nothing_done
  | Replaced_term

type simplification_result = Flambda.t * simplification_summary * t
type simplification_result_named = Flambda.named * simplification_summary * t

(** Given an expression and its approximation, attempt to simplify the
    expression to a constant (with associated approximation), taking into
    account whether the expression has any side effects. *)
val simplify : t -> Flambda.t -> simplification_result

(** As for [simplify], but also enables us to simplify based on equalities
    between variables.  The caller must provide a function that tells us
    whether, if we simplify to a given variable, the value of that variable
    will be accessible in the current environment. *)
val simplify_using_env
   : t
  -> is_present_in_env:(Variable.t -> bool)
  -> Flambda.t
  -> simplification_result

val simplify_named : t -> Flambda.named -> simplification_result_named

val simplify_named_using_env
   : t
  -> is_present_in_env:(Variable.t -> bool)
  -> Flambda.named
  -> simplification_result_named

(** If the given approximation identifies another variable and
    [is_present_in_env] deems it to be in scope, return that variable (wrapped
    in a [Some]), otherwise return [None]. *)
val simplify_var_to_var_using_env
   : t
  -> is_present_in_env:(Variable.t -> bool)
  -> Variable.t option

val simplify_var : t -> (Flambda.named * t) option

type get_field_result =
  | Ok of t
  | Unreachable

(** Given the approximation [t] of a value, expected to correspond to a block
    (in the [Pmakeblock] sense of the word), and a field index then return
    an appropriate approximation for that field of the block (or
    [Unreachable] if the code with the approximation [t] is unreachable).
    N.B. Not all cases of unreachable code are returned as [Unreachable].
*)
val get_field : t -> field_index:int -> get_field_result

type checked_approx_for_block =
  | Wrong
  | Ok of Tag.t * t array

(** Try to prove that a value with the given approximation may be used
    as a block. *)
val check_approx_for_block : t -> checked_approx_for_block

(** Find the approximation for a bound variable in a set-of-closures
    approximation.  A fatal error is produced if the variable is not bound in
    the given approximation. *)
val approx_for_bound_var : value_set_of_closures -> Var_within_closure.t -> t

(** Given a set-of-closures approximation and a closure ID, apply any
    freshening specified by the approximation to the closure ID, and return
    the resulting ID.  Causes a fatal error if the resulting closure ID does
    not correspond to any function declaration in the approximation. *)
val freshen_and_check_closure_id
   : value_set_of_closures
  -> Closure_id.t
  -> Closure_id.t

type strict_checked_approx_for_set_of_closures =
  | Wrong
  | Ok of Variable.t option * value_set_of_closures

val strict_check_approx_for_set_of_closures
   : t
  -> strict_checked_approx_for_set_of_closures

type checked_approx_for_set_of_closures =
  | Wrong
  | Unresolved of unresolved_value
  | Unknown
  | Unknown_because_of_unresolved_value of unresolved_value
  (* In the [Ok] case, there may not be a variable associated with the set of
     closures; it might be out of scope. *)
  | Ok of Variable.t option * value_set_of_closures

(** Try to prove that a value with the given approximation may be used as a
    set of closures.  Values coming from external compilation units with
    unresolved approximations are permitted. *)
val check_approx_for_set_of_closures : t -> checked_approx_for_set_of_closures

type checked_approx_for_closure =
  | Wrong
  | Ok of value_closure * Variable.t option
          * Symbol.t option * value_set_of_closures

(** Try to prove that a value with the given approximation may be used as a
    closure.  Values coming from external compilation units with unresolved
    approximations are not permitted. *)
(* CR-someday mshinwell: naming is inconsistent: this is as "strict"
   as "strict_check_approx_for_set_of_closures" *)
val check_approx_for_closure : t -> checked_approx_for_closure

type checked_approx_for_closure_allowing_unresolved =
  | Wrong
  | Unresolved of unresolved_value
  | Unknown
  | Unknown_because_of_unresolved_value of unresolved_value
  | Ok of value_closure * Variable.t option
          * Symbol.t option * value_set_of_closures

(** As for [check_approx_for_closure], but values coming from external
    compilation units with unresolved approximations are permitted. *)
val check_approx_for_closure_allowing_unresolved
   : t
  -> checked_approx_for_closure_allowing_unresolved

(** Returns the value if it can be proved to be a constant float *)
val check_approx_for_float : t -> float option

(** Returns the value if it can be proved to be a constant float array *)
val float_array_as_constant : value_float_array -> float list option

(** Returns the value if it can be proved to be a constant string *)
val check_approx_for_string : t -> string option

type switch_branch_selection =
  | Cannot_be_taken
  | Can_be_taken
  | Must_be_taken

(** Check that the branch is compatible with the approximation *)
val potentially_taken_const_switch_branch : t -> int -> switch_branch_selection
val potentially_taken_block_switch_branch : t -> int -> switch_branch_selection
