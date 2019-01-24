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

(** Environments and result structures used during inlining and
    simplification.  (See inline_and_simplify.ml.) *)

module Env : sig
  (** Environments follow the lexical scopes of the program. *)
  type t

  (** Create a new environment.  If [never_inline] is true then the returned
      environment will prevent [Inline_and_simplify] from inlining.  The
      [backend] parameter is used for passing information about the compiler
      backend being used.
      Newly-created environments have inactive [Freshening]s (see below) and do
      not initially hold any approximation information. *)
  val create
     : never_inline:bool
    -> backend:(module Backend_intf.S)
    -> round:int
    -> t

  (** Obtain the first-class module that gives information about the
      compiler backend being used for compilation. *)
  val backend : t -> (module Backend_intf.S)

  (** Obtain the really_import_approx function from the backend module. *)
  val really_import_approx
     : t
    -> (Simple_value_approx.t -> Simple_value_approx.t)

  (** Which simplification round we are currently in. *)
  val round : t -> int

  (** Add the approximation of a variable---that is to say, some knowledge
      about the value(s) the variable may take on at runtime---to the
      environment. *)
  val add : t -> Variable.t -> Simple_value_approx.t -> t

  val add_outer_scope : t -> Variable.t -> Simple_value_approx.t -> t

  (** Like [add], but for mutable variables. *)
  val add_mutable : t -> Mutable_variable.t -> Simple_value_approx.t -> t

  (** Find the approximation of a given variable, raising a fatal error if
      the environment does not know about the variable.  Use [find_opt]
      instead if you need to catch the failure case. *)
  val find_exn : t -> Variable.t -> Simple_value_approx.t

  (** Like [find_exn], but for mutable variables. *)
  val find_mutable_exn : t -> Mutable_variable.t -> Simple_value_approx.t

  type scope = Current | Outer

  val find_with_scope_exn : t -> Variable.t -> scope * Simple_value_approx.t

  (** Like [find_exn], but intended for use where the "not present in
      environment" case is to be handled by the caller. *)
  val find_opt : t -> Variable.t -> Simple_value_approx.t option

  (** Like [find_exn], but for a list of variables. *)
  val find_list_exn : t -> Variable.t list -> Simple_value_approx.t list

  val does_not_bind : t -> Variable.t list -> bool

  val does_not_freshen : t -> Variable.t list -> bool

  val add_symbol : t -> Symbol.t -> Simple_value_approx.t -> t
  val redefine_symbol : t -> Symbol.t -> Simple_value_approx.t -> t
  val find_symbol_exn : t -> Symbol.t -> Simple_value_approx.t
  val find_symbol_opt : t -> Symbol.t -> Simple_value_approx.t option
  val find_symbol_fatal : t -> Symbol.t -> Simple_value_approx.t

  (* Like [find_symbol_exn], but load the symbol approximation using
     the backend if not available in the environment. *)
  val find_or_load_symbol : t -> Symbol.t -> Simple_value_approx.t

  (** Note that the given [bound_to] holds the given [projection]. *)
  val add_projection
     : t
    -> projection:Projection.t
    -> bound_to:Variable.t
    -> t

  (** Determine if the environment knows about a variable that is bound
      to the given [projection]. *)
  val find_projection
     : t
    -> projection:Projection.t
    -> Variable.t option

  (** Whether the environment has an approximation for the given variable. *)
  val mem : t -> Variable.t -> bool

  (** Return the freshening that should be applied to variables when
      rewriting code (in [Inline_and_simplify], etc.) using the given
      environment. *)
  val freshening : t -> Freshening.t

  (** Set the freshening that should be used as per [freshening], above. *)
  val set_freshening : t -> Freshening.t -> t

  (** Causes every bound variable in code rewritten during inlining and
      simplification, using the given environment, to be freshened.  This is
      used when descending into subexpressions substituted into existing
      expressions. *)
  val activate_freshening : t -> t

  (** Erase all variable approximation information and freshening information
      from the given environment.  However, the freshening activation state
      is preserved.  This function is used when rewriting inside a function
      declaration, to avoid (due to a compiler bug) accidental use of
      variables from outer scopes that are not accessible. *)
  val local : t -> t

  (** Note that the inliner is descending into a function body from the given
      set of closures.  A set of such descents is maintained. *)
  (* CR-someday mshinwell: consider changing name to remove "declaration".
     Also, isn't this the inlining stack?  Maybe we can use that instead. *)
  val enter_set_of_closures_declaration : Set_of_closures_origin.t -> t -> t

  (** Determine whether the inliner is currently inside a function body from
      the given set of closures.  This is used to detect whether a given
      function call refers to a function which exists somewhere on the current
      inlining stack. *)
  val inside_set_of_closures_declaration : Set_of_closures_origin.t -> t -> bool

  (** Not inside a closure declaration.
      Toplevel code is the one evaluated when the compilation unit is
      loaded *)
  val at_toplevel : t -> bool

  val is_inside_branch : t -> bool
  val branch_depth : t -> int
  val inside_branch : t -> t

  val increase_closure_depth : t -> t

  (** Mark that call sites contained within code rewritten using the given
      environment should never be replaced by inlined (or unrolled) versions
      of the callee(s). *)
  val set_never_inline : t -> t

  (** Equivalent to [set_never_inline] but only applies to code inside
      a set of closures. *)
  val set_never_inline_inside_closures : t -> t

  (** Unset the restriction from [set_never_inline_inside_closures] *)
  val unset_never_inline_inside_closures : t -> t

  (** Equivalent to [set_never_inline] but does not apply to code inside
      a set of closures. *)
  val set_never_inline_outside_closures : t -> t

  (** Unset the restriction from [set_never_inline_outside_closures] *)
  val unset_never_inline_outside_closures : t -> t

  (** Return whether [set_never_inline] is currently in effect on the given
      environment. *)
  val never_inline : t -> bool

  val inlining_level : t -> int

  (** Mark that this environment is used to rewrite code for inlining. This is
      used by the inlining heuristics to decide whether to continue.
      Unconditionally inlined does not take this into account. *)
  val inlining_level_up : t -> t

  (** Whether we are actively unrolling a given function. *)
  val actively_unrolling : t -> Set_of_closures_origin.t -> int option

  (** Start actively unrolling a given function [n] times. *)
  val start_actively_unrolling : t -> Set_of_closures_origin.t -> int -> t

  (** Unroll a function currently actively being unrolled. *)
  val continue_actively_unrolling : t -> Set_of_closures_origin.t -> t

  (** Whether it is permissible to unroll a call to a recursive function
      in the given environment. *)
  val unrolling_allowed : t -> Set_of_closures_origin.t -> bool

  (** Whether the given environment is currently being used to rewrite the
      body of an unrolled recursive function. *)
  val inside_unrolled_function : t -> Set_of_closures_origin.t -> t

  (** Whether it is permissible to inline a call to a function in the given
      environment. *)
  val inlining_allowed : t -> Closure_id.t -> bool

  (** Whether the given environment is currently being used to rewrite the
      body of an inlined function. *)
  val inside_inlined_function : t -> Closure_id.t -> t

  (** If collecting inlining statistics, record that the inliner is about to
      descend into [closure_id].  This information enables us to produce a
      stack of closures that form a kind of context around an inlining
      decision point. *)
  val note_entering_closure
     : t
    -> closure_id:Closure_id.t
    -> dbg:Debuginfo.t
    -> t

   (** If collecting inlining statistics, record that the inliner is about to
       descend into a call to [closure_id].  This information enables us to
       produce a stack of closures that form a kind of context around an
       inlining decision point. *)
  val note_entering_call
     : t
    -> closure_id:Closure_id.t
    -> dbg:Debuginfo.t
    -> t

   (** If collecting inlining statistics, record that the inliner is about to
       descend into an inlined function call.  This requires that the inliner
       has already entered the call with [note_entering_call]. *)
  val note_entering_inlined : t -> t

   (** If collecting inlining statistics, record that the inliner is about to
       descend into a specialised function definition.  This requires that the
       inliner has already entered the call with [note_entering_call]. *)
  val note_entering_specialised : t -> closure_ids:Closure_id.Set.t -> t

  (** Update a given environment to record that the inliner is about to
      descend into [closure_id] and pass the resulting environment to [f].
      If [inline_inside] is [false] then the environment passed to [f] will be
      marked as [never_inline] (see above). *)
  val enter_closure
     : t
    -> closure_id:Closure_id.t
    -> inline_inside:bool
    -> dbg:Debuginfo.t
    -> f:(t -> 'a)
    -> 'a

   (** If collecting inlining statistics, record an inlining decision for the
       call at the top of the closure stack stored inside the given
       environment. *)
  val record_decision
     : t
    -> Inlining_stats_types.Decision.t
    -> unit

  (** Print a human-readable version of the given environment. *)
  val print : Format.formatter -> t -> unit

  (** The environment stores the call-site being inlined to produce
      precise location information. This function sets the current
      call-site being inlined.  *)
  val set_inline_debuginfo : t -> dbg:Debuginfo.t -> t

  (** Appends the locations of inlined call-sites to the [~dbg] argument *)
  val add_inlined_debuginfo : t -> dbg:Debuginfo.t -> Debuginfo.t
end

module Result : sig
  (** Result structures approximately follow the evaluation order of the
      program.  They are returned by the simplification algorithm acting on
      an Flambda subexpression. *)
  type t

  val create : unit -> t

  (** The approximation of the subexpression that has just been
      simplified. *)
  val approx : t -> Simple_value_approx.t

  (** Set the approximation of the subexpression that has just been
      simplified.  Typically used just before returning from a case of the
      simplification algorithm. *)
  val set_approx : t -> Simple_value_approx.t -> t

  (** Set the approximation of the subexpression to the meet of the
      current return approximation and the provided one. Typically
      used just before returning from a branch case of the
      simplification algorithm. *)
  val meet_approx : t -> Env.t -> Simple_value_approx.t -> t

  (** All static exceptions for which [use_staticfail] has been called on
      the given result structure. *)
  val used_static_exceptions : t -> Static_exception.Set.t

  (** Mark that the given static exception has been used. *)
  val use_static_exception : t -> Static_exception.t -> t

  (** Mark that we are moving up out of the scope of a static-catch block
      that catches the given static exception identifier.  This has the effect
      of removing the identifier from the [used_staticfail] set. *)
  val exit_scope_catch : t -> Static_exception.t -> t

  (** The benefit to be gained by inlining the subexpression whose
      simplification yielded the given result structure. *)
  val benefit : t -> Inlining_cost.Benefit.t

  (** Apply a transformation to the inlining benefit stored within the
      given result structure. *)
  val map_benefit
    : t
    -> (Inlining_cost.Benefit.t -> Inlining_cost.Benefit.t)
    -> t

  (** Add some benefit to the inlining benefit stored within the
      given result structure. *)
  val add_benefit : t -> Inlining_cost.Benefit.t -> t

  (** Set the benefit of inlining the subexpression corresponding to the
      given result structure to zero. *)
  val reset_benefit : t -> t

  val set_inlining_threshold :
    t -> Inlining_cost.Threshold.t option -> t
  val add_inlining_threshold :
    t -> Inlining_cost.Threshold.t -> t
  val sub_inlining_threshold :
    t -> Inlining_cost.Threshold.t -> t
  val inlining_threshold : t -> Inlining_cost.Threshold.t option

  val seen_direct_application : t -> t
  val num_direct_applications : t -> int
end

(** Command line argument -inline *)
val initial_inlining_threshold : round:int -> Inlining_cost.Threshold.t

(** Command line argument -inline-toplevel *)
val initial_inlining_toplevel_threshold
  : round:int -> Inlining_cost.Threshold.t

val prepare_to_simplify_set_of_closures
   : env:Env.t
  -> set_of_closures:Flambda.set_of_closures
  -> function_decls:Flambda.function_declarations
  -> freshen:bool
  -> only_for_function_decl:Flambda.function_declaration option
  -> (Flambda.specialised_to * Simple_value_approx.t) Variable.Map.t  (* fvs *)
    * Flambda.specialised_to Variable.Map.t         (* specialised arguments *)
    * Flambda.function_declarations
    * Simple_value_approx.t Variable.Map.t       (* parameter approximations *)
    * Simple_value_approx.value_set_of_closures
    * Env.t

val prepare_to_simplify_closure
   : function_decl:Flambda.function_declaration
  -> free_vars:(Flambda.specialised_to * Simple_value_approx.t) Variable.Map.t
  -> specialised_args:Flambda.specialised_to Variable.Map.t
  -> parameter_approximations:Simple_value_approx.t Variable.Map.t
  -> set_of_closures_env:Env.t
  -> Env.t
