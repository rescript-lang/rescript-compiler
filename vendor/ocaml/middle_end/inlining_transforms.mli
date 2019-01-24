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

(** Source code transformations used during inlining. *)

(** Inline a function by substituting its body (which may be subject to
    further transformation) at a call site.  The function's declaration is
    not copied.

    This transformation is used when:
    - inlining a call to a non-recursive function;
    - inlining a call, within a recursive or mutually-recursive function, to
      the same or another function being defined simultaneously ("unrolling").
      The maximum depth of unrolling is bounded (see [E.unrolling_allowed]).

    In both cases, the body of the function is copied, within a sequence of
    [let]s that bind the function parameters, the variables "bound by the
    closure" (see flambda.mli), and any function identifiers introduced by the
    set of closures.  These stages are delimited below by comments.

    As an example, suppose we are inlining the following function:

     let f x = x + y
     ...
     let p = f, f in
     (fst p) 42

    The call site [ (fst p) 42] will be transformed to:

      let clos_id = fst p in  (* must eventually yield a closure *)
      let y = <access to [y] in [clos_id]> in
      let x' = 42 in
      let x = x' in
      x + y

    When unrolling a recursive function we rename the arguments to the
    recursive call in order to avoid clashes with existing bindings.  For
    example, suppose we are inlining the following call to [f], which lies
    within its own declaration:

      let rec f x y =
        f (fst x) (y + snd x)

    This will be transformed to:

      let rec f x y =
        let clos_id = f in (* not used this time, since [f] has no free vars *)
        let x' = fst x in
        let y' = y + snd x in
        f (fst x') (y' + snd x')  (* body of [f] with parameters freshened *)
*)
val inline_by_copying_function_body
   : env:Inline_and_simplify_aux.Env.t
  -> r:Inline_and_simplify_aux.Result.t
  -> function_decls:Flambda.function_declarations
  -> lhs_of_application:Variable.t
  -> inline_requested:Lambda.inline_attribute
  -> specialise_requested:Lambda.specialise_attribute
  -> closure_id_being_applied:Closure_id.t
  -> function_decl:Flambda.function_declaration
  -> args:Variable.t list
  -> dbg:Debuginfo.t
  -> simplify:Inlining_decision_intf.simplify
  -> Flambda.t * Inline_and_simplify_aux.Result.t

(** Inlining of recursive function(s) yields a copy of the functions'
    definitions (not just their bodies, unlike the non-recursive case) and
    a direct application of the new body.
    Note: the function really does need to be recursive (but possibly only via
    some mutual recursion) to end up in here; a simultaneous binding [that is
    non-recursive] is not sufficient.
*)
val inline_by_copying_function_declaration
   : env:Inline_and_simplify_aux.Env.t
  -> r:Inline_and_simplify_aux.Result.t
  -> function_decls:Flambda.function_declarations
  -> lhs_of_application:Variable.t
  -> inline_requested:Lambda.inline_attribute
  -> closure_id_being_applied:Closure_id.t
  -> function_decl:Flambda.function_declaration
  -> args:Variable.t list
  -> args_approxs:Simple_value_approx.t list
  -> invariant_params:Variable.Set.t Variable.Map.t lazy_t
  -> specialised_args:Flambda.specialised_to Variable.Map.t
  -> direct_call_surrogates:Closure_id.t Closure_id.Map.t
  -> dbg:Debuginfo.t
  -> simplify:Inlining_decision_intf.simplify
  -> (Flambda.t * Inline_and_simplify_aux.Result.t) option
