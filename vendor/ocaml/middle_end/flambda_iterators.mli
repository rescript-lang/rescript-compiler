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

(* CR-soon mshinwell: we need to document whether these iterators follow any
   particular order. *)

(** Apply the given functions to the immediate subexpressions of the given
    Flambda expression.  For avoidance of doubt, if a subexpression is
    [Expr], it is passed to the function taking [Flambda.named], rather
    than being followed and passed to the function taking [Flambda.t]. *)
val apply_on_subexpressions
   : (Flambda.t -> unit)
  -> (Flambda.named -> unit)
  -> Flambda.t
  -> unit

val map_subexpressions
   : (Flambda.t -> Flambda.t)
  -> (Variable.t -> Flambda.named -> Flambda.named)
  -> Flambda.t
  -> Flambda.t

(* CR-soon lwhite: add comment to clarify that these recurse unlike the
   ones above *)
val iter
   : (Flambda.t -> unit)
  -> (Flambda.named -> unit)
  -> Flambda.t
  -> unit

val iter_expr
   : (Flambda.t -> unit)
  -> Flambda.t
  -> unit

val iter_on_named
   : (Flambda.t -> unit)
  -> (Flambda.named -> unit)
  -> Flambda.named
  -> unit

(* CR-someday mshinwell: we might need to add the corresponding variable to
   the parameters of the user function for [iter_named] *)
val iter_named
   : (Flambda.named -> unit)
  -> Flambda.t
  -> unit

(* CR-someday lwhite: These names are pretty indecipherable, perhaps
   create submodules for the normal and "on_named" variants of each
   function. *)

val iter_named_on_named
   : (Flambda.named -> unit)
  -> Flambda.named
  -> unit

(** [iter_toplevel f t] applies [f] on every toplevel subexpression of [t].
    In particular, it never applies [f] to the body of a function (which
    will always be contained within an [Set_of_closures] expression). *)
val iter_toplevel
   : (Flambda.t -> unit)
  -> (Flambda.named -> unit)
  -> Flambda.t
  -> unit

val iter_named_toplevel
   : (Flambda.t -> unit)
  -> (Flambda.named -> unit)
  -> Flambda.named
  -> unit

val iter_on_sets_of_closures
   : (Flambda.set_of_closures -> unit)
  -> Flambda.t
  -> unit

val iter_on_set_of_closures_of_program
   : Flambda.program
  -> f:(constant:bool -> Flambda.set_of_closures -> unit)
  -> unit

val iter_all_immutable_let_and_let_rec_bindings
   : Flambda.t
  -> f:(Variable.t -> Flambda.named -> unit)
  -> unit

val iter_all_toplevel_immutable_let_and_let_rec_bindings
   : Flambda.t
  -> f:(Variable.t -> Flambda.named -> unit)
  -> unit

val iter_exprs_at_toplevel_of_program
   : Flambda.program
  -> f:(Flambda.t -> unit)
  -> unit

val iter_named_of_program
   : Flambda.program
  -> f:(Flambda.named -> unit)
  -> unit

val iter_constant_defining_values_on_program
  : Flambda.program
  -> f:(Flambda.constant_defining_value -> unit)
  -> unit

val iter_apply_on_program
   : Flambda.program
  -> f:(Flambda.apply -> unit)
  -> unit

val map
   : (Flambda.t -> Flambda.t)
  -> (Flambda.named -> Flambda.named)
  -> Flambda.t
  -> Flambda.t

val map_expr
   : (Flambda.t -> Flambda.t)
  -> Flambda.t
  -> Flambda.t

val map_named
   : (Flambda.named -> Flambda.named)
  -> Flambda.t
  -> Flambda.t

val map_toplevel
   : (Flambda.t -> Flambda.t)
  -> (Flambda.named -> Flambda.named)
  -> Flambda.t
  -> Flambda.t

val map_toplevel_expr
   : (Flambda.t -> Flambda.t)
  -> Flambda.t
  -> Flambda.t

val map_toplevel_named
   : (Flambda.named -> Flambda.named)
  -> Flambda.t
  -> Flambda.t

val map_symbols
   : Flambda.t
  -> f:(Symbol.t -> Symbol.t)
  -> Flambda.t

val map_symbols_on_set_of_closures
  : Flambda.set_of_closures
  -> f:(Symbol.t -> Symbol.t)
  -> Flambda.set_of_closures

val map_toplevel_sets_of_closures
   : Flambda.t
  -> f:(Flambda.set_of_closures -> Flambda.set_of_closures)
  -> Flambda.t

val map_apply
   : Flambda.t
  -> f:(Flambda.apply -> Flambda.apply)
  -> Flambda.t

val map_function_bodies
   : Flambda.set_of_closures
  -> f:(Flambda.t -> Flambda.t)
  -> Flambda.set_of_closures

val map_sets_of_closures
   : Flambda.t
  -> f:(Flambda.set_of_closures -> Flambda.set_of_closures)
  -> Flambda.t

val map_sets_of_closures_of_program
   : Flambda.program
  -> f:(Flambda.set_of_closures -> Flambda.set_of_closures)
  -> Flambda.program

val map_project_var_to_expr_opt
   : Flambda.t
  -> f:(Flambda.project_var -> Flambda.t option)
  -> Flambda.t

val map_project_var_to_named_opt
   : Flambda.t
  -> f:(Flambda.project_var -> Flambda.named option)
  -> Flambda.t

val map_exprs_at_toplevel_of_program
   : Flambda.program
  -> f:(Flambda.t -> Flambda.t)
  -> Flambda.program

val map_named_of_program
   : Flambda.program
  -> f:(Variable.t -> Flambda.named -> Flambda.named)
  -> Flambda.program

val map_all_immutable_let_and_let_rec_bindings
   : Flambda.t
  -> f:(Variable.t -> Flambda.named -> Flambda.named)
  -> Flambda.t

val fold_function_decls_ignoring_stubs
   : Flambda.set_of_closures
  -> init:'a
  -> f:(fun_var:Variable.t
    -> function_decl:Flambda.function_declaration
    -> 'a
    -> 'a)
  -> 'a
