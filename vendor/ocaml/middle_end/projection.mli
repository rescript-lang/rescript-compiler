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

(** Representation of projections from closures and blocks. *)

(** The selection of one closure given a set of closures, required before
    a function defined by said set of closures can be applied.  See more
    detailed documentation below on [set_of_closures]. *)
type project_closure = {
  set_of_closures : Variable.t; (** must yield a set of closures *)
  closure_id : Closure_id.t;
}

(** The selection of one closure given another closure in the same set of
    closures.  See more detailed documentation below on [set_of_closures].
    The [move_to] closure must be part of the free variables of
    [start_from]. *)
type move_within_set_of_closures = {
  closure : Variable.t;  (** must yield a closure *)
  start_from : Closure_id.t;
  move_to : Closure_id.t;
}

(** The selection from a closure of a variable bound by said closure.
    In other words, access to a function's environment.  Also see more
    detailed documentation below on [set_of_closures]. *)
type project_var = {
  closure : Variable.t;  (** must yield a closure *)
  closure_id : Closure_id.t;
  var : Var_within_closure.t;
}

val print_project_closure
   : Format.formatter
  -> project_closure
  -> unit

val print_move_within_set_of_closures
   : Format.formatter
  -> move_within_set_of_closures
  -> unit

val print_project_var
   : Format.formatter
  -> project_var
  -> unit

val compare_project_var : project_var -> project_var -> int
val compare_project_closure : project_closure -> project_closure -> int
val compare_move_within_set_of_closures
   : move_within_set_of_closures
  -> move_within_set_of_closures
  -> int

type t =
  | Project_var of project_var
  | Project_closure of project_closure
  | Move_within_set_of_closures of move_within_set_of_closures
  | Field of int * Variable.t

include Identifiable.S with type t := t

(** Return which variable the given projection projects from. *)
val projecting_from : t -> Variable.t

(** Change the variable that the given projection projects from. *)
val map_projecting_from : t -> f:(Variable.t -> Variable.t) -> t
