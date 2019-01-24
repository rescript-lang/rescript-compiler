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

(** Measurement of the cost (including cost in space) of Flambda terms
    in the context of inlining. *)

module Threshold : sig

  (** The maximum size, in some abstract measure of space cost, that an
     Flambda expression may be in order to be inlined. *)
  type t =
    | Never_inline
    | Can_inline_if_no_larger_than of int

  val add : t -> t -> t
  val sub : t -> t -> t
  val min : t -> t -> t

end

(* Determine whether the given Flambda expression has a sufficiently low space
   cost so as to fit under the given [inlining_threshold].  The [bonus] is
   added to the threshold before evaluation. *)
val can_inline
    : Flambda.t
  -> Threshold.t
  -> bonus:int
  -> bool

(* CR-soon mshinwell for pchambart: I think the name of this function might be
   misleading.  It should probably reflect the functionality it provides,
   not the use to which it is put in another module. *)
(* As for [can_inline], but returns the decision as an inlining threshold.
   If [Never_inline] is returned, the expression was too large for the
   input [inlining_threshold].  Otherwise, [Can_inline_if_no_larger_than] is
   returned, with the constructor argument being the measured estimated size
   of the expression. *)
val can_try_inlining
    : Flambda.t
  -> Threshold.t
  -> number_of_arguments:int
  -> size_from_approximation:int option
  -> Threshold.t

module Benefit : sig
  (* A model of the benefit we gain by removing a particular combination
     of operations.  Such removals are typically performed by inlining (for
     example, [remove_call]) and simplification (for example, [remove_alloc])
     passes. *)

  type t

  val zero : t
  val (+) : t -> t -> t
  val max : round:int -> t -> t -> t

  val remove_call : t -> t
  (* CR-soon mshinwell: [remove_alloc] should take the size of the block
     (to account for removal of initializing writes). *)
  val remove_alloc : t -> t
  val remove_prim : t -> t
  val remove_prims : t -> int -> t
  val remove_branch : t -> t
  val direct_call_of_indirect : t -> t
  val requested_inline : t -> size_of:Flambda.t -> t

  val remove_code : Flambda.t -> t -> t
  val remove_code_named : Flambda.named -> t -> t
  val remove_projection : Projection.t -> t -> t

  val add_code : Flambda.t -> t -> t
  val add_code_named : Flambda.named -> t -> t
  val add_projection : Projection.t -> t -> t

  val print : Format.formatter -> t -> unit
end

module Whether_sufficient_benefit : sig
  (* Evaluation of the benefit of removing certain operations against an
     inlining threshold. *)

  type t

  val create
     : original:Flambda.t
    -> toplevel:bool
    -> branch_depth:int
    -> Flambda.t
    -> benefit:Benefit.t
    -> lifting:bool
    -> round:int
    -> t

  val create_estimate
     : original_size:int
    -> toplevel:bool
    -> branch_depth: int
    -> new_size:int
    -> benefit:Benefit.t
    -> lifting:bool
    -> round:int
    -> t

  val evaluate : t -> bool

  val to_string : t -> string

  val print_description : subfunctions:bool -> Format.formatter -> t -> unit
end

val scale_inline_threshold_by : int

val default_toplevel_multiplier : int

val direct_call_size : int

(** If a function body exceeds this size, we can make a fast decision not
    to inline it (see [Inlining_decision]). *)
val maximum_interesting_size_of_function_body : int -> int

(** Measure the given expression to determine whether its size is at or
    below the given threshold.  [None] is returned if it is too big; otherwise
    [Some] is returned with the measured size. *)
val lambda_smaller' : Flambda.expr -> than:int -> int option

val lambda_size : Flambda.expr -> int
