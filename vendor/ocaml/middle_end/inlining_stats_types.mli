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

(* Types used for producing statistics about inlining. *)

module Inlined : sig
  type t =
    | Annotation
    | Decl_local_to_application
    | Without_subfunctions of
        Inlining_cost.Whether_sufficient_benefit.t
    | With_subfunctions of
        Inlining_cost.Whether_sufficient_benefit.t
        * Inlining_cost.Whether_sufficient_benefit.t
end

module Not_inlined : sig
  type t =
    | Classic_mode
    | Above_threshold of int
    | Annotation
    | No_useful_approximations
    | Unrolling_depth_exceeded
    | Self_call
    | Without_subfunctions of
        Inlining_cost.Whether_sufficient_benefit.t
    | With_subfunctions of
        Inlining_cost.Whether_sufficient_benefit.t
        * Inlining_cost.Whether_sufficient_benefit.t
end

module Specialised : sig
  type t =
    | Annotation
    | Without_subfunctions of
        Inlining_cost.Whether_sufficient_benefit.t
    | With_subfunctions of
        Inlining_cost.Whether_sufficient_benefit.t
        * Inlining_cost.Whether_sufficient_benefit.t
end

module Not_specialised : sig
  type t =
    | Classic_mode
    | Above_threshold of int
    | Annotation
    | Not_recursive
    | Not_closed
    | No_invariant_parameters
    | No_useful_approximations
    | Self_call
    | Not_beneficial of
        Inlining_cost.Whether_sufficient_benefit.t
        * Inlining_cost.Whether_sufficient_benefit.t
end

module Prevented : sig
  type t =
    | Function_prevented_from_inlining
    | Level_exceeded
end

module Decision : sig

  type t =
    | Prevented of Prevented.t
    | Specialised of Specialised.t
    | Inlined of Not_specialised.t * Inlined.t
    | Unchanged of Not_specialised.t * Not_inlined.t

  val summary : Format.formatter -> t -> unit
  val calculation : depth:int -> Format.formatter -> t -> unit
end
