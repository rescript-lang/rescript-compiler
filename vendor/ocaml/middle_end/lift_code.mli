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

type lifter = Flambda.program -> Flambda.program

(** Lift [let] bindings to attempt to increase the length of scopes, as an
    aid to further optimizations.  For example:
      let c = let b = <expr> in b, b in fst c
    would be transformed to:
      let b = <expr> in let c = b, b in fst c
    which is then clearly just:
      <expr>
*)
val lift_lets : lifter

val lift_lets_expr : Flambda.t -> toplevel:bool -> Flambda.t

(* CR-someday mshinwell: Rename to [bind]?  Also see Flambda_utils.bind. *)
(* [create_body] always receives the variables corresponding to [evaluate]
   in the same order.  However [evaluation_order] specifies in which order
   the (possibly complex) expressions bound to those variables are
   evaluated. *)
val lifting_helper
   : Flambda.t list
  -> evaluation_order:[ `Left_to_right | `Right_to_left ]
  -> create_body:(Variable.t list -> Flambda.t)
  -> name:string
  -> Flambda.t
