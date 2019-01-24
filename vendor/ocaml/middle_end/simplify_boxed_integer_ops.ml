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

module S = Simplify_common

(* Simplification of operations on boxed integers (nativeint, Int32, Int64). *)
module Simplify_boxed_integer_operator (I : sig
  type t
  val kind : Lambda.boxed_integer
  val zero : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val shift_right_logical : t -> int -> t
  val to_int : t -> int
  val to_int32 : t -> Int32.t
  val to_int64 : t -> Int64.t
  val neg : t -> t
  val swap : t -> t
  val compare : t -> t -> int
end) : Simplify_boxed_integer_ops_intf.S with type t := I.t = struct
  module A = Simple_value_approx
  module C = Inlining_cost

  let simplify_unop (p : Lambda.primitive) (kind : I.t A.boxed_int)
        expr (n : I.t) =
    let eval op = S.const_boxed_int_expr expr kind (op n) in
    let eval_conv kind op = S.const_boxed_int_expr expr kind (op n) in
    let eval_unboxed op = S.const_int_expr expr (op n) in
    match p with
    | Pintofbint kind when kind = I.kind -> eval_unboxed I.to_int
    | Pcvtbint (kind, Pint32) when kind = I.kind ->
      eval_conv A.Int32 I.to_int32
    | Pcvtbint (kind, Pint64) when kind = I.kind ->
      eval_conv A.Int64 I.to_int64
    | Pnegbint kind when kind = I.kind -> eval I.neg
    | Pbbswap kind when kind = I.kind -> eval I.swap
    | _ -> expr, A.value_unknown Other, C.Benefit.zero

  let simplify_binop (p : Lambda.primitive) (kind : I.t A.boxed_int)
        expr (n1 : I.t) (n2 : I.t) =
    let eval op = S.const_boxed_int_expr expr kind (op n1 n2) in
    let non_zero n = (I.compare I.zero n) <> 0 in
    match p with
    | Paddbint kind when kind = I.kind -> eval I.add
    | Psubbint kind when kind = I.kind -> eval I.sub
    | Pmulbint kind when kind = I.kind -> eval I.mul
    | Pdivbint {size=kind} when kind = I.kind && non_zero n2 -> eval I.div
    | Pmodbint {size=kind} when kind = I.kind && non_zero n2 -> eval I.rem
    | Pandbint kind when kind = I.kind -> eval I.logand
    | Porbint kind when kind = I.kind -> eval I.logor
    | Pxorbint kind when kind = I.kind -> eval I.logxor
    | Pbintcomp (kind, c) when kind = I.kind ->
      S.const_comparison_expr expr c n1 n2
    | _ -> expr, A.value_unknown Other, C.Benefit.zero

  let simplify_binop_int (p : Lambda.primitive) (kind : I.t A.boxed_int)
        expr (n1 : I.t) (n2 : int) ~size_int =
    let eval op = S.const_boxed_int_expr expr kind (op n1 n2) in
    let precond = 0 <= n2 && n2 < 8 * size_int in
    match p with
    | Plslbint kind when kind = I.kind && precond -> eval I.shift_left
    | Plsrbint kind when kind = I.kind && precond -> eval I.shift_right_logical
    | Pasrbint kind when kind = I.kind && precond -> eval I.shift_right
    | _ -> expr, A.value_unknown Other, C.Benefit.zero
end

module Simplify_boxed_nativeint = Simplify_boxed_integer_operator (struct
  include Nativeint
  let to_int64 = Int64.of_nativeint
  let swap = S.swapnative
  let kind = Lambda.Pnativeint
end)

module Simplify_boxed_int32 = Simplify_boxed_integer_operator (struct
  include Int32
  let to_int32 i = i
  let to_int64 = Int64.of_int32
  let swap = S.swap32
  let kind = Lambda.Pint32
end)

module Simplify_boxed_int64 = Simplify_boxed_integer_operator (struct
  include Int64
  let to_int64 i = i
  let swap = S.swap64
  let kind = Lambda.Pint64
end)
