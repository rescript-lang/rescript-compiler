(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open Primitive_deriving
type explicit_type =
  | Boolean
  | Number
  | String
  | Symbol
[@@deriving_inline compare]
let _ = fun (_ : explicit_type) -> ()
let compare_explicit_type =
  (Ppx_compare_lib.polymorphic_compare : explicit_type ->
                                           explicit_type -> int)
let _ = compare_explicit_type
[@@@end]
let string_of_explicit_type = function
  | Boolean -> "boolean"
  | Number -> "number"
  | String -> "string"
  | Symbol -> "symbol"
