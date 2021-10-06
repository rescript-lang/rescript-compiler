(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type explicit_type =
  | Boolean
  | Number
  | String
  | Symbol
[@@immediate]

let compare_explicit_type (x : explicit_type) y = compare x y

let string_of_explicit_type = function
  | Boolean -> "boolean"
  | Number -> "number"
  | String -> "string"
  | Symbol -> "symbol"
