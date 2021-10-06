(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | LibFile of string
  | SourceFile of string
  | JsonFile of string
  | ResourceFile of string
  | Builtins

let to_string = function
  | LibFile x
  | SourceFile x
  | JsonFile x
  | ResourceFile x ->
    x
  | Builtins -> "(global)"

let to_path = function
  | LibFile x
  | SourceFile x
  | JsonFile x
  | ResourceFile x ->
    Ok x
  | Builtins -> Error "File key refers to a builtin"

let compare =
  let order_of_filename = function
    | Builtins -> 1
    | LibFile _ -> 2
    | SourceFile _ -> 3
    | JsonFile _ -> 3
    | ResourceFile _ -> 4
  in
  fun a b ->
    let k = order_of_filename a - order_of_filename b in
    if k <> 0 then
      k
    else
      String.compare (to_string a) (to_string b)

let compare_opt a b =
  match (a, b) with
  | (Some _, None) -> -1
  | (None, Some _) -> 1
  | (None, None) -> 0
  | (Some a, Some b) -> compare a b


