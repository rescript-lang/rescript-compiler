(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

[@@@ocaml.text
"\n * Copyright (c) 2017-present, Facebook, Inc.\n *\n * This source code is licensed under the MIT license found in the\n * LICENSE file in the root directory of this source tree.\n "]

type codepoint =
  | Point of int
  | Malformed

type 'a folder = 'a -> int -> codepoint -> 'a

val fold_wtf_8 : ?pos:int -> ?len:int -> 'a folder -> 'a -> string -> 'a

val add_wtf_8 : Buffer.t -> int -> unit
