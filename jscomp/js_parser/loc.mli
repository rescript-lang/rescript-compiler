(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type position = {
  line: int;
  column: int;
}

val equal_position : position -> position -> bool

type t = {
  source: File_key.t option;
  start: position;
  _end: position;
}

val none : t

val btwn : t -> t -> t


val pos_cmp : position -> position -> int

val compare : t -> t -> int

