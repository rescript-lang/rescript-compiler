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

let equal_position x { line; column } = x.line = line && x.column = column

type t = {
  source: File_key.t option;
  start: position;
  _end: position;
}

let none = { source = None; start = { line = 0; column = 0 }; _end = { line = 0; column = 0 } }

let btwn loc1 loc2 = { source = loc1.source; start = loc1.start; _end = loc2._end }


let pos_cmp a b =
  let k = a.line - b.line in
  if k = 0 then
    a.column - b.column
  else
    k



let compare loc1 loc2 =
  let k = File_key.compare_opt loc1.source loc2.source in
  if k = 0 then
    let k = pos_cmp loc1.start loc2.start in
    if k = 0 then
      pos_cmp loc1._end loc2._end
    else
      k
  else
    k

