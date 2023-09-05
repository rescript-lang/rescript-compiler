/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Xavier Leroy, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 1996 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */
type t = int
/* Module [t]: 32-bit integers */

external neg: t => t = "%negint"
external add: (t, t) => t = "%addint"
external sub: (t, t) => t = "%subint"
external mul: (t, t) => t = "%mulint"
external div: (t, t) => t = "%divint"
external rem: (t, t) => t = "%modint"
external logand: (t, t) => t = "%andint"
external logor: (t, t) => t = "%orint"
external logxor: (t, t) => t = "%xorint"
external shift_left: (t, int) => t = "%lslint"
external shift_right: (t, int) => t = "%asrint"
external shift_right_logical: (t, int) => t = "%lsrint"
external of_int: int => t = "%identity"
external to_int: t => int = "%identity"
external of_float: float => t = "?int_of_float"
external to_float: t => float = "?int_to_float"
external bits_of_float: float => t = "?int_bits_of_float"
external float_of_bits: t => float = "?int_float_of_bits"

let zero = 0l
let one = 1l
let minus_one = -1l
let succ = n => add(n, 1l)
let pred = n => sub(n, 1l)
let abs = n =>
  if n >= 0l {
    n
  } else {
    neg(n)
  }
let min_int = 0x80000000l
let max_int = 0x7FFFFFFFl
let lognot = n => logxor(n, -1l)

external format: (string, t) => string = "?format_int"
let to_string = n => format("%d", n)

external of_string: string => t = "?int_of_string"

let of_string_opt = s =>
  /* TODO: expose a non-raising primitive directly. */
  try Some(of_string(s)) catch {
  | Failure(_) => None
  }

let compare = (x: t, y: t) => Pervasives.compare(x, y)
let equal = (x: t, y: t) => compare(x, y) == 0
