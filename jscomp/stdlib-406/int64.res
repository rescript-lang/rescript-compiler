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

/* Module [Int64]: 64-bit integers */

external neg: int64 => int64 = "%int64_neg"
external add: (int64, int64) => int64 = "%int64_add"
external sub: (int64, int64) => int64 = "%int64_sub"
external mul: (int64, int64) => int64 = "%int64_mul"
external div: (int64, int64) => int64 = "%int64_div"
external rem: (int64, int64) => int64 = "%int64_mod"
external logand: (int64, int64) => int64 = "%int64_and"
external logor: (int64, int64) => int64 = "%int64_or"
external logxor: (int64, int64) => int64 = "%int64_xor"
external shift_left: (int64, int) => int64 = "%int64_lsl"
external shift_right: (int64, int) => int64 = "%int64_asr"
external shift_right_logical: (int64, int) => int64 = "%int64_lsr"
external of_int: int => int64 = "%int64_of_int"
external to_int: int64 => int = "%int64_to_int"
external of_float: float => int64 = "?int64_of_float"
external to_float: int64 => float = "?int64_to_float"
external of_int32: int => int64 = "%int64_of_int32"
external to_int32: int64 => int = "%int64_to_int32"

let zero = 0L
let one = 1L
let minus_one = -1L
/* let succ n = add n 1L */
external succ: int64 => int64 = "?int64_succ"
let pred = n => sub(n, 1L)
let abs = n =>
  if n >= 0L {
    n
  } else {
    neg(n)
  }
let min_int = 0x8000000000000000L
let max_int = 0x7FFFFFFFFFFFFFFFL
let lognot = n => logxor(n, -1L)

external format: (string, int64) => string = "?int64_format"
external to_string: int64 => string = "?int64_to_string"

external of_string: string => int64 = "?int64_of_string"

let of_string_opt = s =>
  /* TODO: expose a non-raising primitive directly. */
  try Some(of_string(s)) catch {
  | Failure(_) => None
  }

external bits_of_float: float => int64 = "?int64_bits_of_float"

external float_of_bits: int64 => float = "?int64_float_of_bits"

type t = int64

let compare = (x: t, y: t) => Pervasives.compare(x, y)
let equal = (x: t, y: t) => compare(x, y) == 0
