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

/** 32-bit integers.

   This module provides operations on the type [t]
   of signed 32-bit integers.  Unlike the built-in [int] type,
   the type [t] is guaranteed to be exactly 32-bit wide on all
   platforms.  All arithmetic operations over [t] are taken
   modulo 2{^32}.

   Performance notice: values of type [t] occupy more memory
   space than values of type [int], and arithmetic operations on
   [t] are generally slower than those on [int].  Use [t]
   only when the application requires exact 32-bit arithmetic. */
/** An alias for the type of 32-bit integers. */
type t = int

/** The 32-bit integer 0. */
let zero: t

/** The 32-bit integer 1. */
let one: t

/** The 32-bit integer -1. */
let minus_one: t

/** Unary negation. */
external neg: t => t = "%negint"

/** Addition. */
external add: (t, t) => t = "%addint"

/** Subtraction. */
external sub: (t, t) => t = "%subint"

/** Multiplication. */
external mul: (t, t) => t = "%mulint"

/** Integer division.  Raise [Division_by_zero] if the second
   argument is zero.  This division rounds the real quotient of
   its arguments towards zero, as specified for {!Pervasives.(/)}. */
external div: (t, t) => t = "%divint"

/** Integer remainder.  If [y] is not zero, the result
   of [Int32.rem x y] satisfies the following property:
   [x = Int32.add (Int32.mul (Int32.div x y) y) (Int32.rem x y)].
   If [y = 0], [Int32.rem x y] raises [Division_by_zero]. */
external rem: (t, t) => t = "%modint"

/** Successor.  [Int32.succ x] is [Int32.add x Int32.one]. */
let succ: t => t

/** Predecessor.  [Int32.pred x] is [Int32.sub x Int32.one]. */
let pred: t => t

/** Return the absolute value of its argument. */
let abs: t => t

/** The greatest representable 32-bit integer, 2{^31} - 1. */
let max_int: t

/** The smallest representable 32-bit integer, -2{^31}. */
let min_int: t

/** Bitwise logical and. */
external logand: (t, t) => t = "%andint"

/** Bitwise logical or. */
external logor: (t, t) => t = "%orint"

/** Bitwise logical exclusive or. */
external logxor: (t, t) => t = "%xorint"

/** Bitwise logical negation. */
let lognot: t => t

/** [Int32.shift_left x y] shifts [x] to the left by [y] bits.
   The result is unspecified if [y < 0] or [y >= 32]. */
external shift_left: (t, int) => t = "%lslint"

/** [Int32.shift_right x y] shifts [x] to the right by [y] bits.
   This is an arithmetic shift: the sign bit of [x] is replicated
   and inserted in the vacated bits.
   The result is unspecified if [y < 0] or [y >= 32]. */
external shift_right: (t, int) => t = "%asrint"

/** [Int32.shift_right_logical x y] shifts [x] to the right by [y] bits.
   This is a logical shift: zeroes are inserted in the vacated bits
   regardless of the sign of [x].
   The result is unspecified if [y < 0] or [y >= 32]. */
external shift_right_logical: (t, int) => t = "%lsrint"

/** Convert the given integer (type [int]) to a 32-bit integer
    (type [t]). */
external of_int: int => t = "%identity"

/** Convert the given 32-bit integer (type [t]) to an
   integer (type [int]).  On 32-bit platforms, the 32-bit integer
   is taken modulo 2{^31}, i.e. the high-order bit is lost
   during the conversion.  On 64-bit platforms, the conversion
   is exact. */
external to_int: t => int = "%identity"

/** Convert the given floating-point number to a 32-bit integer,
   discarding the fractional part (truncate towards 0).
   The result of the conversion is undefined if, after truncation,
   the number is outside the range \[{!Int32.min_int}, {!Int32.max_int}\]. */
external of_float: float => t = "?int_of_float"

/** Convert the given 32-bit integer to a floating-point number. */
external to_float: t => float = "?int_to_float"

/** Convert the given string to a 32-bit integer.
   The string is read in decimal (by default, or if the string 
   begins with [0u]) or in hexadecimal, octal or binary if the
   string begins with [0x], [0o] or [0b] respectively.

   The [0u] prefix reads the input as an unsigned integer in the range
   [[0, 2*Int32.max_int+1]].  If the input exceeds {!Int32.max_int}
   it is converted to the signed integer
   [Int32.min_int + input - Int32.max_int - 1].

   The [_] (underscore) character can appear anywhere in the string
   and is ignored.
   Raise [Failure "Int32.of_string"] if the given string is not
   a valid representation of an integer, or if the integer represented
   exceeds the range of integers representable in type [t]. */
external of_string: string => t = "?int_of_string"

/** Same as [of_string], but return [None] instead of raising.
    @since 4.05 */
let of_string_opt: string => option<t>

/** Return the string representation of its argument, in signed decimal. */
let to_string: t => string

/** Return the internal representation of the given float according
   to the IEEE 754 floating-point 'single format' bit layout.
   Bit 31 of the result represents the sign of the float;
   bits 30 to 23 represent the (biased) exponent; bits 22 to 0
   represent the mantissa. */
external bits_of_float: float => t = "?int_bits_of_float"

/** Return the floating-point number whose internal representation,
   according to the IEEE 754 floating-point 'single format' bit layout,
   is the given [t]. */
external float_of_bits: t => float = "?int_float_of_bits"

/** The comparison function for 32-bit integers, with the same specification as
    {!Pervasives.compare}.  Along with the type [t], this function [compare]
    allows the module [Int32] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. */
let compare: (t, t) => int

/** The equal function for int32s.
    @since 4.03.0 */
let equal: (t, t) => bool

/* {1 Deprecated functions} */

/** Do not use this deprecated function.  Instead,
   used {!Printf.sprintf} with a [%l...] format. */
external format: (string, t) => string = "?format_int"
