(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(** 64-bit integers.

   This module provides operations on the type [int64] of
   signed 64-bit integers.  Unlike the built-in [int] type,
   the type [int64] is guaranteed to be exactly 64-bit wide on all
   platforms.  All arithmetic operations over [int64] are taken
   modulo 2{^64}

   Performance notice: values of type [int64] occupy more memory
   space than values of type [int], and arithmetic operations on
   [int64] are generally slower than those on [int].  Use [int64]
   only when the application requires exact 64-bit arithmetic.
*)

val zero : int64
(** The 64-bit integer 0. *)

val one : int64
(** The 64-bit integer 1. *)

val minus_one : int64
(** The 64-bit integer -1. *)

external neg : int64 -> int64 = "%int64_neg"
(** Unary negation. *)

external add : int64 -> int64 -> int64 = "%int64_add"
(** Addition. *)

external sub : int64 -> int64 -> int64 = "%int64_sub"
(** Subtraction. *)

external mul : int64 -> int64 -> int64 = "%int64_mul"
(** Multiplication. *)

external div : int64 -> int64 -> int64 = "%int64_div"
(** Integer division.  Raise [Division_by_zero] if the second
   argument is zero.  This division rounds the real quotient of
   its arguments towards zero, as specified for {!Pervasives.(/)}. *)

external rem : int64 -> int64 -> int64 = "%int64_mod"
(** Integer remainder.  If [y] is not zero, the result
   of [Int64.rem x y] satisfies the following property:
   [x = Int64.add (Int64.mul (Int64.div x y) y) (Int64.rem x y)].
   If [y = 0], [Int64.rem x y] raises [Division_by_zero]. *)

val succ : int64 -> int64
(** Successor.  [Int64.succ x] is [Int64.add x Int64.one]. *)

val pred : int64 -> int64
(** Predecessor.  [Int64.pred x] is [Int64.sub x Int64.one]. *)

val abs : int64 -> int64
(** Return the absolute value of its argument. *)

val max_int : int64
(** The greatest representable 64-bit integer, 2{^63} - 1. *)

val min_int : int64
(** The smallest representable 64-bit integer, -2{^63}. *)

external logand : int64 -> int64 -> int64 = "%int64_and"
(** Bitwise logical and. *)

external logor : int64 -> int64 -> int64 = "%int64_or"
(** Bitwise logical or. *)

external logxor : int64 -> int64 -> int64 = "%int64_xor"
(** Bitwise logical exclusive or. *)

val lognot : int64 -> int64
(** Bitwise logical negation *)

external shift_left : int64 -> int -> int64 = "%int64_lsl"
(** [Int64.shift_left x y] shifts [x] to the left by [y] bits.
   The result is unspecified if [y < 0] or [y >= 64]. *)

external shift_right : int64 -> int -> int64 = "%int64_asr"
(** [Int64.shift_right x y] shifts [x] to the right by [y] bits.
   This is an arithmetic shift: the sign bit of [x] is replicated
   and inserted in the vacated bits.
   The result is unspecified if [y < 0] or [y >= 64]. *)

external shift_right_logical : int64 -> int -> int64 = "%int64_lsr"
(** [Int64.shift_right_logical x y] shifts [x] to the right by [y] bits.
   This is a logical shift: zeroes are inserted in the vacated bits
   regardless of the sign of [x].
   The result is unspecified if [y < 0] or [y >= 64]. *)

external of_int : int -> int64 = "%int64_of_int"
(** Convert the given integer (type [int]) to a 64-bit integer
    (type [int64]). *)

external to_int : int64 -> int = "%int64_to_int"
(** Convert the given 64-bit integer (type [int64]) to an
   integer (type [int]).  On 64-bit platforms, the 64-bit integer
   is taken modulo 2{^63}, i.e. the high-order bit is lost
   during the conversion.  On 32-bit platforms, the 64-bit integer
   is taken modulo 2{^31}, i.e. the top 33 bits are lost
   during the conversion. *)

external of_float : float -> int64 = "caml_int64_of_float"
(** Convert the given floating-point number to a 64-bit integer,
   discarding the fractional part (truncate towards 0).
   The result of the conversion is undefined if, after truncation,
   the number is outside the range \[{!Int64.min_int}, {!Int64.max_int}\]. *)

external to_float : int64 -> float = "caml_int64_to_float"
(** Convert the given 64-bit integer to a floating-point number. *)


external of_int32 : int32 -> int64 = "%int64_of_int32"
(** Convert the given 32-bit integer (type [int32])
   to a 64-bit integer (type [int64]). *)

external to_int32 : int64 -> int32 = "%int64_to_int32"
(** Convert the given 64-bit integer (type [int64]) to a
   32-bit integer (type [int32]). The 64-bit integer
   is taken modulo 2{^32}, i.e. the top 32 bits are lost
   during the conversion.  *)

external of_nativeint : nativeint -> int64 = "%int64_of_nativeint"
(** Convert the given native integer (type [nativeint])
   to a 64-bit integer (type [int64]). *)

external to_nativeint : int64 -> nativeint = "%int64_to_nativeint"
(** Convert the given 64-bit integer (type [int64]) to a
   native integer.  On 32-bit platforms, the 64-bit integer
   is taken modulo 2{^32}.  On 64-bit platforms,
   the conversion is exact. *)

external of_string : string -> int64 = "caml_int64_of_string"
(** Convert the given string to a 64-bit integer.
   The string is read in decimal (by default) or in hexadecimal,
   octal or binary if the string begins with [0x], [0o] or [0b]
   respectively.
   Raise [Failure "int_of_string"] if the given string is not
   a valid representation of an integer, or if the integer represented
   exceeds the range of integers representable in type [int64]. *)

val to_string : int64 -> string
(** Return the string representation of its argument, in decimal. *)

external bits_of_float : float -> int64 = "caml_int64_bits_of_float"
(** Return the internal representation of the given float according
   to the IEEE 754 floating-point 'double format' bit layout.
   Bit 63 of the result represents the sign of the float;
   bits 62 to 52 represent the (biased) exponent; bits 51 to 0
   represent the mantissa. *)

external float_of_bits : int64 -> float = "caml_int64_float_of_bits"
(** Return the floating-point number whose internal representation,
   according to the IEEE 754 floating-point 'double format' bit layout,
   is the given [int64]. *)

type t = int64
(** An alias for the type of 64-bit integers. *)

val compare: t -> t -> int
(** The comparison function for 64-bit integers, with the same specification as
    {!Pervasives.compare}.  Along with the type [t], this function [compare]
    allows the module [Int64] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

(**/**)

(** {6 Deprecated functions} *)

external format : string -> int64 -> string = "caml_int64_format"
(** Do not use this deprecated function.  Instead,
   used {!Printf.sprintf} with a [%L...] format. *)
