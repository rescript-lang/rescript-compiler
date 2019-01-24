(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                        Nicolas Ojeda Bar, LexiFi                       *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Target processor-native integers.

   This module provides operations on the type of
   signed 32-bit integers (on 32-bit target platforms) or
   signed 64-bit integers (on 64-bit target platforms).
   This integer type has exactly the same width as that of a
   pointer type in the C compiler.  All arithmetic operations over
   are taken modulo 2{^32} or 2{^64} depending
   on the word size of the target architecture.
*)

type t
(** The type of target integers. *)

val zero : t
(** The target integer 0.*)

val one : t
(** The target integer 1.*)

val minus_one : t
(** The target integer -1.*)

val neg : t -> t
(** Unary negation. *)

val add : t -> t -> t
(** Addition. *)

val sub : t -> t -> t
(** Subtraction. *)

val mul : t -> t -> t
(** Multiplication. *)

val div : t -> t -> t
(** Integer division.  Raise [Division_by_zero] if the second
   argument is zero.  This division rounds the real quotient of
   its arguments towards zero, as specified for {!Pervasives.(/)}. *)

val rem : t -> t -> t
(** Integer remainder.  If [y] is not zero, the result
   of [Targetint.rem x y] satisfies the following properties:
   [Targetint.zero <= Nativeint.rem x y < Targetint.abs y] and
   [x = Targetint.add (Targetint.mul (Targetint.div x y) y)
                      (Targetint.rem x y)].
   If [y = 0], [Targetint.rem x y] raises [Division_by_zero]. *)

val succ : t -> t
(** Successor.
   [Targetint.succ x] is [Targetint.add x Targetint.one]. *)

val pred : t -> t
(** Predecessor.
   [Targetint.pred x] is [Targetint.sub x Targetint.one]. *)

val abs : t -> t
(** Return the absolute value of its argument. *)

val size : int
(** The size in bits of a target native integer. *)

val max_int : t
(** The greatest representable target integer,
    either 2{^31} - 1 on a 32-bit platform,
    or 2{^63} - 1 on a 64-bit platform. *)

val min_int : t
(** The smallest representable target integer,
   either -2{^31} on a 32-bit platform,
   or -2{^63} on a 64-bit platform. *)

val logand : t -> t -> t
(** Bitwise logical and. *)

val logor : t -> t -> t
(** Bitwise logical or. *)

val logxor : t -> t -> t
(** Bitwise logical exclusive or. *)

val lognot : t -> t
(** Bitwise logical negation. *)

val shift_left : t -> int -> t
(** [Targetint.shift_left x y] shifts [x] to the left by [y] bits.
    The result is unspecified if [y < 0] or [y >= bitsize],
    where [bitsize] is [32] on a 32-bit platform and
    [64] on a 64-bit platform. *)

val shift_right : t -> int -> t
(** [Targetint.shift_right x y] shifts [x] to the right by [y] bits.
    This is an arithmetic shift: the sign bit of [x] is replicated
    and inserted in the vacated bits.
    The result is unspecified if [y < 0] or [y >= bitsize]. *)

val shift_right_logical : t -> int -> t
(** [Targetint.shift_right_logical x y] shifts [x] to the right
    by [y] bits.
    This is a logical shift: zeroes are inserted in the vacated bits
    regardless of the sign of [x].
    The result is unspecified if [y < 0] or [y >= bitsize]. *)

val of_int : int -> t
(** Convert the given integer (type [int]) to a target integer
    (type [t]), module the target word size. *)

val of_int_exn : int -> t
(** Convert the given integer (type [int]) to a target integer
    (type [t]).  Raises a fatal error if the conversion is not exact. *)

val to_int : t -> int
(** Convert the given target integer (type [t]) to an
    integer (type [int]).  The high-order bit is lost during
    the conversion. *)

val of_float : float -> t
(** Convert the given floating-point number to a target integer,
   discarding the fractional part (truncate towards 0).
   The result of the conversion is undefined if, after truncation,
   the number is outside the range
   \[{!Targetint.min_int}, {!Targetint.max_int}\]. *)

val to_float : t -> float
(** Convert the given target integer to a floating-point number. *)

val of_int32 : int32 -> t
(** Convert the given 32-bit integer (type [int32])
    to a target integer. *)

val to_int32 : t -> int32
(** Convert the given target integer to a
    32-bit integer (type [int32]).  On 64-bit platforms,
    the 64-bit native integer is taken modulo 2{^32},
    i.e. the top 32 bits are lost.  On 32-bit platforms,
    the conversion is exact. *)

val of_int64 : int64 -> t
(** Convert the given 64-bit integer (type [int64])
    to a target integer. *)

val to_int64 : t -> int64
(** Convert the given target integer to a
    64-bit integer (type [int64]). *)

val of_string : string -> t
(** Convert the given string to a target integer.
    The string is read in decimal (by default) or in hexadecimal,
    octal or binary if the string begins with [0x], [0o] or [0b]
    respectively.
    Raise [Failure "int_of_string"] if the given string is not
    a valid representation of an integer, or if the integer represented
    exceeds the range of integers representable in type [nativeint]. *)

val to_string : t -> string
(** Return the string representation of its argument, in decimal. *)

val compare: t -> t -> int
(** The comparison function for target integers, with the same specification as
    {!Pervasives.compare}.  Along with the type [t], this function [compare]
    allows the module [Targetint] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

val equal: t -> t -> bool
(** The equal function for target ints. *)

type repr =
  | Int32 of int32
  | Int64 of int64

val repr : t -> repr
(** The concrete representation of a native integer. *)
