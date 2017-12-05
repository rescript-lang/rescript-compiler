(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

(** Provides functions for inspecting and manipulating [int]s *)

 (** If we use number, we need coerce to int32 by adding `|0`,
    otherwise `+0` can be wrong.
    Most JS API is float oriented, it may overflow int32 or 
    comes with [NAN]
  *)
(* + conversion*)

(** Formats an [int] using exponential (scientific) notation

{b Returns} a [string] representing the given value in exponential notation

@raise RangeError if digits is not in the range \[0, 20\] (inclusive)

@example {[
(* prints "7.7e+1" *)
let _ = Js.log \@\@ Js.Int.toExponential 77
]}

@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toExponential> MDN
*)
external toExponential : int -> string = "" [@@bs.send]

(** Formats an [int] using exponential (scientific) notation

{b digits} specifies how many digits should appear after the decimal point. The
value must be in the range \[0, 20\] (inclusive).

{b Returns} a [string] representing the given value in exponential notation

The output will be rounded or padded with zeroes if necessary.

@raise RangeError if digits is not in the range \[0, 20\] (inclusive)

@example {[
(* prints "7.70e+1" *)
let _ = Js.log \@\@ Js.Int.toExponentialWithPrecision 77 ~digits:2

(* prints "5.68e+3" *)
let _ = Js.log \@\@ Js.Int.toExponentialWithPrecision 5678 ~digits:2
]}

@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toExponential> MDN
*)
external toExponentialWithPrecision : int -> digits:int -> string = "toExponential" [@@bs.send]

(** Formats a [int] using some fairly arbitrary rules

{b Returns} a [string] representing the given value in fixed-point (usually)

[toPrecision] differs from [toFixed] in that the former will format the number
with full precision, while the latter will not output any digits after the
decimal point.

@raise RangeError if digits is not in the range accepted by this function (what do you mean "vague"?)

@example {[
(* prints "123456789" *)
let _ = Js.log \@\@ Js.Int.toPrecision 123456789
]}

@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toPrecision> MDN
*)
external toPrecision : int -> string = "" [@@bs.send] (* equivalent to `toString` I think *)

(** Formats an [int] using some fairly arbitrary rules

{b digits} specifies how many digits should appear in total. The
value must between 0 and some arbitrary number that's hopefully at least larger
than 20 (for Node it's 21. Why? Who knows).

{b Returns} a [string] representing the given value in fixed-point or scientific notation

The output will be rounded or padded with zeroes if necessary.

[toPrecisionWithPrecision] differs from [toFixedWithPrecision] in that the former
will count all digits against the precision, while the latter will count only
the digits after the decimal point. [toPrecisionWithPrecision] will also use
scientific notation if the specified precision is less than the number for digits
before the decimal point.

@raise RangeError if digits is not in the range accepted by this function (what do you mean "vague"?)

@example {[
(* prints "1.2e+8" *)
let _ = Js.log \@\@ Js.Int.toPrecisionWithPrecision 123456789 ~digits:2

(* prints "0.0" *)
let _ = Js.log \@\@ Js.Int.toPrecisionWithPrecision 0 ~digits:2
]}

@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toPrecision> MDN
*)
external toPrecisionWithPrecision : int -> digits:int -> string = "toPrecision" [@@bs.send]


(** Formats a [int] as a string

{b Returns} a [string] representing the given value in fixed-point (usually)

@example {[
(* prints "123456789" *)
let _ = Js.log \@\@ Js.Int.toString 123456789
]}

@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toString> MDN
*)
external toString : int -> string = "" [@@bs.send]

(** Formats an [int] as a string

{b radix} specifies the radix base to use for the formatted number. The
value must be in the range \[2, 36\] (inclusive).

{b Returns} a [string] representing the given value in fixed-point (usually)

@raise RangeError if radix is not in the range \[2, 36\] (inclusive)

@example {[
(* prints "110" *)
let _ = Js.log \@\@ Js.Int.toStringWithRadix 6 ~radix:2

(* prints "deadbeef" *)
let _ = Js.log \@\@ Js.Int.toStringWithRadix 3735928559 ~radix:16

(* prints "2n9c" *)
let _ = Js.log \@\@ Js.Int.toStringWithRadix 123456 ~radix:36
]}

@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toString> MDN
*)
external toStringWithRadix : int -> radix:int -> string = "toString" [@@bs.send]

let equal (x: int) y = x = y

let max : int = 2147483647

let min : int = -2147483648
