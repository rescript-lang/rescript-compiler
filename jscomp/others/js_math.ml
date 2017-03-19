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

(** JavaScript Math API *)

(** Euler's number *)
external _E : float = "Math.E" [@@bs.val]

(** natural logarithm of 2 *)
external _LN2 : float = "Math.LN2" [@@bs.val]

(** natural logarithm of 10 *)
external _LN10 : float = "Math.LN10" [@@bs.val]

(** base 2 logarithm of E *)
external _LOG2E : float = "Math.LOG2E" [@@bs.val]

(** base 10 logarithm of E *)
external _LOG10E : float = "Math.LOG10E" [@@bs.val]

(** Pi... (ratio of the circumference and diameter of a circle) *)
external _PI : float = "Math.PI" [@@bs.val]

(** square root of 1/2 *)
external _SQRT1_2 : float = "Math.SQRT1_2" [@@bs.val]

(** square root of 2 *)
external _SQRT2 : float = "Math.SQRT2" [@@bs.val]

(** absolute value *)
external abs_int : int -> int = "Math.abs" [@@bs.val]
(** absolute value *)
external abs_float : float -> float = "Math.abs" [@@bs.val]

(** arccosine in radians, can return NaN *)
external acos : float -> float = "Math.acos" [@@bs.val]

(** hyperbolic arccosine in raidans, can return NaN, ES2015 *)
external acosh : float -> float = "Math.acosh" [@@bs.val]

(** arcsine in radians, can return NaN *)
external asin : float -> float = "Math.asin" [@@bs.val]

(** hyperbolic arcsine in raidans, ES2015 *)
external asinh : float -> float = "Math.asinh" [@@bs.val]

(** arctangent in radians *)
external atan : float -> float = "Math.atan" [@@bs.val]

(** hyperbolic arctangent in radians, can return NaN, ES2015 *)
external atanh : float -> float = "Math.atanh" [@@bs.val]

(** arctangent of the quotient of x and y, mostly... this one's a bit weird *)
external atan2 : y:float -> x:float -> unit -> float = "Math.atan2" [@@bs.val]

(** cube root, can return NaN, ES2015 *)
external cbrt : float -> float = "Math.cbrt" [@@bs.val]

(** may return values not representable by [int] *)
external unsafe_ceil_int : float -> int = "Math.ceil" [@@bs.val]
let unsafe_ceil = unsafe_ceil_int
[@@ocaml.deprecated "Please use `unsafe_ceil_int` instead"]
(** smallest int greater than or equal to the argument *)
let ceil_int f =
  if f > float max_int then max_int
  else if f < float min_int then min_int
  else unsafe_ceil_int f
let ceil = ceil_int
[@@ocaml.deprecated "Please use `ceil_int` instead"]
(** smallest int greater than or equal to the argument *)
external ceil_float : float -> float = "Math.ceil" [@@bs.val]

(** number of leading zero bits of the argument's 32 bit int representation, ES2015 *)
external clz32 : int -> int = "Math.clz32" [@@bs.val]
(* can convert string, float etc. to number *)

(** cosine in radians *)
external cos : float -> float = "Math.cos" [@@bs.val]

(** hyperbolic cosine in radians, ES2015 *)
external cosh : float -> float = "Math.cosh" [@@bs.val]

(** natural exponentional *)
external exp : float -> float = "Math.exp" [@@bs.val]

(** natural exponential minus 1, ES2015 *)
external expm1 : float -> float = "Math.expm1" [@@bs.val]

(** may return values not representable by [int] *)
external unsafe_floor_int : float -> int = "Math.floor" [@@bs.val]
let unsafe_floor = unsafe_floor_int
[@@ocaml.deprecated "Please use `unsafe_floor_int` instead"]
(** largest int greater than or equal to the arugment *)
let floor_int f =
  if f > float max_int then max_int
  else if f < float min_int then min_int
  else unsafe_floor f
let floor = floor_int
[@@ocaml.deprecated "Please use `floor_int` instead"]
external floor_float : float -> float = "Math.floor" [@@bs.val]

(** round to nearest single precision float, ES2015 *)
external fround : float -> float = "Math.fround" [@@bs.val]

(** pythagorean equation, ES2015 *)
external hypot : float -> float -> float = "Math.hypot" [@@bs.val]

(** generalized pythagorean equation, ES2015 *)
external hypotMany : float array -> float = "Math.hypot" [@@bs.val] [@@bs.splice]

(** 32-bit integer multiplication, ES2015 *)
external imul : int -> int -> int = "Math.imul" [@@bs.val]

(** natural logarithm, can return NaN *)
external log : float -> float = "Math.log" [@@bs.val]

(** natural logarithm of 1 + the argument, can return NaN, ES2015 *)
external log1p : float -> float = "Math.log1p" [@@bs.val]

(** base 10 logarithm, can return NaN, ES2015 *)
external log10 : float -> float = "Math.log10" [@@bs.val]

(** base 2 logarithm, can return NaN, ES2015 *)
external log2 : float -> float = "Math.log2" [@@bs.val]

(** max value *)
external max_int : int -> int -> int = "Math.max" [@@bs.val]
(** max value *)
external maxMany_int : int array -> int = "Math.max" [@@bs.val] [@@bs.splice]
(** max value *)
external max_float : float -> float -> float = "Math.max" [@@bs.val]
(** max value *)
external maxMany_float : float array -> float = "Math.max" [@@bs.val] [@@bs.splice]

(** max value *)
external min_int : int -> int -> int = "Math.min" [@@bs.val]
(** max value *)
external minMany_int : int array -> int = "Math.min" [@@bs.val] [@@bs.splice]
(** max value *)
external min_float : float -> float -> float = "Math.min" [@@bs.val]
(** max value *)
external minMany_float : float array -> float = "Math.min" [@@bs.val] [@@bs.splice]

(** base to the power of the exponent *)
external pow_int : base:int -> exp:int -> int = "Math.pow" [@@bs.val]
(** base to the power of the exponent *)
external pow_float : base:float -> exp:float -> float = "Math.pow" [@@bs.val]

(** random number in \[0,1) *)
external random : unit -> float = "Math.random" [@@bs.val]
(** random number in \[min,max) *)
let random_int min max =
  floor ((random ()) *. (float (max - min))) + min

(** rounds to nearest integer, returns a value not representable as [int] if NaN *)
external unsafe_round : float -> int = "Math.round" [@@bs.val]
(** rounds to nearest integer *)
external round : float -> float = "Math.round" [@@bs.val]

(** the sign of the argument, 1, -1 or 0, ES2015 *)
external sign_int : int -> int = "Math.sign" [@@bs.val]
(** the sign of the argument, 1, -1, 0, -0 or NaN, ES2015 *)
external sign_float : float -> float = "Math.sign" [@@bs.val]

(** sine in radians *)
external sin : float -> float = "Math.sin" [@@bs.val]

(** hyperbolic sine in radians, ES2015 *)
external sinh : float -> float = "Math.sinh" [@@bs.val]

(** square root, can return NaN *)
external sqrt : float -> float = "Math.sqrt" [@@bs.val]

(** tangent in radians *)
external tan : float -> float = "Math.tan" [@@bs.val]

(** hyperbolic tangent in radians, ES2015 *)
external tanh : float -> float = "Math.tanh" [@@bs.val]

(** truncate, ie. remove fractional digits, returns a value not representable as [int] if NaN, ES2015 *)
external unsafe_trunc : float -> int = "Math.trunc" [@@bs.val]
(** truncate, ie. remove fractional digits, returns a value not representable as [int] if NaN, ES2015 *)
external trunc : float -> float = "Math.trunc" [@@bs.val]
