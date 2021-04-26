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
external _E : float = "E" [@@bs.val] [@@bs.scope "Math"]

(** natural logarithm of 2 *)
external _LN2 : float = "LN2" [@@bs.val] [@@bs.scope "Math"]

(** natural logarithm of 10 *)
external _LN10 : float = "LN10" [@@bs.val] [@@bs.scope "Math"]

(** base 2 logarithm of E *)
external _LOG2E : float = "LOG2E" [@@bs.val] [@@bs.scope "Math"]

(** base 10 logarithm of E *)
external _LOG10E : float = "LOG10E" [@@bs.val] [@@bs.scope "Math"]

(** Pi... (ratio of the circumference and diameter of a circle) *)
external _PI : float = "PI" [@@bs.val] [@@bs.scope "Math"]

(** square root of 1/2 *)
external _SQRT1_2 : float = "SQRT1_2" [@@bs.val] [@@bs.scope "Math"]

(** square root of 2 *)
external _SQRT2 : float = "SQRT2" [@@bs.val] [@@bs.scope "Math"]

(** absolute value *)
external abs_int : int -> int = "abs" [@@bs.val] [@@bs.scope "Math"]

(** absolute value *)
external abs_float : float -> float = "abs" [@@bs.val] [@@bs.scope "Math"]

(** arccosine in radians, can return NaN *)
external acos : float -> float = "acos" [@@bs.val] [@@bs.scope "Math"]

(** hyperbolic arccosine in raidans, can return NaN, ES2015 *)
external acosh : float -> float = "acosh" [@@bs.val] [@@bs.scope "Math"]

(** arcsine in radians, can return NaN *)
external asin : float -> float = "asin" [@@bs.val] [@@bs.scope "Math"]

(** hyperbolic arcsine in raidans, ES2015 *)
external asinh : float -> float = "asinh" [@@bs.val] [@@bs.scope "Math"]

(** arctangent in radians *)
external atan : float -> float = "atan" [@@bs.val] [@@bs.scope "Math"]

(** hyperbolic arctangent in radians, can return NaN, ES2015 *)
external atanh : float -> float = "atanh" [@@bs.val] [@@bs.scope "Math"]

(** arctangent of the quotient of x and y, mostly... this one's a bit weird *)
external atan2 : y:float -> x:float -> unit -> float = "atan2" [@@bs.val] [@@bs.scope "Math"]

(** cube root, can return NaN, ES2015 *)
external cbrt : float -> float = "cbrt" [@@bs.val] [@@bs.scope "Math"]

(** may return values not representable by `int` *)
external unsafe_ceil_int : float -> int = "ceil" [@@bs.val] [@@bs.scope "Math"]
let unsafe_ceil = unsafe_ceil_int
[@@deprecated "Please use `unsafe_ceil_int` instead"]

(** smallest int greater than or equal to the argument *)
let ceil_int (f : float) : int =
  if f > Js_int.toFloat Js_int.max then Js_int.max
  else if f < Js_int.toFloat Js_int.min then Js_int.min
  else unsafe_ceil_int f
let ceil = ceil_int
[@@deprecated "Please use `ceil_int` instead"]

(** smallest float greater than or equal to the argument *)
external ceil_float : float -> float = "ceil" [@@bs.val] [@@bs.scope "Math"]

(** number of leading zero bits of the argument's 32 bit int representation, ES2015 *)
external clz32 : int -> int = "clz32" [@@bs.val] [@@bs.scope "Math"]
(* can convert string, float etc. to number *)

(** cosine in radians *)
external cos : float -> float = "cos" [@@bs.val] [@@bs.scope "Math"]

(** hyperbolic cosine in radians, ES2015 *)
external cosh : float -> float = "cosh" [@@bs.val] [@@bs.scope "Math"]

(** natural exponentional *)
external exp : float -> float = "exp" [@@bs.val] [@@bs.scope "Math"]

(** natural exponential minus 1, ES2015 *)
external expm1 : float -> float = "expm1" [@@bs.val] [@@bs.scope "Math"]

(** may return values not representable by `int` *)
external unsafe_floor_int : float -> int = "floor" [@@bs.val] [@@bs.scope "Math"]
let unsafe_floor = unsafe_floor_int
[@@deprecated "Please use `unsafe_floor_int` instead"]

(** largest int greater than or equal to the arugment *)
let floor_int f =
  if f > Js_int.toFloat Js_int.max then Js_int.max
  else if f < Js_int.toFloat Js_int.min then Js_int.min
  else unsafe_floor f
let floor = floor_int
[@@deprecated "Please use `floor_int` instead"]
external floor_float : float -> float = "floor" [@@bs.val] [@@bs.scope "Math"]

(** round to nearest single precision float, ES2015 *)
external fround : float -> float = "fround" [@@bs.val] [@@bs.scope "Math"]

(** pythagorean equation, ES2015 *)
external hypot : float -> float -> float = "hypot" [@@bs.val] [@@bs.scope "Math"]

(** generalized pythagorean equation, ES2015 *)
external hypotMany : float array -> float = "hypot" [@@bs.val] [@@bs.splice] [@@bs.scope "Math"]

(** 32-bit integer multiplication, ES2015 *)
external imul : int -> int -> int = "imul" [@@bs.val] [@@bs.scope "Math"]

(** natural logarithm, can return NaN *)
external log : float -> float = "log" [@@bs.val] [@@bs.scope "Math"]

(** natural logarithm of 1 + the argument, can return NaN, ES2015 *)
external log1p : float -> float = "log1p" [@@bs.val] [@@bs.scope "Math"]

(** base 10 logarithm, can return NaN, ES2015 *)
external log10 : float -> float = "log10" [@@bs.val] [@@bs.scope "Math"]

(** base 2 logarithm, can return NaN, ES2015 *)
external log2 : float -> float = "log2" [@@bs.val] [@@bs.scope "Math"]

(** max value *)
external max_int : int -> int -> int = "max" [@@bs.val] [@@bs.scope "Math"]

(** max value *)
external maxMany_int : int array -> int = "max" [@@bs.val] [@@bs.splice] [@@bs.scope "Math"]

(** max value *)
external max_float : float -> float -> float = "max" [@@bs.val] [@@bs.scope "Math"]

(** max value *)
external maxMany_float : float array -> float = "max" [@@bs.val] [@@bs.splice] [@@bs.scope "Math"]

(** min value *)
external min_int : int -> int -> int = "min" [@@bs.val] [@@bs.scope "Math"]

(** min value *)
external minMany_int : int array -> int = "min" [@@bs.val] [@@bs.splice] [@@bs.scope "Math"]

(** min value *)
external min_float : float -> float -> float = "min" [@@bs.val] [@@bs.scope "Math"]

(** min value *)
external minMany_float : float array -> float = "min" [@@bs.val] [@@bs.splice] [@@bs.scope "Math"]

(** base to the power of the exponent *)
external pow_int : base:int -> exp:int -> int = "pow" [@@bs.val] [@@bs.scope "Math"]
[@@deprecated "use `power_float` instead, the return type may be not int"]

(** base to the power of the exponent *)
external pow_float : base:float -> exp:float -> float = "pow" [@@bs.val] [@@bs.scope "Math"]

(** random number in [0,1) *)
external random : unit -> float = "random" [@@bs.val] [@@bs.scope "Math"]

(** random number in [min,max) *)
let random_int min max =
  floor ((random ()) *. (Js_int.toFloat (max - min))) + min

(** rounds to nearest integer, returns a value not representable as `int` if NaN *)
external unsafe_round : float -> int = "round" [@@bs.val] [@@bs.scope "Math"]

(** rounds to nearest integer *)
external round : float -> float = "round" [@@bs.val] [@@bs.scope "Math"]

(** the sign of the argument, 1, -1 or 0, ES2015 *)
external sign_int : int -> int = "sign" [@@bs.val][@@bs.scope "Math"]

(** the sign of the argument, 1, -1, 0, -0 or NaN, ES2015 *)
external sign_float : float -> float = "sign" [@@bs.val][@@bs.scope "Math"]

(** sine in radians *)
external sin : float -> float = "sin" [@@bs.val][@@bs.scope "Math"]

(** hyperbolic sine in radians, ES2015 *)
external sinh : float -> float = "sinh" [@@bs.val][@@bs.scope "Math"]

(** square root, can return NaN *)
external sqrt : float -> float = "sqrt" [@@bs.val][@@bs.scope "Math"]

(** tangent in radians *)
external tan : float -> float = "tan" [@@bs.val][@@bs.scope "Math"]

(** hyperbolic tangent in radians, ES2015 *)
external tanh : float -> float = "tanh" [@@bs.val][@@bs.scope "Math"]

(** truncate, ie. remove fractional digits, returns a value not representable as `int` if NaN, ES2015 *)
external unsafe_trunc : float -> int = "trunc" [@@bs.val][@@bs.scope "Math"]

(** truncate, ie. remove fractional digits, returns a value not representable as `int` if NaN, ES2015 *)
external trunc : float -> float = "trunc" [@@bs.val][@@bs.scope "Math"]
