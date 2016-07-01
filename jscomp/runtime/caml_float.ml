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






open Typed_array



let caml_int32_float_of_bits (x : int32) =
  let int32 = Int32_array.create [| x |] in 
  let float32 = Float32_array.of_buffer ( Int32_array.buffer int32) in
  Float32_array.get float32 0

let caml_int32_bits_of_float (x : float) = 
  let float32 = Float32_array.create [|x|] in 
  Int32_array.get (Int32_array.of_buffer (Float32_array.buffer float32)) 0 


(**
Math methods available prior to ES6 (ES5 or less)
{[
  abs,
  acos,
  asin,
  atan,
  atan2,
  ceil,
  cos,
  exp,
  floor,
  log,
  max,
  min,
  pow,
  random,
  round,
  sin,
  sqrt,
  tan,
  E, 
  LN10,
  LN2,
  LOG10E,
  LOG2E,
  PI,
  SQRT1_2,
  SQRT2
]}

{[
  acosh,
  asinh,
  atanh,
  cbrt,
  clz32,
  cosh,
  expm1,
  fround,
  hypot,
  imul,
  log10,
  log1p,
  log2,
  sign,
  sinh,
  tanh,
  trunc
]}
*)

let caml_classify_float x : fpclass  = 
  if Js.Float.is_finite x then 
    if abs_float x >= 2.2250738585072014e-308  then
      FP_normal
    else if x <> 0. then FP_subnormal
    else FP_zero
  else 
  if Js.Float.is_nan x then 
    FP_nan 
  else FP_infinite


let caml_modf_float (x : float) : float * float = 
  if Js.Float.is_finite x then 
    let neg = 1. /. x < 0. in 
    let x = abs_float x  in
    let i = floor x in
    let f = x -. i in
    if neg then 
      -. f, -. i       
    else f, i 
  else if Js.Float.is_nan x then Js.Float.nan ,  Js.Float.nan 
  else (1. /. x , x)

let caml_ldexp_float : float ->  int -> float [@fn] = [%bs.raw {| function (x,exp) {
    exp |= 0;
    if (exp > 1023) {
        exp -= 1023;
        x *= Math.pow(2, 1023);
        if (exp > 1023) {  // in case x is subnormal
            exp -= 1023;
            x *= Math.pow(2, 1023);
        }
    }
    if (exp < -1023) {
        exp += 1023;
        x *= Math.pow(2, -1023);
    }
    x *= Math.pow(2, exp);
    return x;
}
|}]



let caml_frexp_float : float -> float * int [@fn]=  [%bs.raw {|function (x) {
    if ((x == 0) || !isFinite(x)) return [ x, 0];
    var neg = x < 0;
    if (neg) x = - x;
    var exp = Math.floor(Math.LOG2E*Math.log(x)) + 1;
    x *= Math.pow(2,-exp);
    if (x < 0.5) { x *= 2; exp -= 1; }
    if (neg) x = - x;
    return [x, exp];
}
|}]

let caml_float_compare (x : float) (y : float ) = 
  if x = y then 0 
  else if x < y then  -1 
  else if x > y then 1 
  else if x = x then 1 
  else if y = y then -1
  else 0

let caml_copysign_float   (x : float) (y :  float) :  float = 
  let x = abs_float x in 
  let y = 
    if y = 0. then 1. /. y else y in 
  if y < 0. then -. x else x   

(* http://www.johndcook.com/blog/cpp_expm1/ *)
let  caml_expm1_float : float -> float = function x -> 
  let y = exp x in 
  let z = y -. 1. in 
  if abs_float x > 1. then z 
  else if z = 0. then x else x *. z /. log y 

(* http://blog.csdn.net/liyuanbhu/article/details/8544644 *)
let caml_log1p_float  : float -> float = function x -> 
  let y = 1. +.  x  in 
  let z =  y -. 1. in 
  if z = 0. then x else x *. log y /. z 


let caml_hypot_float : float ->  float -> float [@fn] = [%bs.raw {| function (x, y) {
    var x0 = Math.abs(x), y0 = Math.abs(y);
    var a = Math.max(x0, y0), b = Math.min(x0,y0) / (a?a:1);
    return a * Math.sqrt(1 + b*b);
}
|}]


let caml_log10_float : float -> float [@fn] =  [%bs.raw {| function  (x) { 
   return Math.LOG10E * Math.log(x); }
|} ]


let caml_cosh_float x = exp x +. exp (-. x) /. 2. 
let caml_sin_float x = exp x -. exp (-. x) /. 2.
let caml_tan_float x = 
  let y = exp x in 
  let z = exp (-. x) in 
  (y +. z) /. (y -. z   )





