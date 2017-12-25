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


(** *)

(* borrowed from others/js_math.ml *)
external _LOG2E : float = "Math.LOG2E" [@@bs.val]
external _LOG10E : float = "Math.LOG10E" [@@bs.val]
external abs_float : float -> float = "Math.abs" [@@bs.val]
external floor_float : float -> float = "Math.floor" [@@bs.val]
external log : float -> float = "Math.log" [@@bs.val]
external max_float : float -> float -> float = "Math.max" [@@bs.val]
external min_float : float -> float -> float = "Math.min" [@@bs.val]
external pow_float : base:float -> exp:float -> float = "Math.pow" [@@bs.val]

open Js_typed_array

let caml_int32_float_of_bits (x : int32) =
  let int32 = Int32_array.make [| x |] in
  let float32 = Float32_array.fromBuffer ( Int32_array.buffer int32) in
  Float32_array.unsafe_get float32 0

let caml_int32_bits_of_float (x : float) =
  let float32 = Float32_array.make [|x|] in
  Int32_array.unsafe_get (Int32_array.fromBuffer (Float32_array.buffer float32)) 0

let caml_classify_float x : fpclass  =
  if Js_float.isFinite x then
    if abs_float x >= 2.2250738585072014e-308  then
      FP_normal
    else if x <> 0. then FP_subnormal
    else FP_zero
  else
  if Js_float.isNaN x then
    FP_nan
  else FP_infinite


let caml_modf_float (x : float) : float * float =
  if Js_float.isFinite x then
    let neg = 1. /. x < 0. in
    let x = abs_float x  in
    let i = floor x in
    let f = x -. i in
    if neg then
      -. f, -. i
    else f, i
  else if Js_float.isNaN x then Js_float._NaN, Js_float._NaN
  else (1. /. x , x)

let caml_ldexp_float (x: float) (exp: int) : float =
  let x', exp' = ref x, ref (float exp) in
  if !exp' > 1023. then begin
    exp' := !exp' -. 1023.;
    x' := !x' *. pow_float ~base:2. ~exp:1023.;
    if !exp' > 1023. then begin (* in case x is subnormal *)
      exp' := !exp' -. 1023.;
      x' := !x' *. pow_float ~base:2. ~exp:1023.;
    end
  end
  else if !exp' < (-1023.) then begin
    exp' := !exp' +. 1023.;
    x' := !x' *. pow_float ~base:2. ~exp:(-1023.);
  end;
  !x' *. pow_float ~base:2. ~exp:!exp'


let caml_frexp_float (x: float): float * int =
  if x = 0. || not  (Js_float.isFinite x) then
    (x, 0)
  else begin
    let neg = x < 0. in
    let x' = ref (abs_float x) in
    let exp = ref (floor_float (_LOG2E *. log !x') +. 1.) in
    begin
      x' := !x' *. pow_float ~base:2. ~exp:(-.(!exp));
      if !x' < 0.5 then begin
        x' := !x' *. 2.;
        exp := !exp -. 1.;
      end;
      if neg then x' := (-.(!x'));
      (!x', int_of_float (!exp))
    end
  end



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
let caml_log1p_float : float -> float = function x ->
  let y = 1. +.  x  in
  let z =  y -. 1. in
  if z = 0. then x else x *. log y /. z


let caml_hypot_float (x: float) (y: float): float =
  let x0, y0 = abs_float x, abs_float y in
  let a = max_float x0 y0 in
  let b = min_float x0 y0 /. if a <> 0. then a else 1. in
  a *. sqrt (1. +. b *. b)


let caml_log10_float (x: float): float =
   _LOG10E *. log x


let caml_cosh_float x = exp x +. exp (-. x) /. 2.
let caml_sin_float x = exp x -. exp (-. x) /. 2.
let caml_tan_float x =
  let y = exp x in
  let z = exp (-. x) in
  (y +. z) /. (y -. z   )
