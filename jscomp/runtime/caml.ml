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


let int_compare (x : int) (y: int) : int =
  if  x < y then -1 else if x = y then 0 else  1
let bool_compare (x : bool) (y : bool): int = 
  match x,y with 
  | true, true | false , false -> 0 
  | true, false -> 1 
  | false, true -> -1



let float_compare (x : float) (y : float ) =
  if x = y then 0
  else if x < y then  -1
  else if x > y then 1
  else if x = x then 1
  else if y = y then -1
  else 0

(* Lexical order *)
let string_compare (s1 : string) (s2 : string) : int = 
  if s1 = s2 then 0 
  else if s1 < s2 then -1
  else 1


type 'a selector = 'a -> 'a -> 'a 

(* could be replaced by [Math.min], but it seems those built-ins are slower *)
let caml_bool_min (x : bool) y : bool =  
  if x then y else x 
let caml_int_min (x : int) (y : int) : int =
  if x < y then x else y 
let caml_float_min (x : float) y   =
  if x < y then x else y   
let caml_string_min (x : string) y =     
  if x < y then x else y 

let caml_bool_max (x : bool) y : bool =   
  if x then x else y
let caml_int_max (x : int) (y : int) : int =
  if x > y then x else y 
let caml_float_max (x : float) y   =
  if x > y then x else y   
let caml_string_max (x : string) y =     
  if x > y then x else y 
type i64 = Caml_int64_extern.t
let i64_eq ( x : i64) (y : i64) = 
  x.lo = y.lo && x.hi = y.hi

let  i64_ge ( {hi; lo } : i64)  ( {hi = other_hi; lo = other_lo}: i64) : bool =
  if hi > other_hi then true
  else if hi < other_hi then false
  else  lo  >=  other_lo

let i64_neq x y = Pervasives.not (i64_eq x y)
let i64_lt x y  = Pervasives.not (i64_ge x y)
let i64_gt ( x : i64) ( y : i64) =
  if x.hi > y.hi then
    true
  else if x.hi < y.hi  then
    false
  else
    x.lo >  y.lo


let i64_le x y = Pervasives.not (i64_gt x y)

let i64_min x y = if i64_lt x  y then x else y 
let i64_max x y = if i64_gt x y then x else y 
