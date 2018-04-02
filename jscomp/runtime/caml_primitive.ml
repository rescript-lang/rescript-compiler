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


let caml_int_compare (x : int) (y: int) : int =
  if  x < y then -1 else if x = y then 0 else  1
let caml_bool_compare (x : bool) (y : bool): int = 
  match x,y with 
  | true, true | false , false -> 0 
  | true, false -> 1 
  | false, true -> -1

let caml_int32_compare = caml_int_compare
let caml_nativeint_compare = caml_int_compare

let caml_float_compare (x : float) (y : float ) =
  if x = y then 0
  else if x < y then  -1
  else if x > y then 1
  else if x = x then 1
  else if y = y then -1
  else 0

let caml_string_compare (s1 : string) (s2 : string) : int = 
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
let caml_nativeint_min (x : nativeint) y =   
  if x < y then x else y 
let caml_int32_min (x : int32) y   = 
  if x < y then x else y 

let caml_bool_max (x : bool) y : bool =   
  if x then x else y
let caml_int_max (x : int) (y : int) : int =
  if x > y then x else y 
let caml_float_max (x : float) y   =
  if x > y then x else y   
let caml_string_max (x : string) y =     
  if x > y then x else y 
let caml_nativeint_max (x : nativeint) y =   
  if x > y then x else y 
let caml_int32_max (x : int32) y   = 
  if x > y then x else y 




