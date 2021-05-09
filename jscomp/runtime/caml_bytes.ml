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


external new_uninitialized : int -> bytes = "Array"  [@@bs.new]

external (.![]) : bytes -> int -> char = "%bytes_unsafe_get"
external (.![]<-) : bytes -> int -> char -> unit = "%bytes_unsafe_set"
external length : bytes -> int = "%bytes_length" 


let set s i ch =
  if i < 0 || i >= length s then  
    raise (Invalid_argument "index out of bounds")
  else s.![i] <- ch 

let get s i =
  if i < 0 || i >= length s then
    raise (Invalid_argument "index out of bounds")
  else s.![i]      


let create len : bytes = 
  (* Node raise [RangeError] exception *)
  if len < 0 then raise (Invalid_argument "String.create")
  else 
    let result = new_uninitialized len in 
    for i = 0 to  len - 1 do 
      result.![i] <- '\000'
    done ;
    result 
  

let rec caml_bytes_compare_aux (s1 : bytes) (s2 : bytes) off len def =   
  if off < len then 
    let a, b = s1.![off], s2.![off] in  
    if a > b then 1
    else if a < b then -1 
    else caml_bytes_compare_aux s1 s2 (off + 1) len def 
  else def   

(* code path could be using a tuple if we can eliminate the tuple allocation for code below
   {[
     let (len, v) = 
       if len1 = len2 then (..,...)
       else (.., .)
   ]}

*)
let caml_bytes_compare (s1 : bytes) (s2 : bytes) : int =  
  let len1, len2 = length s1, length s2 in 
  if len1 = len2 then 
    caml_bytes_compare_aux s1 s2 0 len1 0 
  else if len1 < len2 then   
    caml_bytes_compare_aux s1 s2 0 len1 (-1)
  else
    caml_bytes_compare_aux s1 s2 0 len2 1 

let rec caml_bytes_equal_aux (s1 : bytes) s2 (off : int) len =      
  if off = len then true 
  else 
    let a, b = s1.![off], s2.![off] in  
    a = b
    && caml_bytes_equal_aux s1 s2 (off + 1) len 

let caml_bytes_equal (s1 : bytes) (s2 : bytes) : bool = 
  let len1, len2 = length s1, length s2 in 
  len1 = len2 &&
  caml_bytes_equal_aux s1 s2 0 len1 

let caml_bytes_greaterthan (s1 : bytes) s2 = 
  caml_bytes_compare s1 s2 > 0  

let caml_bytes_greaterequal (s1 : bytes) s2 = 
  caml_bytes_compare s1 s2 >= 0  

let caml_bytes_lessthan (s1 : bytes) s2 = 
  caml_bytes_compare s1 s2 < 0

let caml_bytes_lessequal (s1 : bytes) s2 =    
  caml_bytes_compare s1 s2 <= 0  