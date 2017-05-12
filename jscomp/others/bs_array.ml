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

type 'a t = 'a array

(** @param a array 
    @param p predicate 
*)
let filterInPlace p a = 
  let  i = ref 0 in 
  let j = ref 0 in 
  while !i < Array.length a do 
    let v = Array.unsafe_get a !i in 
    if p v   [@bs] then 
      begin 
        Array.unsafe_set a !j v ;
        incr j
      end;
    incr i 
  done;
  ignore @@ Js_array.removeFromInPlace ~pos:!j a 

let empty a  = 
  ignore @@ Js.Array.removeFromInPlace ~pos:0 a

let pushBack x xs = 
  ignore @@ Js.Array.push x xs 

(** Find by JS (===)  equality *)
let memByRef x xs = 
  Js.Array.indexOf x xs >= 0 

let iter f  xs = 
  for i = 0 to Array.length xs - 1 do 
    f (Array.unsafe_get xs i) [@bs]
  done 

(* here [f] is of type ['a -> 'b]
let iterX f  xs = 
  for i = 0 to Array.length xs - 1 do 
    f (Array.unsafe_get xs i) 
  done 
*)
