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

external new_uninitialized : int -> bytes = "Array"  [@@bs.new]
external to_int_array : bytes -> int array = "%identity"
external of_int_array : int array -> bytes = "%identity"


(***********************)
(* replaced primitives *)
(* Note that we explicitly define [unsafe_set] instead of 
   using {!Bytes.unsafe_set} since for some standard libraries, 
   it might point to ["%string_unsafe_set"]
*)



let caml_string_get s i= 
  if i >=Bs_string.length s || i < 0  then
    raise (Invalid_argument "index out of bounds") 
  else Bs_string.unsafe_get s i





let caml_string_get16 s i = 
  Caml_char.code (Bs_string.unsafe_get s i) + Caml_char.code (Bs_string.unsafe_get s (i+1)) lsl 8  

let caml_string_get32 s i = 
  Caml_char.code (Bs_string.unsafe_get s i) + 
  Caml_char.code (Bs_string.unsafe_get s (i+1)) lsl 8  + 
  Caml_char.code (Bs_string.unsafe_get s (i+2)) lsl 16 + 
  Caml_char.code (Bs_string.unsafe_get s (i+3)) lsl 24

let get s i =
  if i < 0 || i >= Bs_string.length s then
    raise (Invalid_argument "index out of bounds")
  else Bs_string.unsafe_get s i      
