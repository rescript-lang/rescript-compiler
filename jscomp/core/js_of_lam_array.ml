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








(*
   construct array,
   set array,
   ref array,

   Also make sure, don't call any primitive array method, i.e [E.index] 

   We also need check primitive [caml_make_vect], i.e, 
   [Caml_primitive['caml_make_vect']] see if it's correct 

   [caml_make_vect]
   [caml_array_sub]
   [caml_array_append]
   [caml_array_concat]
   [caml_make_float_vect]
   [caml_array_blit]

   research: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Typed_arrays


 *)

module E  = Js_exp_make


(* Parrayref(u|s) *)
let make_array mt (kind : Lambda.array_kind) args = 
  E.array ~comment:"array" mt args 
 
let set_array  e e0 e1 = 
  E.assign (E.access e e0)  e1

let ref_array  e e0 = 
  E.access  e  e0