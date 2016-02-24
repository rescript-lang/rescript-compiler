(* BuckleScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Author: Hongbo Zhang  *)



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
  match kind with 
  | Pgenarray
  | Paddrarray -> E.arr ~comment:"array" mt args 
  | Pintarray  -> E.arr ~comment:"int array" mt args 
  | Pfloatarray -> E.arr ~comment:"float array" mt args

let set_array  e e0 e1 = 
  E.assign (E.access e e0)  e1

let ref_array  e e0 = 
  E.access  e  e0
