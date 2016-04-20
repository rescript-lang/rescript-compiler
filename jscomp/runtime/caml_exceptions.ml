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

(* Author: Hongbo Zhang *)

(** 
   Could be exported for better inlining
   It's common that we have 
   {[ a = caml_set_oo_id([248,"string",0]) ]}
   This can be inlined as 
   {[ a = caml_set_oo_id([248,"tag", caml_oo_last_id++]) ]}
*)

let id = ref 0n


(* see  #251
   {[
     CAMLprim value caml_set_oo_id (value obj) {
       Field(obj, 1) = oo_last_id;
       oo_last_id += 2;
       return obj;
     }

   ]}*)
let caml_set_oo_id (b : Caml_builtin_exceptions.exception_block)  = 
    Obj.set_field (Obj.repr b) 1 (Obj.repr !id);
    id := Nativeint.add !id  1n; 
    b

let get_id () = 
  id := Nativeint.add !id 1n; !id

let create (str : string) : Caml_builtin_exceptions.exception_block = 
  let v = ( str, get_id ()) in 
  Obj.set_tag (Obj.repr v) 248 (* Obj.object_tag*);
  v 
