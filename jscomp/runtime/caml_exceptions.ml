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

(** It could be either customized exception or built in exception *)
let isCamlException e = 
  let slot = Obj.field e 0 in 
  not (Js.Undefined.testAny slot) &&
  (Obj.tag slot = 248
   || (Js.typeof (Obj.field slot 0) == "string"))
