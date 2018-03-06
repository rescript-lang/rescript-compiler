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

let object_tag = 248

let create (str : string) : Caml_builtin_exceptions.exception_block = 
  let v = ( str, get_id ()) in 
  Obj.set_tag (Obj.repr v) object_tag;
  v 

(* let makeExtension (str : string) : Caml_builtin_exceptions.exception_block =  *)
(*   let v = ( str, get_id ()) in  *)
(*   Obj.set_tag (Obj.repr v) object_tag; *)
(*   v  *)



(**
   This function should never throw
   It could be either customized exception or built in exception 
   Note due to that in OCaml extensible variants have the same 
   runtime representation as exception, so we can not 
   really tell the difference. 

   However, if we make a false alarm, classified extensible variant 
   as exception, it will be OKAY for nested pattern match

   {[
     match toExn x : exn option with 
     | Some _ 
       -> Js.log "Could be an OCaml exception or an open variant"
     (* If it is an Open variant, it will never pattern match, 
        This is Okay, since exception could never have exhaustive pattern match

     *)
     | None -> Js.log "Not an OCaml exception for sure"
   ]}

   However, there is still something wrong, since if user write such code
   {[
     match toExn x with 
     | Some _ -> (* assert it is indeed an exception *)
       (* This assertion is wrong, since it could be an open variant *)
     | None -> (* assert it is not an exception *)
   ]}

   This is not a problem in `try .. with` since the logic above is not expressible, see more design in [destruct_exn.md]
*)
let isCamlExceptionOrOpenVariant e = 
  if  Obj.magic e = Js.undefined then false 
  else 
    Obj.tag (Obj.repr e) = object_tag  (* nullary exception *)
    ||
    let slot = Obj.field (Obj.repr e) 0 in 
    not (Obj.magic slot = Js.undefined) &&
    (Obj.tag slot = object_tag)


