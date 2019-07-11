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

(**  *)

type 'a t = 'a Caml_undefined_extern.t array

let caml_weak_create n = Caml_array_extern.new_uninitialized n

let caml_weak_set xs i v =
  match v with
  | Some x ->
      Caml_array_extern.unsafe_set xs i (Caml_undefined_extern.return x)
  | None -> ()

let caml_weak_get xs i =
  Caml_undefined_extern.toOption (Caml_array_extern.unsafe_get xs i)

let caml_weak_get_copy xs i =
  match Caml_undefined_extern.toOption (Caml_array_extern.unsafe_get xs i) with
  | None -> None
  | Some x -> Some (Obj.magic (Caml_obj.caml_obj_dup (Caml_obj_extern.repr x)))

let caml_weak_check xs i =
  Caml_array_extern.unsafe_get xs i <> Caml_undefined_extern.empty

let caml_weak_blit = Caml_array.caml_array_blit
