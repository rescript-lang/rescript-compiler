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

type t = Caml_obj_extern.t

(* external tag : t -> int = "caml_obj_tag" external repr : 'a -> t =
   "%identity" external field : t -> int -> t = "%obj_field" external set_field
   : t -> int -> t -> unit = "%obj_set_field" *)
val caml_obj_block : int -> int -> Caml_obj_extern.t
val caml_obj_dup : Caml_obj_extern.t -> Caml_obj_extern.t
val caml_obj_truncate : Caml_obj_extern.t -> int -> unit
val caml_lazy_make_forward : 'a -> 'a lazy_t
val caml_update_dummy : Caml_obj_extern.t -> Caml_obj_extern.t -> unit
val caml_compare : Caml_obj_extern.t -> Caml_obj_extern.t -> int

type eq = Caml_obj_extern.t -> Caml_obj_extern.t -> bool

val caml_equal : eq
val caml_equal_null : Caml_obj_extern.t -> Caml_obj_extern.t Js.null -> bool

val caml_equal_undefined :
  Caml_obj_extern.t -> Caml_obj_extern.t Js.undefined -> bool

val caml_equal_nullable :
  Caml_obj_extern.t -> Caml_obj_extern.t Js.nullable -> bool

val caml_notequal : eq
val caml_greaterequal : eq
val caml_greaterthan : eq
val caml_lessthan : eq
val caml_lessequal : eq

type 'a selector = 'a -> 'a -> 'a

val caml_min : Caml_obj_extern.t selector
val caml_max : Caml_obj_extern.t selector
val caml_obj_set_tag : Caml_obj_extern.t -> int -> unit
