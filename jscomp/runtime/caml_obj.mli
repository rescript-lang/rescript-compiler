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
val caml_obj_block : int -> int -> Obj.t
val caml_obj_dup : Obj.t -> Obj.t

val caml_obj_truncate : Obj.t -> int -> unit



val caml_lazy_make_forward : 'a -> 'a lazy_t

val caml_update_dummy : Obj.t -> Obj.t -> unit


val caml_compare : Obj.t -> Obj.t  -> int

type eq = Obj.t -> Obj.t -> bool

val caml_equal : eq

val caml_equal_null : Obj.t -> Obj.t Js.null -> bool 
val caml_equal_undefined : Obj.t -> Obj.t Js.undefined -> bool 
val caml_equal_nullable : Obj.t -> Obj.t Js.nullable -> bool 

val caml_notequal : eq
val caml_greaterequal : eq
val caml_greaterthan : eq
val caml_lessthan : eq
val caml_lessequal : eq

type 'a selector = 'a -> 'a -> 'a 


val caml_min : Obj.t selector
val caml_max : Obj.t selector