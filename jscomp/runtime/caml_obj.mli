(* OCamlScript compiler
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

val caml_obj_dup : Obj.t -> Obj.t

val caml_obj_truncate : Obj.t -> int -> unit

(** 

   TODO: Semantics difference 
   {[
   Obj.is_block (Obj.repr "x");;
   true   
   ]}

   {[
     Obj.is_block (Obj.repr 32.0);;
     true
   ]}   
*)
val caml_obj_is_block : Obj.t -> bool

val caml_lazy_make_forward : 'a -> 'a lazy_t

val caml_update_dummy : Obj.t -> Obj.t -> unit

val caml_int_compare : int -> int -> int
val caml_int32_compare : int -> int -> int
val caml_nativeint_compare : int -> int -> int

val caml_compare : Obj.t -> Obj.t  -> int

type eq = Obj.t -> Obj.t -> bool

val caml_equal : eq
val caml_notequal : eq
val caml_greaterequal : eq
val caml_greaterthan : eq
val caml_lessthan : eq
val caml_lessequal : eq

