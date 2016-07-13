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

(** Place holder for bindings to js array functions, will be enhanced when stablized *)

type 'a t = 'a array



(** Initial bindings for JS array types 
    Note that we can also create functionl FFI, it's currently experimental
*)
(* class type ['a] _obj = object ('self_type) *)
(*   method length : int  *)
(*   method toString : unit -> string  *)
(*   method toLocaleString : unit -> string  *)
(*   method concat : 'self_type Js.t -> 'self_type Js.t *)
(*   method concat_one : 'a -> 'self_type Js.t  *)
(*   method join : string -> string  *)
(*   method slice : int -> int -> 'self_type Js.t  *)
(*   method slice_copy : unit -> 'self_type Js.t *)
(*   method slice_start : int -> 'self_type Js.t  *)
(*   method indexOf : 'a -> int -> int  *)
(*   method indexOf_start : 'a -> int  *)
(*   method lastIndexOf : 'a -> int -> int  *)
(*   method lastIndexOf_start : 'a -> int  *)
(*   method every : ('a -> int -> Js.boolean [@bs]) -> Js.boolean *)
(*   method some : ('a -> int -> Js.boolean [@bs]) -> Js.boolean *)
(*   method forEach : ('a -> 'int -> unit [@bs]) -> unit  *)
(*   method map : ('a  ->  'b [@bs]) -> 'b _obj Js.t  *)
(*   method map_i : ('a -> int ->  'b [@bs]) -> 'b _obj Js.t  *)
(*   method filter : ('a -> int -> Js.boolean[@bs]) -> 'self_type Js.t *)
(*   method reduce : ('a -> 'a -> int -> 'a [@bs]) ->  'a -> 'a  *)
(* end [@bs] *)

(* type 'a obj = 'a _obj Js.t  *)


(* external as_obj : 'a t -> 'a obj = "%identity" *)
(* external as_val : 'a obj -> 'a t = "%identity" *)

(* external map : 'a t -> ('a -> 'b [@bs]) -> 'b t = "map" [@@bs.send] *)



