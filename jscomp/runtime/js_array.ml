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


type 'a t = 'a array

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

(* external concat : 'a t -> 'a t -> 'a t = "concat" [@@bs.send] *)
(* external append : 'a t -> 'a -> 'a t = "concat" [@@bs.send] *)
(* external join : 'a t -> string -> string = "join" [@@bs.send] *)
(* external slice : 'at -> int -> int -> 'a t = "slice" [@@bs.send] *)
(* external copy : 'a t -> 'a t = "slice" [@@bs.send] *)
(* external slice_from : 'a t -> int  -> 'a t  = "slice"[@@bs.send] *)
(* external indexOf : 'a t -> 'a  -> int = "indexOf" [@@bs.send] *)
(* external indexOf_from : 'a t -> 'a -> int -> int = "indexOf" [@@bs.send] *)
(* external lastIndexOf : 'a t -> 'a -> int = "lastIndexOf" [@@bs.send] *)
(* external lastIndexOf_from : 'a t -> 'a -> int -> int = "lastIndexOf" [@@bs.send] *)
(* external every : 'a t ->  ('a -> Js.boolean [@bs]) -> Js.boolean = "every" [@@bs.send] *)
(* external every_i : 'a t -> ('a  -> int -> Js.boolean [@bs]) -> Js.boolean = "every" [@@bs.send] *)
(* external some : 'a t -> ('a -> Js.boolean [@bs]) -> Js.boolean = "some" [@@bs.send] *)
(* external some_i : 'a t -> ('a -> int -> Js.boolean [@bs]) -> Js.boolean = "some" [@@bs.send] *)
(* external forEach : 'a t -> ('a -> unit [@bs]) ->  Js.boolean = "forEach" [@@bs.send] *)
(* external forEach_i : 'a t -> ('a -> int -> unit [@bs]) ->  Js.boolean = "forEach" [@@bs.send] *)
(* external map : 'a t -> ('a -> 'b [@bs]) -> 'b t = "map" [@@bs.send] *)
(* external map_i : 'a t -> ('a -> int ->  'b [@bs]) -> 'b t = "map" [@@bs.send] *)
(* external filter : ('a -> int -> Js.boolean [@bs]) -> 'a t = "filter" [@@bs.send] *)
(* external reduce : ('a -> 'a -> int -> 'a [@bs]) -> 'a -> 'a = "reduce" [@@bs.send] *)

(* external as_obj : 'a t -> 'a obj = "%identity" *)
(* external as_val : 'a obj -> 'a t = "%identity" *)


(* type 'a u  =  *)
(*   <  *)
(*          concat : 'a t -> 'self ; *)
(*          append : 'a  -> 'self  [@bs.name "concat"];  *)
(*          join : string -> 'self ;  *)
(*          slice : int -> int -> 'self ;  *)
(*          copy : unit -> 'self [@bs.name "slice"] ;  *)
(*          slice_from : int -> int -> 'self *)
(*   > as 'self  [@@deriving {ffi_only}] *)

(*
[%%bs.ffi
type 'a u  = 
  < 
         concat : 'a t -> 'self ;
         append : 'a  -> 'self  [@bs.send "concat"]; 
         join : string -> string ; 
         slice : int -> int -> 'self ; 
         copy : unit -> 'self [@bs.send "slice"] ; 
         slice_from : int -> int -> 'self
  > as 'self

]
*)



