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

external toString : unit -> string  = "" [@@bs.send.pipe: 'a t as 'this]
external toLocaleString : unit -> string  = "" [@@bs.send.pipe: 'a t as 'this]
external concat : 'this -> 'this  = "" [@@bs.send.pipe: 'a t as 'this]
external append : 'a -> 'this = "concat" [@@bs.send.pipe: 'a t as 'this]

external slice : int -> int -> 'this = "" [@@bs.send.pipe: 'a t as 'this]
external slice_copy : unit -> 'this = "slice"[@@bs.send.pipe: 'a t as 'this]
external slice_start : int -> 'this = "slice"[@@bs.send.pipe: 'a t as 'this]

external indexOf : 'a  -> int = "" [@@bs.send.pipe: 'a t as 'this]
external indexOfFrom : 'a -> int ->  int = "indexOf" [@@bs.send.pipe: 'a t as 'this]

(* TODO: Not available in Node V4  *)
external includes : 'a -> Js.boolean = "" [@@bs.send.pipe: 'a t as 'this]

external lastIndexOf : 'a -> int -> int = "" [@@bs.send.pipe: 'a t as 'this]
external lastIndexOf_start : 'a -> int  = "lastIndex" [@@bs.send.pipe: 'a t as 'this]

external every : ('a  -> Js.boolean [@bs]) -> Js.boolean = "" [@@bs.send.pipe: 'a t as 'this]
external everyi : ('a -> int -> Js.boolean [@bs]) -> Js.boolean = "every" [@@bs.send.pipe: 'a t as 'this]

external some : ('a  -> Js.boolean [@bs]) -> Js.boolean = "" [@@bs.send.pipe: 'a t as 'this]
external somei : ('a  -> int -> Js.boolean [@bs]) -> Js.boolean = "some" [@@bs.send.pipe: 'a t as 'this]


external forEach : ('a -> unit [@bs]) -> unit  = "" [@@bs.send.pipe: 'a t as 'this]
external forEachi : ('a -> int -> unit [@bs]) -> unit  = "forEach" [@@bs.send.pipe: 'a t as 'this]

external map : ('a  ->  'b [@bs]) -> 'b t  = "" [@@bs.send.pipe: 'a t as 'this]
external mapi : ('a -> int ->  'b [@bs]) -> 'b t = "map" [@@bs.send.pipe: 'a t as 'this]

(** should we use [bool] or [boolan] seems they are intechangeable here *)
external filter : ('a  -> bool [@bs]) -> 'this = "" [@@bs.send.pipe: 'a t as 'this]
external filteri : ('a -> int  -> Js.boolean[@bs]) -> 'this = "filter" [@@bs.send.pipe: 'a t as 'this]

external reducei : ('a -> 'a -> int -> 'a [@bs]) ->  'a -> 'a = "reduce" [@@bs.send.pipe: 'a t as 'this]
external reduce :  ('a -> 'a  -> 'a [@bs]) ->  'a -> 'a = "reduce" [@@bs.send.pipe: 'a t as 'this]

external isArray : 'a -> Js.boolean = "Array.isArray" [@@bs.val]

(** https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/push *)    
external push : 'a array  -> 'a -> int = ""
[@@bs.send]

external pop : 'a array -> 'a Js.undefined = ""
[@@bs.send]
   
external length : 'a array -> int = ""
[@@bs.get]
   
