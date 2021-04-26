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

(** JavaScript Array API *)

[@@@warning "-103"]

type 'a t = 'a array
type 'a array_like = 'a Js_array2.array_like

(* commented out until bs has a plan for iterators
type 'a array_iter = 'a array_like
*)

external from : 'a array_like -> 'a array = "Array.from" [@@bs.val] (* ES2015 *)
external fromMap : 'a array_like -> ('a -> 'b [@bs.uncurry]) -> 'b array = "Array.from" [@@bs.val] (* ES2015 *)
external isArray : 'a -> bool = "Array.isArray" [@@bs.val] (* ES2015 *)
(* Array.of: seems pointless unless you can bind *) (* ES2015 *)

external length : 'a array -> int = "length" [@@bs.get]


(* Mutator functions
*)
external copyWithin : to_:int -> 'this = "copyWithin" [@@bs.send.pipe: 'a t as 'this] (* ES2015 *)
external copyWithinFrom : to_:int -> from:int -> 'this = "copyWithin" [@@bs.send.pipe: 'a t as 'this] (* ES2015 *)
external copyWithinFromRange : to_:int -> start:int -> end_:int -> 'this = "copyWithin" [@@bs.send.pipe: 'a t as 'this] (* ES2015 *)

external fillInPlace : 'a -> 'this = "fill" [@@bs.send.pipe: 'a t as 'this] (* ES2015 *)
external fillFromInPlace : 'a -> from:int -> 'this = "fill" [@@bs.send.pipe: 'a t as 'this] (* ES2015 *)
external fillRangeInPlace : 'a -> start:int -> end_:int -> 'this = "fill" [@@bs.send.pipe: 'a t as 'this] (* ES2015 *)

(** https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/push *)
external pop : 'a option = "pop" [@@bs.send.pipe: 'a t as 'this] [@@bs.return undefined_to_opt]
external push : 'a -> int = "push" [@@bs.send.pipe: 'a t as 'this]
external pushMany : 'a array -> int = "push" [@@bs.send.pipe: 'a t as 'this] [@@bs.splice]

external reverseInPlace : 'this = "reverse" [@@bs.send.pipe: 'a t as 'this]

external shift : 'a option = "shift" [@@bs.send.pipe: 'a t as 'this] [@@bs.return {undefined_to_opt}]

external sortInPlace : 'this = "sort" [@@bs.send.pipe: 'a t as 'this]
external sortInPlaceWith : ('a -> 'a -> int [@bs.uncurry]) -> 'this = "sort" [@@bs.send.pipe: 'a t as 'this]

external spliceInPlace : pos:int -> remove:int -> add:('a array) -> 'this = "splice" [@@bs.send.pipe: 'a t as 'this] [@@bs.splice]
external removeFromInPlace : pos:int -> 'this = "splice" [@@bs.send.pipe: 'a t as 'this]
external removeCountInPlace : pos:int -> count:int -> 'this = "splice" [@@bs.send.pipe: 'a t as 'this]
(* screwy naming, but screwy function *)

external unshift : 'a -> int = "unshift" [@@bs.send.pipe: 'a t as 'this]
external unshiftMany : 'a array -> int = "unshift" [@@bs.send.pipe: 'a t as 'this] [@@bs.splice]


(* Accessor functions
*)
external append : 'a -> 'this = "concat" [@@bs.send.pipe: 'a t as 'this]
[@@deprecated "append is not type-safe. Use `concat` instead, and see #1884"]
external concat : 'this -> 'this = "concat" [@@bs.send.pipe: 'a t as 'this]
external concatMany : 'this array -> 'this = "concat" [@@bs.send.pipe: 'a t as 'this] [@@bs.splice]

(* TODO: Not available in Node V4  *)
external includes : 'a -> bool = "includes" [@@bs.send.pipe: 'a t as 'this] (** ES2016 *)

external indexOf : 'a  -> int = "indexOf" [@@bs.send.pipe: 'a t as 'this]
external indexOfFrom : 'a -> from:int -> int = "indexOf" [@@bs.send.pipe: 'a t as 'this]

external join : 'a t -> string = "join" 
[@@bs.send] [@@deprecated "please use joinWith instead"]

external joinWith : string -> string = "join" [@@bs.send.pipe: 'a t as 'this]

external lastIndexOf : 'a -> int = "lastIndexOf" [@@bs.send.pipe: 'a t as 'this]
external lastIndexOfFrom : 'a -> from:int -> int = "lastIndexOf" [@@bs.send.pipe: 'a t as 'this]
external lastIndexOf_start : 'a -> int = "lastIndexOf" [@@bs.send.pipe: 'a t as 'this]
[@@deprecated "Please use `lastIndexOf"]

external slice : start:int -> end_:int -> 'this = "slice" [@@bs.send.pipe: 'a t as 'this]
external copy : 'this = "slice" [@@bs.send.pipe: 'a t as 'this]
external slice_copy : unit -> 'this = "slice" [@@bs.send.pipe: 'a t as 'this]
[@@deprecated "Please use `copy`"]
external sliceFrom : int -> 'this = "slice" [@@bs.send.pipe: 'a t as 'this]
external slice_start : int -> 'this = "slice" [@@bs.send.pipe: 'a t as 'this]
[@@deprecated "Please use `sliceFrom`"]

external toString : string = "toString" [@@bs.send.pipe: 'a t as 'this]
external toLocaleString : string = "toLocaleString" [@@bs.send.pipe: 'a t as 'this]


(* Iteration functions
*)
(* commented out until bs has a plan for iterators
external entries : (int * 'a) array_iter = "" [@@bs.send.pipe: 'a t as 'this] (* ES2015 *)
*)

external every : ('a  -> bool[@bs.uncurry]) -> bool = "every" [@@bs.send.pipe: 'a t as 'this]
external everyi : ('a -> int -> bool [@bs.uncurry]) -> bool = "every" [@@bs.send.pipe: 'a t as 'this]

(** should we use `bool` or `boolean` seems they are intechangeable here *)
external filter : ('a -> bool [@bs.uncurry]) -> 'this = "filter" [@@bs.send.pipe: 'a t as 'this]
external filteri : ('a -> int  -> bool[@bs.uncurry]) -> 'this = "filter" [@@bs.send.pipe: 'a t as 'this]

external find : ('a -> bool [@bs.uncurry]) -> 'a option = "find" [@@bs.send.pipe: 'a t as 'this] [@@bs.return {undefined_to_opt}] (* ES2015 *)
external findi : ('a -> int -> bool [@bs.uncurry]) -> 'a option  = "find" [@@bs.send.pipe: 'a t as 'this] [@@bs.return {undefined_to_opt}] (* ES2015 *)

external findIndex : ('a -> bool [@bs.uncurry]) -> int = "findIndex" [@@bs.send.pipe: 'a t as 'this] (* ES2015 *)
external findIndexi : ('a -> int -> bool [@bs.uncurry]) -> int = "findIndex" [@@bs.send.pipe: 'a t as 'this] (* ES2015 *)

external forEach : ('a -> unit [@bs.uncurry]) -> unit = "forEach" [@@bs.send.pipe: 'a t as 'this]
external forEachi : ('a -> int -> unit [@bs.uncurry]) -> unit  = "forEach" [@@bs.send.pipe: 'a t as 'this]

(* commented out until bs has a plan for iterators
external keys : int array_iter = "" [@@bs.send.pipe: 'a t as 'this] (* ES2015 *)
*)

external map : ('a  -> 'b [@bs.uncurry]) -> 'b t = "map" [@@bs.send.pipe: 'a t as 'this]
external mapi : ('a -> int ->  'b [@bs.uncurry]) -> 'b t = "map" [@@bs.send.pipe: 'a t as 'this]

external reduce :  ('b -> 'a  -> 'b [@bs.uncurry]) -> 'b -> 'b = "reduce" [@@bs.send.pipe: 'a t as 'this]
external reducei : ('b -> 'a -> int -> 'b [@bs.uncurry]) -> 'b -> 'b = "reduce" [@@bs.send.pipe: 'a t as 'this]

external reduceRight :  ('b -> 'a  -> 'b [@bs.uncurry]) -> 'b -> 'b = "reduceRight" [@@bs.send.pipe: 'a t as 'this]
external reduceRighti : ('b -> 'a -> int -> 'b [@bs.uncurry]) -> 'b -> 'b = "reduceRight" [@@bs.send.pipe: 'a t as 'this]

external some : ('a  -> bool [@bs.uncurry]) -> bool = "some" [@@bs.send.pipe: 'a t as 'this]
external somei : ('a  -> int -> bool [@bs.uncurry]) -> bool = "some" [@@bs.send.pipe: 'a t as 'this]

(* commented out until bs has a plan for iterators
external values : 'a array_iter = "" [@@bs.send.pipe: 'a t as 'this] (* ES2015 *)
*)
external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"

