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

type 'a t = 'a array
type 'a array_like

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
external copyWithin : 'a t -> to_:int -> 'a t = "copyWithin" [@@bs.send] (* ES2015 *)
external copyWithinFrom : 'a t -> to_:int -> from:int -> 'a t = "copyWithin" [@@bs.send] (* ES2015 *)
external copyWithinFromRange : 'a t -> to_:int -> start:int -> end_:int -> 'a t = "copyWithin" [@@bs.send] (* ES2015 *)

external fillInPlace : 'a t -> 'a -> 'a t = "fill" [@@bs.send] (* ES2015 *)
external fillFromInPlace : 'a t -> 'a -> from:int -> 'a t = "fill" [@@bs.send] (* ES2015 *)
external fillRangeInPlace : 'a t -> 'a -> start:int -> end_:int -> 'a t = "fill" [@@bs.send] (* ES2015 *)

(** https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/push *)
external pop : 'a t -> 'a option = "pop" [@@bs.send] [@@bs.return undefined_to_opt]
external push : 'a t -> 'a -> int = "push" [@@bs.send]
external pushMany : 'a t -> 'a array -> int = "push" [@@bs.send] [@@bs.splice]

external reverseInPlace : 'a t -> 'a t = "reverse" [@@bs.send]

external shift : 'a t -> 'a option = "shift" [@@bs.send] [@@bs.return undefined_to_opt]

external sortInPlace : 'a t -> 'a t = "sort" [@@bs.send]
external sortInPlaceWith : 'a t -> ('a -> 'a -> int [@bs.uncurry]) -> 'a t = "sort" [@@bs.send]

external spliceInPlace : 'a t -> pos:int -> remove:int -> add:('a array) -> 'a t = "splice" [@@bs.send] [@@bs.splice]
external removeFromInPlace : 'a t -> pos:int -> 'a t = "splice" [@@bs.send]
external removeCountInPlace : 'a t -> pos:int -> count:int -> 'a t = "splice" [@@bs.send]
(* screwy naming, but screwy function *)

external unshift : 'a t -> 'a -> int = "unshift" [@@bs.send]
external unshiftMany : 'a t -> 'a array -> int = "unshift" [@@bs.send] [@@bs.splice]


(* Accessor functions
*)
external append : 'a t -> 'a -> 'a t = "concat" [@@bs.send]
[@@deprecated "append is not type-safe. Use `concat` instead, and see #1884"]
external concat : 'a t -> 'a t -> 'a t = "concat" [@@bs.send]
external concatMany : 'a t -> 'a t array -> 'a t = "concat" [@@bs.send] [@@bs.splice]

(* TODO: Not available in Node V4  *)
external includes : 'a t -> 'a -> bool = "includes" [@@bs.send] (** ES2016 *)

external indexOf : 'a t -> 'a  -> int = "indexOf" [@@bs.send]
external indexOfFrom : 'a t -> 'a -> from:int -> int = "indexOf" [@@bs.send]


external joinWith : 'a t -> string -> string = "join" [@@bs.send]

external lastIndexOf : 'a t -> 'a -> int = "lastIndexOf" [@@bs.send]
external lastIndexOfFrom : 'a t -> 'a -> from:int -> int = "lastIndexOf" [@@bs.send]

external slice : 'a t -> start:int -> end_:int -> 'a t = "slice" [@@bs.send]
external copy : 'a t -> 'a t = "slice" [@@bs.send]
external sliceFrom : 'a t -> int -> 'a t = "slice" [@@bs.send]

external toString : 'a t -> string = "toString" [@@bs.send]
external toLocaleString : 'a t -> string = "toLocaleString" [@@bs.send]


(* Iteration functions
*)
(* commented out until bs has a plan for iterators
external entries : 'a t -> (int * 'a) array_iter = "" [@@bs.send] (* ES2015 *)
*)

external every : 'a t -> ('a  -> bool[@bs.uncurry]) -> bool = "every" [@@bs.send]
external everyi : 'a t -> ('a -> int -> bool [@bs.uncurry]) -> bool = "every" [@@bs.send]

(** should we use `bool` or `boolean` seems they are intechangeable here *)
external filter : 'a t -> ('a -> bool [@bs.uncurry]) -> 'a t = "filter" [@@bs.send]
external filteri : 'a t -> ('a -> int  -> bool[@bs.uncurry]) -> 'a t = "filter" [@@bs.send]

external find : 'a t -> ('a -> bool [@bs.uncurry]) -> 'a option = "find" [@@bs.send] [@@bs.return {undefined_to_opt}] (* ES2015 *)
external findi : 'a t -> ('a -> int -> bool [@bs.uncurry]) -> 'a option  = "find" [@@bs.send] [@@bs.return {undefined_to_opt}] (* ES2015 *)

external findIndex : 'a t -> ('a -> bool [@bs.uncurry]) -> int = "findIndex" [@@bs.send] (* ES2015 *)
external findIndexi : 'a t -> ('a -> int -> bool [@bs.uncurry]) -> int = "findIndex" [@@bs.send] (* ES2015 *)

external forEach : 'a t -> ('a -> unit [@bs.uncurry]) -> unit = "forEach" [@@bs.send]
external forEachi : 'a t -> ('a -> int -> unit [@bs.uncurry]) -> unit  = "forEach" [@@bs.send]

(* commented out until bs has a plan for iterators
external keys : 'a t -> int array_iter = "" [@@bs.send] (* ES2015 *)
*)

external map : 'a t -> ('a  -> 'b [@bs.uncurry]) -> 'b t = "map" [@@bs.send]
external mapi : 'a t -> ('a -> int ->  'b [@bs.uncurry]) -> 'b t = "map" [@@bs.send]

external reduce : 'a t ->  ('b -> 'a  -> 'b [@bs.uncurry]) -> 'b -> 'b = "reduce" [@@bs.send]
external reducei : 'a t -> ('b -> 'a -> int -> 'b [@bs.uncurry]) -> 'b -> 'b = "reduce" [@@bs.send]

external reduceRight : 'a t ->  ('b -> 'a  -> 'b [@bs.uncurry]) -> 'b -> 'b = "reduceRight" [@@bs.send]
external reduceRighti : 'a t -> ('b -> 'a -> int -> 'b [@bs.uncurry]) -> 'b -> 'b = "reduceRight" [@@bs.send]

external some : 'a t -> ('a  -> bool [@bs.uncurry]) -> bool = "some" [@@bs.send]
external somei : 'a t -> ('a  -> int -> bool [@bs.uncurry]) -> bool = "some" [@@bs.send]

(* commented out until bs has a plan for iterators
external values : 'a t -> 'a array_iter = "" [@@bs.send] (* ES2015 *)
*)
external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"

