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

val filterInPlace : ('a -> bool [@bs]) -> 'a t -> unit

val empty : 'a t -> unit

val pushBack : 'a -> 'a t -> unit

val copy : 'a t -> 'a t 
(** shallow copy *)

val memByRef : 'a -> 'a t -> bool

val iter : ('a -> unit [@bs]) -> 'a t -> unit
val iteri : (int -> 'a -> unit [@bs]) -> 'a t -> unit 


(* [@@deprecated "Use Js.List.toVector instead"] *)
(* val ofList : 'a list -> 'a t   *)
(* removed, we choose that {!Js.List} depends on Vector to avoid cylic dependency
*)

val toList : 'a t -> 'a list

val map : ('a -> 'b [@bs]) -> 'a t -> 'b t 
val mapi : (int -> 'a -> 'b [@bs]) -> 'a t -> 'b t 
val foldLeft : ('a -> 'b -> 'a [@bs]) -> 'a -> 'b t -> 'a 
val foldRight : ('b -> 'a -> 'a [@bs]) -> 'b t -> 'a -> 'a 
external length : 'a t -> int = "%array_length"
(** Return the length (number of elements) of the given array. *)

external get : 'a t -> int -> 'a = "%array_safe_get"
(** [Array.get a n] returns the element number [n] of array [a].
   The first element has number 0.
   The last element has number [Array.length a - 1].
   You can also write [a.(n)] instead of [Array.get a n].

   Raise [Invalid_argument "index out of bounds"]
   if [n] is outside the range 0 to [(Array.length a - 1)]. *)

external set : 'a t -> int -> 'a -> unit = "%array_safe_set"
(** [Array.set a n x] modifies array [a] in place, replacing
   element number [n] with [x].
   You can also write [a.(n) <- x] instead of [Array.set a n x].

   Raise [Invalid_argument "index out of bounds"]
   if [n] is outside the range 0 to [Array.length a - 1]. *)


external make : int -> 'a -> 'a t = "caml_make_vect"
(** [Array.make n x] returns a fresh array of length [n],
   initialized with [x].
   All the elements of this new array are initially
   physically equal to [x] (in the sense of the [==] predicate).
   Consequently, if [x] is mutable, it is shared among all elements
   of the array, and modifying [x] through one of the array entries
   will modify all other entries at the same time.

   Raise [Invalid_argument] if [n < 0] or [n > Sys.max_array_length].
   If the value of [x] is a floating-point number, then the maximum
   size is only [Sys.max_array_length / 2].*)


val init : int -> (int -> 'a [@bs]) -> 'a t 
(** @param n size 
    @param fn callback
    @raise RangeError when [n] is negative  *)

val append : 'a -> 'a t -> 'a t
(** [append x a] returns a fresh array with x appended to a *)

external unsafe_get : 'a t -> int -> 'a = "%array_unsafe_get"
external unsafe_set : 'a t -> int -> 'a -> unit = "%array_unsafe_set"
