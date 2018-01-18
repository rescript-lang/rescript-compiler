(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(** Array operations. *)

external length : 'a array -> int = "%array_length"
(** Return the length (number of elements) of the given array. *)

external get : 'a array -> int -> 'a = "%array_safe_get"
(** [Array.get a n] returns the element number [n] of array [a].
   The first element has number 0.
   The last element has number [Array.length a - 1].
   You can also write [a.(n)] instead of [Array.get a n].

   Raise [Invalid_argument "index out of bounds"]
   if [n] is outside the range 0 to [(Array.length a - 1)]. *)

external set : 'a array -> int -> 'a -> unit = "%array_safe_set"
(** [Array.set a n x] modifies array [a] in place, replacing
   element number [n] with [x].
   You can also write [a.(n) <- x] instead of [Array.set a n x].

   Raise [Invalid_argument "index out of bounds"]
   if [n] is outside the range 0 to [Array.length a - 1]. *)


external makeUninitialized : int -> 'a Js.undefined array = "Array" [@@bs.new]
external makeUninitializedUnsafe : int -> 'a array = "Array" [@@bs.new]

val init : int -> (int -> 'a [@bs]) -> 'a array

val shuffleDone : 'a array -> unit    

val shuffle :'a array -> 'a array 
(** [shuffle xs] it mutates [xs] and return
    [xs] for chaining
 *)
val zip : 'a array -> 'b array -> ('a * 'b) array
(** [zip a b] stop with the shorter array *)

val makeMatrix : int -> int -> 'a -> 'a array array


val append : 'a array -> 'a array -> 'a array
(** Note it returns a fresh array containing the
    concatenation of the arrays [v1] and [v2], so even if [v1] or [v2]
    is empty, it can not be shared 
*)

val concat : 'a array list -> 'a array
(** Same as [Array.append], but concatenates a list of arrays. *)

val sub : 'a array -> int -> int -> 'a array


val copy : 'a array -> 'a array
(** [Array.copy a] returns a copy of [a], that is, a fresh array
   containing the same elements as [a]. *)

val fill : 'a array -> int -> int -> 'a -> unit
(** [Array.fill a ofs len x] modifies the array [a] in place,
   storing [x] in elements number [ofs] to [ofs + len - 1].

   Raise [Invalid_argument "Array.fill"] if [ofs] and [len] do not
   designate a valid subarray of [a]. *)

val blit : 
    'a array -> int -> 'a array -> int -> int -> unit
(** [blit v1 o1 v2 o2 len] copies [len] elements
   from array [v1], starting at element number [o1], to array [v2],
   starting at element number [o2]. It works correctly even if
   [v1] and [v2] are the same array, and the source and
   destination chunks overlap.

   Raise [Invalid_argument "Array.blit"] if [o1] and [len] do not
   designate a valid subarray of [v1], or if [o2] and [len] do not
   designate a valid subarray of [v2]. *)
external blitUnsafe :
  'a array -> int -> 'a array -> int -> int -> unit = "caml_array_blit"

val toList : 'a array -> 'a list


val ofList : 'a list -> 'a array

val iter : 'a array ->  ('a -> unit [@bs]) -> unit

val map : 'a array ->  ('a -> 'b [@bs]) -> 'b array

val iteri : 'a array ->  (int -> 'a -> unit [@bs]) -> unit

val mapi : 'a array ->  (int -> 'a -> 'b [@bs]) -> 'b array

val foldLeft :  'b array -> 'a -> ('a -> 'b -> 'a [@bs]) ->'a

val foldRight : 'b array -> 'a -> ('a -> 'b ->  'a [@bs]) ->  'a

val forAll: 'a array -> ('a -> bool [@bs]) -> bool

(** [forAll2 a b] return false when [length a <> length b] *)
val forAll2: 'a array -> 'b array -> ('a -> 'b -> bool [@bs]) -> bool

external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"
external truncateToLengthUnsafe : 'a array -> int ->  unit = "length" [@@bs.set]