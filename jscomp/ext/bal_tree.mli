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

(** Balanced tree based on stdlib distribution *)


type 'a t = 
    | Empty 
    | Node of 'a t * 'a * 'a t * int 
(** this operation is exposed intentionally , so that
    users can whip up a specialized collection quickly
*)

(** operations does not depend on ordering, given the set is already ordered *)

val empty: 'a t

val singleton: 'a -> 'a t


val is_empty: 'a t -> bool

val cardinal: 'a t -> int

val min_elt: 'a t -> 'a
(**  raise [Not_found] if the set is empty. *)

val max_elt: 'a t -> 'a
(** raise [Not_found] *)

val choose: 'a t -> 'a
(** raise [Not_found] if the set is empty. Which element is chosen is unspecified,
    but equal elements will be chosen for equal sets. *)

val elements: 'a t -> 'a list
(** Return the list of all elements of the given set.
    The returned list is sorted in increasing order with respect
*)

val iter: ('a -> unit) -> 'a t -> unit
(** [iter f s] applies [f] in turn to all elements of [s].
    The elements of [s] are presented to [f] in increasing order
    with respect to the ordering over the type of the elements. *)

val fold: ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)],
    where [x1 ... xN] are the elements of [s], in increasing order. *)

val for_all: ('a -> bool) -> 'a t -> bool
(** [for_all p s] checks if all elements of the set
    satisfy the predicate [p]. *)

val exists: ('a -> bool) -> 'a t -> bool

val of_sorted_list : 'a list -> 'a t
val of_sorted_array : 'a array -> 'a t

val internal_bal : 'a t -> 'a -> 'a t -> 'a t 
val internal_merge : 'a t -> 'a t -> 'a t
val internal_join : 'a t -> 'a ->  'a t -> 'a t 
val internal_concat : 'a t -> 'a t -> 'a t   
(** operations depend on ordering *)

val mem: 'a -> 'a t -> bool
(** [mem x s] tests whether [x] belongs to the set [s]. *)

val add: 'a -> 'a t -> 'a t
(** [add x s] returns a set containing all elements of [s],
    plus [x]. If [x] was already in [s], [s] is returned unchanged. *)

val remove: 'a -> 'a t -> 'a t
(** [remove x s] returns a set containing all elements of [s],
    except [x]. If [x] was not in [s], [s] is returned unchanged. *)

val union: 'a t -> 'a t -> 'a t

val inter: 'a t -> 'a t -> 'a t

val diff: 'a t -> 'a t -> 'a t


val compare: 'a t -> 'a t -> int

val equal: 'a t -> 'a t -> bool

val subset: 'a t -> 'a t -> bool

val filter: ('a -> bool) -> 'a t -> 'a t

val partition: ('a -> bool) -> 'a t -> 'a t * 'a t
(** [partition p s] returns a pair of sets [(s1, s2)], where
    [s1] is the set of all the elements of [s] that satisfy the
    predicate [p], and [s2] is the set of all the elements of
    [s] that do not satisfy [p]. *)


val split: 'a -> 'a t -> 'a t * bool * 'a t
(** [split x s] returns a triple [(l, present, r)], where
      [l] is the set of elements of [s] that are
      strictly less than [x];
      [r] is the set of elements of [s] that are
      strictly greater than [x];
      [present] is [false] if [s] contains no element equal to [x],
      or [true] if [s] contains an element equal to [x]. *)

val find: 'a -> 'a t -> 'a
(** [find x s] returns the element of [s] equal to [x] (according
    to [Ord.compare]), or raise [Not_found] if no such element
    exists.
*)

val of_list: 'a list -> 'a t

val of_array : 'a array -> 'a t

val invariant : 'a t -> bool


module Make (Ord : Set.OrderedType) : sig
  type elt = Ord.t
  type t
  val empty: t
  val is_empty: t -> bool
  val iter: (elt -> unit) -> t -> unit
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all: (elt -> bool) -> t -> bool
  val exists: (elt -> bool) -> t -> bool
  val singleton: elt -> t
  val cardinal: t -> int
  val elements: t -> elt list
  val min_elt: t -> elt
  val max_elt: t -> elt
  val choose: t -> elt
  val of_sorted_list : elt list -> t 
  val of_sorted_array : elt array -> t
  val partition: (elt -> bool) -> t -> t * t

  val mem: elt -> t -> bool
  val add: elt -> t -> t
  val remove: elt -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val subset: t -> t -> bool
  val filter: (elt -> bool) -> t -> t
  
  val split: elt -> t -> t * bool * t
  val find: elt -> t -> elt
  val of_list: elt list -> t


end 
