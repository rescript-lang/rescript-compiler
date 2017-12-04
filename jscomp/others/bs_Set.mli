


type ('elt, 'id) t0
(** The type of sets. *)

val empty0: ('elt, 'id) t0
(** The empty set. *)

val is_empty0: ('elt, 'id) t0 -> bool
(** Test whether a set is empty or not. *)

val mem0: 
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  'elt -> ('elt, 'id) t0 -> bool
(** [mem x s] tests whether [x] belongs to the set [s]. *)

val add0: 
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  'elt -> ('elt, 'id) t0 -> ('elt, 'id) t0
(** [add x s] returns a set containing all elements of [s],
    plus [x]. If [x] was already in [s], [s] is returned unchanged. *)

val singleton0: 'elt -> ('elt, 'id) t0
(** [singleton x] returns the one-element set containing only [x]. *)

val remove0: 
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  'elt -> ('elt, 'id) t0 -> ('elt, 'id) t0
(** [remove x s] returns a set containing all elements of [s],
    except [x]. If [x] was not in [s], [s] is returned unchanged. *)

val union0: 
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  ('elt, 'id) t0 -> ('elt, 'id) t0 -> ('elt, 'id) t0
(** Set union. *)

val inter0: 
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  ('elt, 'id) t0 -> ('elt, 'id) t0 -> ('elt, 'id) t0
(** Set intersection. *)

val diff0: 
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  ('elt, 'id) t0 -> ('elt, 'id) t0 -> ('elt, 'id) t0
(** Set difference. *)

val compare0: 
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  ('elt, 'id) t0 -> ('elt, 'id) t0 -> int
(** Total ordering between sets. Can be used as the ordering function
    for doing sets of sets. *)

val equal0: 
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  ('elt, 'id) t0 -> ('elt, 'id) t0 -> bool
(** [equal s1 s2] tests whether the sets [s1] and [s2] are
    equal, that is, contain equal elements. *)

val subset0: 
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  ('elt, 'id) t0 -> ('elt, 'id) t0 -> bool
(** [subset s1 s2] tests whether the set [s1] is a subset of
    the set [s2]. *)

val iter0: ('elt -> unit [@bs]) -> ('elt, 'id) t0 -> unit
(** [iter f s] applies [f] in turn to all elements of [s].
    The elements of [s] are presented to [f] in increasing order
    with respect to the ordering over the type of the elements. *)

val fold0: ('elt -> 'a -> 'a [@bs]) -> ('elt, 'id) t0 -> 'a -> 'a
(** [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)],
    where [x1 ... xN] are the elements of [s], in increasing order. *)

val for_all0: ('elt -> bool [@bs]) -> ('elt, 'id) t0 -> bool
(** [for_all p s] checks if all elements of the set
    satisfy the predicate [p]. *)

val exists0: ('elt -> bool [@bs]) -> ('elt, 'id) t0 -> bool
(** [exists p s] checks if at least one element of
    the set satisfies the predicate [p]. *)

val filter0: ('elt -> bool [@bs]) -> ('elt, 'id) t0 -> ('elt, 'id) t0
(** [filter p s] returns the set of all elements in [s]
    that satisfy predicate [p]. *)

val partition0: ('elt -> bool [@bs]) -> ('elt, 'id) t0 -> ('elt, 'id) t0 * ('elt, 'id) t0
(** [partition p s] returns a pair of sets [(s1, s2)], where
    [s1] is the set of all the elements of [s] that satisfy the
    predicate [p], and [s2] is the set of all the elements of
    [s] that do not satisfy [p]. *)

val cardinal0: ('elt, 'id) t0 -> int
(** Return the number of elements of a set. *)

val elements0: ('elt, 'id) t0 -> 'elt list
(** Return the list of all elements of the given set.
    The returned list is sorted in increasing order with respect
    to the ordering [Ord.compare], where [Ord] is the argument
    given to {!Set.Make}. *)

val min_elt0: ('elt, 'id) t0 -> 'elt
(** Return the smallest element of the given set
    (with respect to the [Ord.compare] ordering), or raise
    [Not_found] if the set is empty. *)

val max_elt0: ('elt, 'id) t0 -> 'elt
(** Same as {!Set.S.min_elt}, but returns the largest element of the
    given set. *)


val split0: 
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  'elt -> ('elt, 'id) t0 -> ('elt, 'id) t0 * bool * ('elt, 'id) t0
(** [split x s] returns a triple [(l, present, r)], where
      [l] is the set of elements of [s] that are
      strictly less than [x];
      [r] is the set of elements of [s] that are
      strictly greater than [x];
      [present] is [false] if [s] contains no element equal to [x],
      or [true] if [s] contains an element equal to [x]. *)

val find0: 
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  'elt -> ('elt, 'id) t0 -> 'elt
(** [find x s] returns the element of [s] equal to [x] (according
    to [Ord.compare]), or raise [Not_found] if no such element
    exists.
    @since 4.01.0 *)

