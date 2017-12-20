


type ('elt, 'id) t0


type ('k,'id) t = 
  (('k,'id) Bs_Cmp.t,
   ('k, 'id) t0 ) Bs_Bag.bag

(** The type of sets. *)

val empty0: ('elt, 'id) t0
val empty : ('elt, 'id) Bs_Cmp.t -> ('elt, 'id) t
(** The empty set. *)


val ofArray0: cmp:('k,'id) Bs_Cmp.cmp -> 'k array -> ('k, 'id) t0  
val ofArray: ('k, 'id) Bs_Cmp.t -> 'k array -> ('k, 'id) t 


val isEmpty0: ('elt, 'id) t0 -> bool
val isEmpty : ('elt, 'id) t -> bool
(** Test whether a set is empty or not. *)

val mem0: 
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  'elt -> ('elt, 'id) t0 -> bool
val mem:  
  'elt -> ('elt, 'id) t -> bool
(** [mem x s] tests whether [x] belongs to the set [s]. *)

val add0: 
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  'elt -> ('elt, 'id) t0 -> ('elt, 'id) t0
val add:   
  'elt -> ('elt, 'id) t -> ('elt, 'id) t
(** [add x s] returns a set containing all elements of [s],
    plus [x]. If [x] was already in [s], [s] is returned unchanged. *)

val singleton0: 'elt -> ('elt, 'id) t0
val singleton : 
  ('elt,'id) Bs_Cmp.t -> 
  'elt -> ('elt, 'id) t
(** [singleton x] returns the one-element set containing only [x]. *)

val remove0: 
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  'elt -> ('elt, 'id) t0 -> ('elt, 'id) t0
val remove:  
  'elt -> ('elt, 'id) t -> ('elt, 'id) t
(** [remove x s] returns a set containing all elements of [s],
    except [x]. If [x] was not in [s], [s] is returned unchanged. *)

val union0: 
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  ('elt, 'id) t0 -> ('elt, 'id) t0 -> ('elt, 'id) t0
val union:  
  ('elt, 'id) t -> ('elt, 'id) t -> ('elt, 'id) t
(** Set union. *)

(* val inter0: 
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  ('elt, 'id) t0 -> ('elt, 'id) t0 -> ('elt, 'id) t0
val inter: 
  ('elt, 'id) t -> ('elt, 'id) t -> ('elt, 'id) t *)
(** Set intersection. *)

(* val diff0: 
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  ('elt, 'id) t0 -> ('elt, 'id) t0 -> ('elt, 'id) t0
val diff:   
  ('elt, 'id) t -> ('elt, 'id) t -> ('elt, 'id) t *)
(** Set difference. *)

val cmp0: 
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  ('elt, 'id) t0 -> ('elt, 'id) t0 -> int
val cmp:  
  ('elt, 'id) t -> ('elt, 'id) t -> int
(** Total ordering between sets. Can be used as the ordering function
    for doing sets of sets. *)

val eq0:
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  ('elt, 'id) t0 -> ('elt, 'id) t0 -> bool
val eq:  
  ('elt, 'id) t -> ('elt, 'id) t -> bool
(** [equal s1 s2] tests whether the sets [s1] and [s2] are
    equal, that is, contain equal elements. *)

(* val subset0: 
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  ('elt, 'id) t0 -> ('elt, 'id) t0 -> bool
val subset:  
  ('elt, 'id) t -> ('elt, 'id) t -> bool *)
(** [subset s1 s2] tests whether the set [s1] is a subset of
    the set [s2]. *)

val iter0: ('elt -> unit [@bs]) -> ('elt, 'id) t0 -> unit
val iter: ('elt -> unit [@bs]) -> ('elt, 'id) t -> unit
(** [iter f s] applies [f] in turn to all elements of [s].
    The elements of [s] are presented to [f] in increasing order
    with respect to the ordering over the type of the elements. *)

val fold0: ('elt -> 'a -> 'a [@bs]) -> ('elt, 'id) t0 -> 'a -> 'a
val fold: ('elt -> 'a -> 'a [@bs]) -> ('elt, 'id) t -> 'a -> 'a
(** [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)],
    where [x1 ... xN] are the elements of [s], in increasing order. *)

val forAll0: ('elt -> bool [@bs]) -> ('elt, 'id) t0 -> bool
val forAll:('elt -> bool [@bs]) -> ('elt, 'id) t -> bool
(** [for_all p s] checks if all elements of the set
    satisfy the predicate [p]. *)

val exists0: ('elt -> bool [@bs]) -> ('elt, 'id) t0 -> bool
val exists: ('elt -> bool [@bs]) -> ('elt, 'id) t -> bool
(** [exists p s] checks if at least one element of
    the set satisfies the predicate [p]. *)

val filter0: ('elt -> bool [@bs]) -> ('elt, 'id) t0 -> ('elt, 'id) t0
val filter: ('elt -> bool [@bs]) -> ('elt, 'id) t -> ('elt, 'id) t
(** [filter p s] returns the set of all elements in [s]
    that satisfy predicate [p]. *)

val partition0: ('elt -> bool [@bs]) -> ('elt, 'id) t0 -> ('elt, 'id) t0 * ('elt, 'id) t0
val partition: ('elt -> bool [@bs]) -> ('elt, 'id) t -> ('elt, 'id) t * ('elt, 'id) t
(** [partition p s] returns a pair of sets [(s1, s2)], where
    [s1] is the set of all the elements of [s] that satisfy the
    predicate [p], and [s2] is the set of all the elements of
    [s] that do not satisfy [p]. *)

val cardinal0: ('elt, 'id) t0 -> int
val cardinal:('elt, 'id) t -> int
(** Return the number of elements of a set. *)

val elements0: ('elt, 'id) t0 -> 'elt list
val elements: ('elt, 'id) t -> 'elt list
(** Return the list of all elements of the given set.
    The returned list is sorted in increasing order with respect
    to the ordering [Ord.compare], where [Ord] is the argument
    given to {!Set.Make}. *)
val toArray0: ('elt, 'id) t0 -> 'elt array
val toArray: ('elt, 'id) t -> 'elt array

val min0: ('elt, 'id) t0 -> 'elt option
val min: ('elt, 'id) t -> 'elt option
(** Return the smallest element of the given set
    (with respect to the [Ord.compare] ordering), or raise
    [Not_found] if the set is empty. *)

val max0: ('elt, 'id) t0 -> 'elt option
val max: ('elt, 'id) t -> 'elt option
(** Same as {!Set.S.min_elt}, but returns the largest element of the
    given set. *)


val split0: 
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  'elt -> ('elt, 'id) t0 -> ('elt, 'id) t0 * bool * ('elt, 'id) t0
val split: 
  'elt -> ('elt, 'id) t -> ('elt, 'id) t * bool * ('elt, 'id) t
(** [split x s] returns a triple [(l, present, r)], where
      [l] is the set of elements of [s] that are
      strictly less than [x];
      [r] is the set of elements of [s] that are
      strictly greater than [x];
      [present] is [false] if [s] contains no element equal to [x],
      or [true] if [s] contains an element equal to [x]. *)

(* val findOpt0: 
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  'elt -> ('elt, 'id) t0 -> 'elt option
val findOpt: 
  'elt -> ('elt, 'id) t -> 'elt option *)
(** [find x s] returns the element of [s] equal to [x] (according
    to [Ord.compare]), or raise [Not_found] if no such element
    exists.
    @since 4.01.0 *)

(* val findAssert0:
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  'elt -> ('elt, 'id) t0 -> 'elt 

val findAssert:  
  'elt -> ('elt, 'id) t -> 'elt  *)
