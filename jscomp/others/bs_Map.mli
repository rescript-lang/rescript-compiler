
(** The type of the map keys. *)

type ('k, +'a, 'id) t 
(** The type of maps from type ['k] to type ['a]. *)

val empty: ('k, 'id) Bs_Cmp.t -> ('k, 'a, 'id) t 
(** The empty map. *)

val is_empty: ('k, 'a, 'id) t -> bool
(** Test whether a map is empty or not. *)

val mem: 'k -> ('k, 'a, 'id) t -> bool
(** [mem x m] returns [true] if [m] contains a binding for [x],
    and [false] otherwise. *)

val add: 'k -> 'a -> ('k, 'a, 'id) t -> ('k, 'a, 'id) t
(** [add x y m] returns a map containing the same bindings as
    [m], plus a binding of [x] to [y]. If [x] was already bound
    in [m], its previous binding disappears. *)

val singleton: ('k,'id) Bs_Cmp.t -> 'k -> 'a -> ('k, 'a, 'id) t
(** [singleton x y] returns the one-element map that contains a binding [y]
    for [x].
    @since 3.12.0
*)

val remove: 'k -> ('k, 'a, 'id) t -> ('k, 'a, 'id) t
(** [remove x m] returns a map containing the same bindings as
    [m], except for [x] which is unbound in the returned map. *)

val merge:
  ('k -> 'a option -> 'b option -> 'c option [@bs]) -> ('k, 'a, 'id ) t -> ('k, 'b,'id) t -> ('k, 'c,'id) t
(** [merge f m1 m2] computes a map whose keys is a subset of keys of [m1]
    and of [m2]. The presence of each such binding, and the corresponding
    value, is determined with the function [f].
    @since 3.12.0
*)

val compare: ('a -> 'a -> int [@bs]) -> ('k, 'a, 'id) t -> ('k, 'a, 'id) t -> int
(** Total ordering between maps.  The first argument is a total ordering
    used to compare data associated with equal keys in the two maps. *)

val equal: ('a -> 'a -> bool [@bs]) -> ('k, 'a, 'id) t -> ('k, 'a, 'id) t -> bool
(** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are
    equal, that is, contain equal keys and associate them with
    equal data.  [cmp] is the equality predicate used to compare
    the data associated with the keys. *)

val iter: ('k -> 'a -> unit [@bs]) -> ('k, 'a, 'id) t -> unit
(** [iter f m] applies [f] to all bindings in map [m].
    [f] receives the 'k as first argument, and the associated value
    as second argument.  The bindings are passed to [f] in increasing
    order with respect to the ordering over the type of the keys. *)

val fold: ('k -> 'a -> 'b -> 'b [@bs]) -> ('k, 'a, 'id) t -> 'b -> 'b
(** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
    where [k1 ... kN] are the keys of all bindings in [m]
    (in increasing order), and [d1 ... dN] are the associated data. *)

val for_all: ('k -> 'a -> bool [@bs]) -> ('k, 'a, 'id) t -> bool
(** [for_all p m] checks if all the bindings of the map
    satisfy the predicate [p].
    @since 3.12.0
*)

val exists: ('k -> 'a -> bool [@bs]) -> ('k, 'a, 'id) t -> bool
(** [exists p m] checks if at least one binding of the map
    satisfy the predicate [p].
    @since 3.12.0
*)

val filter: ('k -> 'a -> bool [@bs]) -> ('k, 'a, 'id) t -> ('k, 'a, 'id) t
(** [filter p m] returns the map with all the bindings in [m]
    that satisfy predicate [p].
    @since 3.12.0
*)

val partition: ('k -> 'a -> bool [@bs]) -> ('k, 'a, 'id) t -> ('k, 'a, 'id) t * ('k, 'a, 'id) t
(** [partition p m] returns a pair of maps [(m1, m2)], where
    [m1] contains all the bindings of [s] that satisfy the
    predicate [p], and [m2] is the map with all the bindings of
    [s] that do not satisfy [p].
    @since 3.12.0
*)

val cardinal: ('k, 'a, 'id) t -> int
(** Return the number of bindings of a map.
    @since 3.12.0
*)

val bindings: ('k, 'a, 'id) t -> ('k * 'a) list
(** Return the list of all bindings of the given map.
    The returned list is sorted in increasing order with respect
    to the ordering [Ord.compare], where [Ord] is the argument
    given to {!Map.Make}.
    @since 3.12.0
*)

val min_binding: ('k, 'a, 'id) t -> ('k * 'a)
(** Return the smallest binding of the given map
    (with respect to the [Ord.compare] ordering), or raise
    [Not_found] if the map is empty.
    @since 3.12.0
*)

val max_binding: ('k, 'a, 'id) t -> ('k * 'a)
(** Same as {!Map.S.min_binding}, but returns the largest binding
    of the given map.
    @since 3.12.0
*)



val split: 'k -> ('k, 'a, 'id) t -> ('k, 'a, 'id) t * 'a option * ('k, 'a, 'id) t
(** [split x m] returns a triple [(l, data, r)], where
      [l] is the map with all the bindings of [m] whose 'k
    is strictly less than [x];
      [r] is the map with all the bindings of [m] whose 'k
    is strictly greater than [x];
      [data] is [None] if [m] contains no binding for [x],
      or [Some v] if [m] binds [v] to [x].
    @since 3.12.0
*)

val find: 'k -> ('k, 'a, 'id) t -> 'a
(** [find x m] returns the current binding of [x] in [m],
    or raises [Not_found] if no such binding exists. *)

val map: ('a -> 'b [@bs]) -> ('k, 'a, 'id) t -> ('k ,'b,'id ) t
(** [map f m] returns a map with same domain as [m], where the
    associated value [a] of all bindings of [m] has been
    replaced by the result of the application of [f] to [a].
    The bindings are passed to [f] in increasing order
    with respect to the ordering over the type of the keys. *)

val mapi: ('k -> 'a -> 'b [@bs]) -> ('k, 'a, 'id) t -> ('k, 'b, 'id) t
(** Same as {!Map.S.map}, but the function receives as arguments both the
    'k and the associated value for each binding of the map. *)

