# 4 "others/map.cppo.mli"
type key = int
# 8 "others/map.cppo.mli"
type 'value t
(** The type of maps from type `key` to type `'value`. *)

val empty: 'v t

val isEmpty: 'v t -> bool

val has: 'v t -> key -> bool

val cmpU: 'v t -> 'v t -> ('v -> 'v -> int [@bs]) -> int
val cmp: 'v t -> 'v t -> ('v -> 'v -> int) -> int

val eqU: 'v t -> 'v t -> ('v -> 'v -> bool [@bs]) -> bool
val eq: 'v t -> 'v t -> ('v -> 'v -> bool) -> bool
(**
  `eq m1 m2` tests whether the maps `m1` and `m2` are
  equal, that is, contain equal keys and associate them with
  equal data.
*)

val findFirstByU : 'v t -> (key -> 'v -> bool [@bs]) -> (key * 'v) option
val findFirstBy : 'v t -> (key -> 'v -> bool) -> (key * 'v) option
(**
  `findFirstBy m p` uses funcion `f` to find the first key value pair
  to match predicate `p`.

  ```
  let s0 = fromArray ~id:(module IntCmp) [|4,"4";1,"1";2,"2,"3""|];;
  findFirstBy s0 (fun k v -> k = 4 ) = option (4, "4");;
  ```
*)

val forEachU: 'v t -> (key -> 'v -> unit [@bs]) -> unit
val forEach: 'v t -> (key -> 'v -> unit) -> unit
(**
  `forEach m f` applies `f` to all bindings in map `m`.
  `f` receives the key as first argument, and the associated value
  as second argument. The bindings are passed to `f` in increasing
  order with respect to the ordering over the type of the keys.
*)

val reduceU: 'v t -> 'v2 -> ('v2 -> key -> 'v -> 'v2 [@bs]) -> 'v2
val reduce: 'v t -> 'v2 -> ('v2 -> key -> 'v -> 'v2) -> 'v2
(**
  `reduce m a f` computes `(f kN dN ... (f k1 d1 a)...)`,
  where `k1 ... kN` are the keys of all bindings in `m`
  (in increasing order), and `d1 ... dN` are the associated data.
*)

val everyU: 'v t -> (key -> 'v -> bool [@bs]) -> bool
val every: 'v t -> (key -> 'v -> bool) -> bool
(** `every m p` checks if all the bindings of the map
    satisfy the predicate `p`. Order unspecified *)

val someU: 'v t -> (key -> 'v -> bool [@bs]) -> bool
val some: 'v t -> (key -> 'v -> bool) -> bool
(** `some m p` checks if at least one binding of the map
    satisfy the predicate `p`. Order unspecified *)

val size: 'v t -> int

val toList: 'v t -> (key * 'v) list
(** In increasing order. *)

val toArray: 'v t -> (key * 'v) array


val fromArray: (key * 'v) array -> 'v t

val keysToArray: 'v t -> key array

val valuesToArray: 'v t -> 'v array

val minKey: _ t -> key option

val minKeyUndefined: _ t -> key Js.undefined

val maxKey: _ t -> key option

val maxKeyUndefined: _ t -> key Js.undefined

val minimum: 'v t -> (key * 'v) option

val minUndefined: 'v t -> (key * 'v) Js.undefined

val maximum: 'v t -> (key * 'v) option

val maxUndefined: 'v t -> (key * 'v) Js.undefined

val get: 'v t -> key -> 'v option

val getUndefined: 'v t -> key -> 'v Js.undefined

val getWithDefault: 'v t -> key -> 'v -> 'v

val getExn: 'v t -> key -> 'v

val checkInvariantInternal: _ t -> unit
(**
  **raise** when invariant is not held
*)

val remove: 'v t -> key -> 'v t
(** `remove m x` returns a map containing the same bindings as
    `m`, except for `x` which is unbound in the returned map. *)

val removeMany: 'v t -> key array -> 'v t

val set: 'v t -> key -> 'v -> 'v t
(**
  `set m x y` returns a map containing the same bindings as
  `m`, plus a binding of `x` to `y`. If `x` was already bound
  in `m`, its previous binding disappears.
*)

val updateU: 'v t -> key -> ('v option -> 'v option [@bs]) -> 'v t
val update: 'v t -> key -> ('v option -> 'v option) -> 'v t

val mergeU:
  'v t -> 'v2 t ->
  (key -> 'v option -> 'v2 option -> 'c option [@bs]) ->
  'c t
val merge:
  'v t -> 'v2 t ->
  (key -> 'v option -> 'v2 option -> 'c option) ->
  'c t
(**
  `merge m1 m2 f` computes a map whose keys is a subset of keys of `m1`
  and of `m2`. The presence of each such binding, and the corresponding
  value, is determined with the function `f`.
*)

val mergeMany: 'v t -> (key * 'v) array -> 'v t


val keepU:
  'v t ->
  (key -> 'v -> bool [@bs]) ->
  'v t
val keep:
  'v t ->
  (key -> 'v -> bool) ->
  'v t
(** `keep m p` returns the map with all the bindings in `m`
    that satisfy predicate `p`. *)

val partitionU:
  'v t ->
  (key -> 'v -> bool [@bs]) ->
  'v t * 'v t
val partition:
  'v t ->
  (key -> 'v -> bool) ->
  'v t * 'v t
(**
  `partition m p` returns a pair of maps `(m1, m2)`, where
  `m1` contains all the bindings of `s` that satisfy the
  predicate `p`, and `m2` is the map with all the bindings of
  `s` that do not satisfy `p`.
*)

val split: key -> 'v t -> 'v t * 'v option * 'v t
(**
  `split x m` returns a triple `(l, data, r)`, where
  `l` is the map with all the bindings of `m` whose key
  is strictly less than `x`;
  `r` is the map with all the bindings of `m` whose key
  is strictly greater than `x`;
  `data` is `None` if `m` contains no binding for `x`,
  or `Some v` if `m` binds `v` to `x`.
*)

val mapU: 'v t -> ('v -> 'v2 [@bs]) -> 'v2 t
val map: 'v t -> ('v -> 'v2) -> 'v2 t
(**
  `map m f` returns a map with same domain as `m`, where the
  associated value `a` of all bindings of `m` has been
  replaced by the result of the application of `f` to `a`.
  The bindings are passed to `f` in increasing order
  with respect to the ordering over the type of the keys.
*)

val mapWithKeyU: 'v t -> (key -> 'v -> 'v2 [@bs]) -> 'v2 t
val mapWithKey: 'v t -> (key -> 'v -> 'v2) -> 'v2 t
