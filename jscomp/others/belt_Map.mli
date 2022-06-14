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
(*  Adapted by authors of ReScript without using functors          *)
(***********************************************************************)

(** The top level provides generic immutable map operations.

    It also has three specialized inner modules `Belt.Map.Int`,
    `Belt.Map.String` and `Belt.Map.Dict`. *)

(*  ```res prelude
    type t<'key, 'value, 'identity>
    type id<'key, 'id> = Belt_Id.comparable<'key, 'id>
    ```
*)

module Int = Belt_MapInt

module String = Belt_MapString

module Dict = Belt_MapDict


type ('key, 'value, 'identity) t
(** `'key` is the field type

    `'value` is the element type

    `'identity` the identity of the collection
*)


type ('key, 'id) id = ('key, 'id) Belt_Id.comparable
(** The identity needed for making an empty map. *)

val make: id:('k, 'id) id -> ('k, 'v, 'id) t
(** `make(~id)` creates a new map by taking in the comparator.

    ```res example
    module IntCmp = Belt.Id.MakeComparable({
      type t = int
      let cmp = (a, b) => Pervasives.compare(a, b)
    })

    let m = Belt.Map.make(~id=module(IntCmp))

    Belt.Map.set(m, 0, "a")
    ```
*)


val isEmpty: _ t -> bool
(** `isEmpty(m)` checks whether a map m is empty.

    ```res example
    module IntCmp = Belt.Id.MakeComparable({
      type t = int
      let cmp = (a, b) => Pervasives.compare(a, b)
    })

    Belt.Map.isEmpty(Belt.Map.fromArray([(1, "1")], ~id=module(IntCmp))) == false
    ```
*)

val has: ('k, 'v, 'id) t -> 'k  -> bool
(** `has(m, k)` checks whether `m` has the key `k`.

    ```res example
    module IntCmp = Belt.Id.MakeComparable({
      type t = int
      let cmp = (a, b) => Pervasives.compare(a, b)
    })

    Belt.Map.has(Belt.Map.fromArray([(1, "1")], ~id=module(IntCmp)), 1) == true
    ```
*)

val cmpU:
  ('k, 'v, 'id) t ->
  ('k, 'v, 'id) t ->
  ('v -> 'v -> int [@bs]) ->
  int
val cmp:
  ('k, 'v, 'id) t ->
  ('k, 'v, 'id) t ->
  ('v -> 'v -> int ) ->
  int
(** `cmp(m0, m1, vcmp);`

    Total ordering of map given total ordering of value function.

    It will compare size first and each element following the order one by one.
*)

val eqU:
  ('k, 'v, 'id) t ->
  ('k, 'v, 'id) t ->
  ('v -> 'v -> bool [@bs]) ->
  bool
val eq:
  ('k, 'v, 'id) t ->
  ('k, 'v, 'id) t ->
  ('v -> 'v -> bool) ->
  bool
(** `eq(m1, m2, veq)` tests whether the maps `m1` and `m2` are equal, that is,
    contain equal keys and associate them with equal data. `veq` is the
    equality predicate used to compare the data associated with the keys. *)

val findFirstByU : ('k, 'v, 'id) t -> ('k -> 'v -> bool [@bs]) -> ('k * 'v) option
val findFirstBy : ('k, 'v, 'id) t -> ('k -> 'v -> bool ) -> ('k * 'v) option
(** `findFirstBy(m, p)` uses function `f` to find the first key value pair to
    match predicate `p`.

    ```res example
    module IntCmp = Belt.Id.MakeComparable({
      type t = int
      let cmp = (a, b) => Pervasives.compare(a, b)
    })

    let s0 = Belt.Map.fromArray(~id=module(IntCmp), [(4, "4"), (1, "1"), (2, "2"), (3, "")])

    Belt.Map.findFirstBy(s0, (k, v) => k == 4) /* (4, "4") */
    ```
*)

val forEachU:  ('k, 'v, 'id) t -> ('k -> 'v -> unit [@bs]) -> unit
val forEach:  ('k, 'v, 'id) t -> ('k -> 'v -> unit) -> unit
(** `forEach(m, f)` applies `f` to all bindings in map `m`. `f` receives the
    `'k` as first argument, and the associated value as second argument. The
    bindings are passed to `f` in increasing order with respect to the ordering
    over the type of the keys.

    ```res example
    module IntCmp = Belt.Id.MakeComparable({
      type t = int
      let cmp = (a, b) => Pervasives.compare(a, b)
    })

    let s0 = Belt.Map.fromArray(~id=module(IntCmp), [(4, "4"), (1, "1"), (2, "2"), (3, "")])

    let acc = ref(list{})

    Belt.Map.forEach(s0, (k, v) => acc := list{(k, v), ...acc.contents})

    acc.contents == list{(4, "4"), (3, "3"), (2, "2"), (1, "1")}
    ```
*)

val reduceU: ('k, 'v, 'id) t -> 'acc -> ('acc -> 'k -> 'v -> 'acc [@bs]) -> 'acc
val reduce: ('k, 'v, 'id) t -> 'acc -> ('acc -> 'k -> 'v -> 'acc) -> 'acc
(** `reduce(m, a, f)` computes `(f(kN, dN) ... (f(k1, d1, a))...)`, where `k1
    ... kN` are the keys of all bindings in m (in increasing order), and `d1
    ... dN` are the associated data.

    ```res example
    module IntCmp = Belt.Id.MakeComparable({
      type t = int
      let cmp = (a, b) => Pervasives.compare(a, b)
    })

    let s0 = Belt.Map.fromArray(~id=module(IntCmp), [(4, "4"), (1, "1"), (2, "2"), (3, "3")])

    Belt.Map.reduce(s0, list{}, (acc, k, v) => list{
      (k, v),
      ...acc,
    }) /* [(4, "4"), (3, "3"), (2, "2"), (1, "1"), 0] */
    ```
*)

val everyU: ('k, 'v, 'id) t -> ('k -> 'v -> bool [@bs]) ->  bool
val every: ('k, 'v, 'id) t -> ('k -> 'v -> bool) ->  bool
(** `every(m, p)` checks if all the bindings of the map satisfy the predicate
    `p`. Order unspecified *)

val someU: ('k, 'v, 'id) t -> ('k -> 'v -> bool [@bs]) ->  bool
val some: ('k, 'v, 'id) t -> ('k -> 'v -> bool) ->  bool
(** `some(m, p)` checks if at least one binding of the map satisfy the
    predicate `p`. Order unspecified *)

val size: ('k, 'v, 'id) t -> int
(** `size(s)`

    ```res example
    module IntCmp = Belt.Id.MakeComparable({
      type t = int
      let cmp = (a, b) => Pervasives.compare(a, b)
    })

    Belt.Map.size(Belt.Map.fromArray([(2, "2"), (2, "1"), (3, "3")], ~id=module(IntCmp))) == 2
    ```
*)

val toArray: ('k, 'v, 'id) t -> ('k * 'v) array
(** `toArray(s)`

    ```res example
    module IntCmp = Belt.Id.MakeComparable({
      type t = int
      let cmp = (a, b) => Pervasives.compare(a, b)
    })

    Belt.Map.toArray(Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp))) == [
        (1, "1"),
        (2, "2"),
        (3, "3"),
      ]
    ```
*)

val toList: ('k, 'v, 'id) t -> ('k * 'v) list
(** In increasing order.

    See `Belt.Map.toArray`
*)


val fromArray:  ('k * 'v) array -> id:('k,'id) id -> ('k,'v,'id) t
(** `fromArray(kvs, ~id);`

    ```res example
    module IntCmp = Belt.Id.MakeComparable({
      type t = int
      let cmp = (a, b) => Pervasives.compare(a, b)
    })

    Belt.Map.toArray(Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp))) == [
        (1, "1"),
        (2, "2"),
        (3, "3"),
      ]
    ```
*)

val keysToArray: ('k, 'v, 'id) t -> 'k  array
(** `keysToArray(s);`

    ```res example
    module IntCmp = Belt.Id.MakeComparable({
      type t = int
      let cmp = (a, b) => Pervasives.compare(a, b)
    })

    Belt.Map.keysToArray(Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp))) == [
        1,
        2,
        3,
      ]
    ```
*)

val valuesToArray: ('k, 'v, 'id) t -> 'v  array
(** `valuesToArray(s);`

    ```res example
    module IntCmp = Belt.Id.MakeComparable({
      type t = int
      let cmp = (a, b) => Pervasives.compare(a, b)
    })

    Belt.Map.valuesToArray(
      Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp)),
    ) == ["1", "2", "3"]
    ```
*)

val minKey: ('k, _, _) t -> 'k option
(** `minKey(s)` returns the minimum key, None if not exist. *)

val minKeyUndefined: ('k, _, _) t -> 'k Js.undefined
(** See `Belt.Map.minKey` *)

val maxKey: ('k, _, _) t -> 'k option
(** `maxKey(s)` returns the maximum key, None if not exist. *)

val maxKeyUndefined: ('k, _, _) t -> 'k Js.undefined
(** See `Belt.Map.maxKey` *)

val minimum: ('k, 'v,  _) t -> ('k * 'v) option
(** `minimum(s)` returns the minimum key value pair, None if not exist. *)

val minUndefined: ('k, 'v, _) t -> ('k * 'v) Js.undefined
(** See `Belt.Map.minimum` *)

val maximum: ('k, 'v, _) t -> ('k * 'v) option
(** `maximum(s)` returns the maximum key value pair, None if not exist. *)

val maxUndefined:('k, 'v, _) t -> ('k * 'v) Js.undefined
(** See `Belt.Map.maximum` *)

val get:  ('k, 'v, 'id) t -> 'k -> 'v option
(** `get(s, k)`

    ```res example
    module IntCmp = Belt.Id.MakeComparable({
      type t = int
      let cmp = (a, b) => Pervasives.compare(a, b)
    })

    Belt.Map.get(Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp)), 2) ==
      Some("2")

    Belt.Map.get(Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp)), 2) == None
    ```
*)

val getUndefined: ('k, 'v, 'id) t -> 'k ->  'v Js.undefined
(** See `Belt.Map.get`

    Returns `undefined` when not found
*)

val getWithDefault:
  ('k, 'v, 'id) t -> 'k ->  'v -> 'v
(** `getWithDefault(s, k, default)`

    See `Belt.Map.get`

    Returns default when `k` is not found.
*)

val getExn:  ('k, 'v, 'id) t -> 'k -> 'v
(** `getExn(s, k)`

    See `Belt.Map.getExn`

    raise when `k` not exist
*)

(****************************************************************************)

val remove:  ('k, 'v, 'id) t -> 'k -> ('k, 'v, 'id) t
(** `remove(m, x)` when `x` is not in `m`, `m` is returned reference unchanged.

    ```res example
    module IntCmp = Belt.Id.MakeComparable({
      type t = int
      let cmp = (a, b) => Pervasives.compare(a, b)
    })

    let s0 = Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp))

    let s1 = Belt.Map.remove(s0, 1)

    let s2 = Belt.Map.remove(s1, 1)

    s1 === s2

    Belt.Map.keysToArray(s1) == [2, 3]
    ```
*)

val removeMany: ('k, 'v, 'id) t -> 'k array -> ('k, 'v, 'id) t
(** `removeMany(s, xs)`

    Removing each of `xs` to `s`, note unlike `Belt.Map.remove`, the reference
    of return value might be changed even if none in `xs` exists `s`.
*)

val set:
  ('k, 'v, 'id) t -> 'k -> 'v ->  ('k, 'v, 'id) t
(** `set(m, x, y)` returns a map containing the same bindings as `m`, with a
    new binding of `x` to `y`. If `x` was already bound in `m`, its previous
    binding disappears.

    ```res example
    module IntCmp = Belt.Id.MakeComparable({
      type t = int
      let cmp = (a, b) => Pervasives.compare(a, b)
    })

    let s0 = Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp))

    let s1 = Belt.Map.set(s0, 2, "3")

    Belt.Map.valuesToArray(s1) == ["1", "3", "3"]
    ```
*)

val updateU: ('k, 'v, 'id) t -> 'k -> ('v option -> 'v option [@bs]) -> ('k, 'v, 'id) t
val update: ('k, 'v, 'id) t -> 'k -> ('v option -> 'v option) -> ('k, 'v, 'id) t
(** `update(m, x, f)` returns a map containing the same bindings as `m`, except
    for the binding of `x`. Depending on the value of `y` where `y` is
    `f(get(m, x))`, the binding of `x` is added, removed or updated. If `y` is
    `None`, the binding is removed if it exists; otherwise, if `y` is `Some(z)`
    then `x` is associated to `z` in the resulting map. *)

val mergeMany:
  ('k, 'v, 'id) t -> ('k * 'v) array ->  ('k, 'v, 'id) t
(** `mergeMany(s, xs)`

    Adding each of `xs` to `s`, note unlike `add`, the reference of return
    value might be changed even if all values in `xs` exist `s`.
*)

val mergeU:
  ('k, 'v, 'id ) t ->
  ('k, 'v2, 'id) t ->
  ('k -> 'v option -> 'v2 option -> 'v3 option [@bs]) ->
  ('k, 'v3, 'id) t
val merge:
  ('k, 'v, 'id ) t ->
  ('k, 'v2, 'id) t ->
  ('k -> 'v option -> 'v2 option -> 'v3 option) ->
  ('k, 'v3, 'id) t
(** `merge(m1, m2, f)` computes a map whose keys is a subset of keys of `m1`
    and of `m2`. The presence of each such binding, and the corresponding
    value, is determined with the function `f`. *)


val keepU:
  ('k, 'v, 'id) t ->
  ('k -> 'v -> bool [@bs]) ->
  ('k, 'v, 'id) t
val keep:
  ('k, 'v, 'id) t ->
  ('k -> 'v -> bool) ->
  ('k, 'v, 'id) t
(** `keep(m, p)` returns the map with all the bindings in m that satisfy
    predicate `p`. *)

val partitionU:
  ('k, 'v, 'id) t ->
  ('k -> 'v -> bool [@bs]) ->
  ('k, 'v, 'id) t * ('k, 'v, 'id) t
val partition:
  ('k, 'v, 'id) t ->
  ('k -> 'v -> bool) ->
  ('k, 'v, 'id) t * ('k, 'v, 'id) t
(** `partition(m, p)` returns a pair of maps `(m1, m2)`, where `m1` contains
    all the bindings of `s` that satisfy the predicate `p`, and `m2` is the map
    with all the bindings of `s` that do not satisfy `p`. *)

val split:
  ('k, 'v, 'id) t -> 'k ->
  (('k, 'v, 'id) t * ('k, 'v, 'id) t )* 'v option
(** `split(x, m)` returns a tuple `(l, r)`, data, where `l` is the map with all
    the bindings of `m` whose 'k is strictly less than `x`; `r` is the map with
    all the bindings of m whose 'k is strictly greater than `x`; `data` is
    `None` if `m` contains no binding for `x`, or `Some(v)` if `m` binds `v` to
    `x`. *)

val mapU: ('k, 'v, 'id) t -> ('v -> 'v2 [@bs]) ->  ('k, 'v2, 'id) t
val map: ('k, 'v, 'id) t -> ('v -> 'v2) ->  ('k, 'v2, 'id) t
(** `map(m, f) returns a map with same domain as`m`, where the associated
    value`a`of all bindings of`m`has been replaced by the result of the
    application of`f`to`a`. The bindings are passed to`f` in increasing order
    with respect to the ordering over the type of the keys. *)

val mapWithKeyU: ('k, 'v, 'id) t -> ('k -> 'v -> 'v2 [@bs]) -> ('k, 'v2, 'id) t
val mapWithKey: ('k, 'v, 'id) t -> ('k -> 'v -> 'v2) -> ('k, 'v2, 'id) t
(** `mapWithKey(m, f)`

    The same as `Belt.Map.map` except that `f` is supplied with one more
    argument: the key.
*)



val getData: ('k, 'v, 'id) t -> ('k, 'v, 'id) Belt_MapDict.t
(** `getData(s0)`

    Advanced usage only

    Returns the raw data (detached from comparator), but its type is still
    manifested, so that user can pass identity directly without boxing.
*)

val getId: ('k, 'v, 'id) t -> ('k, 'id) id
(** Advanced usage only

    Returns the identity of s0.
*)

val packIdData: id:('k, 'id) id -> data:('k, 'v, 'id) Belt_MapDict.t -> ('k, 'v, 'id) t
(** `packIdData(~id, ~data)`

    Advanced usage only

    Returns the packed collection.
*)

(**/**)
val checkInvariantInternal: _ t -> unit
(**
   **raise** when invariant is not held
*)
(**/**)
