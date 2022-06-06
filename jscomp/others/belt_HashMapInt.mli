(** Specalized when key type is `int`, more efficient than the generic type *)

# 4 "others/hashmap.cppo.mli"
type key = int


# 10 "others/hashmap.cppo.mli"
type 'b t


(**
`make(~hintSize=10)` creates a new hash map by taking the `hintSize`.

```res example
let hMap = Belt.HashMap.Int.make(~hintSize=10)

Belt.HashMap.Int.set(hMap, 1, "a")
```
**)
val make:  hintSize:int -> 'b t

(**
Clears a hash table.

```res example
let hMap = Belt.HashMap.Int.fromArray([(1, "1")])
Belt.HashMap.Int.clear(hMap)
Belt.HashMap.Int.isEmpty(hMap) == true
```
*)
val clear: 'b t -> unit

(**
`isEmpty(m)` checks whether a hash map is empty.

```res example
let hMap = Belt.HashMap.Int.fromArray([(1, "1")])
Belt.HashMap.Int.isEmpty(hMap) == false
```
 *)
val isEmpty: _ t -> bool

(**
`set(tbl, k, v)` if `k` does not exist, add the binding `k,v`, otherwise,
update the old value with the new `v`.

```res example
let hMap = Belt.HashMap.Int.fromArray([(2, "2")])

Belt.HashMap.Int.set(hMap, 1, "1")

Belt.HashMap.Int.valuesToArray(hMap) == ["1", "2"]
```
*)
val set: 'a t -> key -> 'a -> unit

(**
Creates copy of a hash map.

```res example
let hMap1 = Belt.HashMap.Int.fromArray([(1, "1"), (2, "2")])
let hMap2 = Belt.HashMap.Int.copy(hMap1)

Belt.HashMap.Int.set(hMap2, 2, "3")

Belt.HashMap.Int.get(hMap1, 2) != Belt.HashMap.Int.get(hMap2, 2)
```
*)
val copy: 'a t -> 'a t

(**
Returns the value of given key or `None` if it doesn't exist.

```res example
let hMap = Belt.HashMap.Int.make(~hintSize=10)
Belt.HashMap.Int.set(hMap, 1, "value1")

Belt.HashMap.Int.get(hMap, 1) == Some("value1")
Belt.HashMap.Int.get(hMap, 2) == None
```
 *)
val get:  'a t -> key -> 'a option

(**
has(t, key) returns `true` if `key` exists in given map `t`.
*)
val has:  'b  t -> key -> bool

(**
`remove(t, key)` will remove the value bound to `key` from map `t`, if given
`key` exists.

```res example
let hMap = Belt.HashMap.Int.make(~hintSize=10)
Belt.HashMap.Int.set(hMap, 1, "value1")
Belt.HashMap.Int.remove(hMap, 1)
Belt.HashMap.Int.has(hMap, 1) == false
```
*)
val remove: 'a t -> key -> unit

(** Same as [forEach](##forEach) but takes an uncurried function. *)
val forEachU: 'b t -> (key -> 'b -> unit [@bs]) -> unit

(**
`forEach(tbl, f)` applies `f` to all bindings in table `tbl`. `f` receives the
key as first argument, and the associated value as second argument. Each
binding is presented exactly once to `f`.

```res example
let hMap = Belt.HashMap.Int.make(~hintSize=10)
Belt.HashMap.Int.set(hMap, 1, "value1")
Belt.HashMap.Int.forEach(hMap, (key, value) => Js.log2(key, value))
// prints ("1", "value1")
```
*)
val forEach: 'b t -> (key -> 'b -> unit) -> unit

(** Same as [reduce](##reduce) but takes an uncurried function. *)
val reduceU: 'b t -> 'c -> ('c -> key -> 'b ->  'c [@bs]) -> 'c

(**
`reduce(tbl, init, f)` computes `(f(kN, dN) ... (f(k1, d1, init))...)`, where
`k1 ... kN` are the keys of all bindings in `tbl`, and `d1 ... dN` are the
associated values. Each binding is presented exactly once to `f`.

The order in which the bindings are passed to `f` is unspecified. However, if
  the table contains several bindings for the same key, they are passed to `f`
  in reverse order of introduction, that is, the most recent binding is passed
  first.

```res example
let hMap = Belt.HashMap.Int.make(~hintSize=10)
Belt.HashMap.Int.set(hMap, 1, "value1")
Belt.HashMap.Int.set(hMap, 2, "value2")

Belt.HashMap.Int.reduce(hMap, "", (acc, key, value) => acc ++ (", " ++ value)) == "value1, value2"
```
*)
val reduce: 'b t -> 'c -> ('c -> key -> 'b ->  'c) -> 'c

(** Same as [keepMapInPlace](##keepMapInPlace) but takes an uncurried function. *)
val keepMapInPlaceU: 'a t ->  (key -> 'a -> 'a option [@bs]) -> unit

(**
Filters out values for which function `f` returned `None`.

```res example let hMap = Belt.HashMap.Int.make(~hintSize=10)
Belt.HashMap.Int.set(hMap, 1, "value1") Belt.HashMap.Int.set(hMap, 2, "value2")

Belt.HashMap.Int.keepMapInPlace(hMap, (key, value) => mod(key, 1) == 0 ? None :
  Some(value))
```
*)
val keepMapInPlace: 'a t ->  (key -> 'a -> 'a option) -> unit

(**
`size(tbl)` returns the number of bindings in `tbl`. It takes constant time.

```res example
let hMap = Belt.HashMap.Int.make(~hintSize=10)
Belt.HashMap.Int.set(hMap, 1, "value1")
Belt.HashMap.Int.set(hMap, 2, "value2")

Belt.HashMap.Int.size(hMap) == 2
```
*)
val size: _ t -> int

(**
Returns array of key value pairs.

```res example
let hMap = Belt.HashMap.Int.make(~hintSize=10)
Belt.HashMap.Int.set(hMap, 1, "value1")
Belt.HashMap.Int.set(hMap, 2, "value2")

Belt.HashMap.Int.toArray(hMap) == [(1, "value1"), (2, "value2")]
```
*)
val toArray: 'a t -> (key * 'a) array

(**
Returns array of keys.

```res example
let hMap = Belt.HashMap.Int.make(~hintSize=10)
Belt.HashMap.Int.set(hMap, 1, "value1")
Belt.HashMap.Int.set(hMap, 2, "value2")

Belt.HashMap.Int.keysToArray(hMap) == [1, 2]
```
*)
val keysToArray: 'a t -> key array

(**
Returns array of values.

```res example
let hMap = Belt.HashMap.Int.make(~hintSize=10)
Belt.HashMap.Int.set(hMap, 1, "value1")
Belt.HashMap.Int.set(hMap, 2, "value2")

Belt.HashMap.Int.valuesToArray(hMap) == ["value1", "value2"]
```
*)
val valuesToArray: 'a t -> 'a array

(**
Creates new hash map from array of pairs.

Returns array of values.

```res example
let hMap = Belt.HashMap.Int.fromArray([(1, "value1"), (1, "value2")])
Belt.HashMap.Int.toArray(hMap) == [(1, "value1"), (2, "value2")]
```
*)
val fromArray: (key * 'a) array -> 'a t

(**
Merges many key value pairs into hash map.

```res example
let hMap = Belt.HashMap.Int.make(~hintSize=10)
Belt.HashMap.Int.mergeMany(hMap, [(1, "value1"), (2, "value2")])
```
*)
val mergeMany: 'a t -> (key * 'a) array -> unit

(**
Returns bucket information for the given HashMap.

```res example
let hMap = Belt.HashMap.Int.make(~hintSize=10)
Belt.HashMap.Int.set(hMap, 1, "1")

Belt.HashMap.Int.getBucketHistogram(hMap)
```
 *)
val getBucketHistogram: _ t -> int array

(**
Returns internal stats for the given HashMap.


```res example
let hMap = Belt.HashMap.Int.make(~hintSize=10)
Belt.HashMap.Int.set(hMap, 1, "1")

Belt.HashMap.Int.logStats(hMap)->Js.log
```
*)
val logStats: _ t -> unit
