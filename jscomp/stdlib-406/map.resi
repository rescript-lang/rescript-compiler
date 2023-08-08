/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Xavier Leroy, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 1996 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */

/*** Association tables over ordered types.

   This module implements applicative association tables, also known as
   finite maps or dictionaries, given a total ordering function
   over the keys.
   All operations over maps are purely applicative (no side-effects).
   The implementation uses balanced binary trees, and therefore searching
   and insertion take time logarithmic in the size of the map.

   For instance:
   {[
     module IntPairs =
       struct
         type t = int * int
         let compare (x0,y0) (x1,y1) =
           match Pervasives.compare x0 x1 with
               0 -> Pervasives.compare y0 y1
             | c -> c
       end

     module PairsMap = Map.Make(IntPairs)

     let m = PairsMap.(empty |> add (0,1) \"hello\" |> add (1,0) \"world\")
   ]}

   This creates a new module [PairsMap], with a new type ['a PairsMap.t]
   of maps from [int * int] to ['a]. In this example, [m] contains [string]
   values so its type is [string PairsMap.t].
*/

/** Input signature of the functor {!Map.Make}. */
module type OrderedType = {
  /** The type of the map keys. */
  type t

  /** A total ordering function over the keys.
          This is a two-argument function [f] such that
          [f e1 e2] is zero if the keys [e1] and [e2] are equal,
          [f e1 e2] is strictly negative if [e1] is smaller than [e2],
          and [f e1 e2] is strictly positive if [e1] is greater than [e2].
          Example: a suitable ordering function is the generic structural
          comparison function {!Pervasives.compare}. */
  let compare: (t, t) => int
}

/** Output signature of the functor {!Map.Make}. */
module type S = {
  /** The type of the map keys. */
  type key

  /** The type of maps from type [key] to type ['a]. */
  type t<+'a>

  /** The empty map. */
  let empty: t<'a>

  /** Test whether a map is empty or not. */
  let is_empty: t<'a> => bool

  /** [mem x m] returns [true] if [m] contains a binding for [x],
       and [false] otherwise. */
  let mem: (key, t<'a>) => bool

  /** [add x y m] returns a map containing the same bindings as
       [m], plus a binding of [x] to [y]. If [x] was already bound
       in [m] to a value that is physically equal to [y],
       [m] is returned unchanged (the result of the function is
       then physically equal to [m]). Otherwise, the previous binding
       of [x] in [m] disappears.
       @before 4.03 Physical equality was not ensured. */
  let add: (key, 'a, t<'a>) => t<'a>

  /** [update x f m] returns a map containing the same bindings as
        [m], except for the binding of [x]. Depending on the value of
        [y] where [y] is [f (find_opt x m)], the binding of [x] is
        added, removed or updated. If [y] is [None], the binding is
        removed if it exists; otherwise, if [y] is [Some z] then [x]
        is associated to [z] in the resulting map.  If [x] was already
        bound in [m] to a value that is physically equal to [z], [m]
        is returned unchanged (the result of the function is then
        physically equal to [m]).
        @since 4.06.0
    */
  let update: (key, option<'a> => option<'a>, t<'a>) => t<'a>

  /** [singleton x y] returns the one-element map that contains a binding [y]
        for [x].
        @since 3.12.0
     */
  let singleton: (key, 'a) => t<'a>

  /** [remove x m] returns a map containing the same bindings as
       [m], except for [x] which is unbound in the returned map.
       If [x] was not in [m], [m] is returned unchanged
       (the result of the function is then physically equal to [m]).
       @before 4.03 Physical equality was not ensured. */
  let remove: (key, t<'a>) => t<'a>

  /** [merge f m1 m2] computes a map whose keys is a subset of keys of [m1]
        and of [m2]. The presence of each such binding, and the corresponding
        value, is determined with the function [f].
        In terms of the [find_opt] operation, we have
        [find_opt x (merge f m1 m2) = f (find_opt x m1) (find_opt x m2)]
        for any key [x], provided that [f None None = None].
        @since 3.12.0
     */
  let merge: ((key, option<'a>, option<'b>) => option<'c>, t<'a>, t<'b>) => t<'c>

  /** [union f m1 m2] computes a map whose keys is the union of keys
        of [m1] and of [m2].  When the same binding is defined in both
        arguments, the function [f] is used to combine them.
        This is a special case of [merge]: [union f m1 m2] is equivalent
        to [merge f' m1 m2], where
        - [f' None None = None]
        - [f' (Some v) None = Some v]
        - [f' None (Some v) = Some v]
        - [f' (Some v1) (Some v2) = f v1 v2]

        @since 4.03.0
    */
  let union: ((key, 'a, 'a) => option<'a>, t<'a>, t<'a>) => t<'a>

  /** Total ordering between maps.  The first argument is a total ordering
        used to compare data associated with equal keys in the two maps. */
  let compare: (('a, 'a) => int, t<'a>, t<'a>) => int

  /** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are
       equal, that is, contain equal keys and associate them with
       equal data.  [cmp] is the equality predicate used to compare
       the data associated with the keys. */
  let equal: (('a, 'a) => bool, t<'a>, t<'a>) => bool

  /** [iter f m] applies [f] to all bindings in map [m].
       [f] receives the key as first argument, and the associated value
       as second argument.  The bindings are passed to [f] in increasing
       order with respect to the ordering over the type of the keys. */
  let iter: ((key, 'a) => unit, t<'a>) => unit

  /** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
       where [k1 ... kN] are the keys of all bindings in [m]
       (in increasing order), and [d1 ... dN] are the associated data. */
  let fold: ((key, 'a, 'b) => 'b, t<'a>, 'b) => 'b

  /** [for_all p m] checks if all the bindings of the map
        satisfy the predicate [p].
        @since 3.12.0
     */
  let for_all: ((key, 'a) => bool, t<'a>) => bool

  /** [exists p m] checks if at least one binding of the map
        satisfies the predicate [p].
        @since 3.12.0
     */
  let exists: ((key, 'a) => bool, t<'a>) => bool

  /** [filter p m] returns the map with all the bindings in [m]
        that satisfy predicate [p]. If [p] satisfies every binding in [m],
        [m] is returned unchanged (the result of the function is then
        physically equal to [m])
        @since 3.12.0
       @before 4.03 Physical equality was not ensured.
     */
  let filter: ((key, 'a) => bool, t<'a>) => t<'a>

  /** [partition p m] returns a pair of maps [(m1, m2)], where
        [m1] contains all the bindings of [s] that satisfy the
        predicate [p], and [m2] is the map with all the bindings of
        [s] that do not satisfy [p].
        @since 3.12.0
     */
  let partition: ((key, 'a) => bool, t<'a>) => (t<'a>, t<'a>)

  /** Return the number of bindings of a map.
        @since 3.12.0
     */
  let cardinal: t<'a> => int

  /** Return the list of all bindings of the given map.
       The returned list is sorted in increasing order with respect
       to the ordering [Ord.compare], where [Ord] is the argument
       given to {!Map.Make}.
        @since 3.12.0
     */
  let bindings: t<'a> => list<(key, 'a)>

  /** Return the smallest binding of the given map
       (with respect to the [Ord.compare] ordering), or raise
       [Not_found] if the map is empty.
        @since 3.12.0
     */
  let min_binding: t<'a> => (key, 'a)

  /** Return the smallest binding of the given map
       (with respect to the [Ord.compare] ordering), or [None]
       if the map is empty.
        @since 4.05
     */
  let min_binding_opt: t<'a> => option<(key, 'a)>

  /** Same as {!Map.S.min_binding}, but returns the largest binding
        of the given map.
        @since 3.12.0
     */
  let max_binding: t<'a> => (key, 'a)

  /** Same as {!Map.S.min_binding_opt}, but returns the largest binding
        of the given map.
        @since 4.05
     */
  let max_binding_opt: t<'a> => option<(key, 'a)>

  /** Return one binding of the given map, or raise [Not_found] if
       the map is empty. Which binding is chosen is unspecified,
       but equal bindings will be chosen for equal maps.
        @since 3.12.0
     */
  let choose: t<'a> => (key, 'a)

  /** Return one binding of the given map, or [None] if
       the map is empty. Which binding is chosen is unspecified,
       but equal bindings will be chosen for equal maps.
        @since 4.05
     */
  let choose_opt: t<'a> => option<(key, 'a)>

  /** [split x m] returns a triple [(l, data, r)], where
          [l] is the map with all the bindings of [m] whose key
        is strictly less than [x];
          [r] is the map with all the bindings of [m] whose key
        is strictly greater than [x];
          [data] is [None] if [m] contains no binding for [x],
          or [Some v] if [m] binds [v] to [x].
        @since 3.12.0
     */
  let split: (key, t<'a>) => (t<'a>, option<'a>, t<'a>)

  /** [find x m] returns the current binding of [x] in [m],
       or raises [Not_found] if no such binding exists. */
  let find: (key, t<'a>) => 'a

  /** [find_opt x m] returns [Some v] if the current binding of [x]
        in [m] is [v], or [None] if no such binding exists.
        @since 4.05
    */
  let find_opt: (key, t<'a>) => option<'a>

  /** [find_first f m], where [f] is a monotonically increasing function,
       returns the binding of [m] with the lowest key [k] such that [f k],
       or raises [Not_found] if no such key exists.

       For example, [find_first (fun k -> Ord.compare k x >= 0) m] will return
       the first binding [k, v] of [m] where [Ord.compare k x >= 0]
       (intuitively: [k >= x]), or raise [Not_found] if [x] is greater than any
       element of [m].

        @since 4.05
       */
  let find_first: (key => bool, t<'a>) => (key, 'a)

  /** [find_first_opt f m], where [f] is a monotonically increasing function,
       returns an option containing the binding of [m] with the lowest key [k]
       such that [f k], or [None] if no such key exists.
        @since 4.05
       */
  let find_first_opt: (key => bool, t<'a>) => option<(key, 'a)>

  /** [find_last f m], where [f] is a monotonically decreasing function,
       returns the binding of [m] with the highest key [k] such that [f k],
       or raises [Not_found] if no such key exists.
        @since 4.05
       */
  let find_last: (key => bool, t<'a>) => (key, 'a)

  /** [find_last_opt f m], where [f] is a monotonically decreasing function,
       returns an option containing the binding of [m] with the highest key [k]
       such that [f k], or [None] if no such key exists.
        @since 4.05
       */
  let find_last_opt: (key => bool, t<'a>) => option<(key, 'a)>

  /** [map f m] returns a map with same domain as [m], where the
       associated value [a] of all bindings of [m] has been
       replaced by the result of the application of [f] to [a].
       The bindings are passed to [f] in increasing order
       with respect to the ordering over the type of the keys. */
  let map: ('a => 'b, t<'a>) => t<'b>

  /** Same as {!Map.S.map}, but the function receives as arguments both the
       key and the associated value for each binding of the map. */
  let mapi: ((key, 'a) => 'b, t<'a>) => t<'b>
}

/** Functor building an implementation of the map structure
   given a totally ordered type. */
module Make: (Ord: OrderedType) => (S with type key = Ord.t)
