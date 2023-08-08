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

/*** Sets over ordered types.

   This module implements the set data structure, given a total ordering
   function over the set elements. All operations over sets
   are purely applicative (no side-effects).
   The implementation uses balanced binary trees, and is therefore
   reasonably efficient: insertion and membership take time
   logarithmic in the size of the set, for instance.

   The {!Make} functor constructs implementations for any type, given a
   [compare] function.
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

     module PairsSet = Set.Make(IntPairs)

     let m = PairsSet.(empty |> add (2,3) |> add (5,7) |> add (11,13))
   ]}

   This creates a new module [PairsSet], with a new type [PairsSet.t]
   of sets of [int * int].
*/

/** Input signature of the functor {!Set.Make}. */
module type OrderedType = {
  /** The type of the set elements. */
  type t

  /** A total ordering function over the set elements.
          This is a two-argument function [f] such that
          [f e1 e2] is zero if the elements [e1] and [e2] are equal,
          [f e1 e2] is strictly negative if [e1] is smaller than [e2],
          and [f e1 e2] is strictly positive if [e1] is greater than [e2].
          Example: a suitable ordering function is the generic structural
          comparison function {!Pervasives.compare}. */
  let compare: (t, t) => int
}

/** Output signature of the functor {!Set.Make}. */
module type S = {
  /** The type of the set elements. */
  type elt

  /** The type of sets. */
  type t

  /** The empty set. */
  let empty: t

  /** Test whether a set is empty or not. */
  let is_empty: t => bool

  /** [mem x s] tests whether [x] belongs to the set [s]. */
  let mem: (elt, t) => bool

  /** [add x s] returns a set containing all elements of [s],
       plus [x]. If [x] was already in [s], [s] is returned unchanged
       (the result of the function is then physically equal to [s]).
       @before 4.03 Physical equality was not ensured. */
  let add: (elt, t) => t

  /** [singleton x] returns the one-element set containing only [x]. */
  let singleton: elt => t

  /** [remove x s] returns a set containing all elements of [s],
       except [x]. If [x] was not in [s], [s] is returned unchanged
       (the result of the function is then physically equal to [s]).
       @before 4.03 Physical equality was not ensured. */
  let remove: (elt, t) => t

  /** Set union. */
  let union: (t, t) => t

  /** Set intersection. */
  let inter: (t, t) => t

  /** Set difference. */
  let diff: (t, t) => t

  /** Total ordering between sets. Can be used as the ordering function
       for doing sets of sets. */
  let compare: (t, t) => int

  /** [equal s1 s2] tests whether the sets [s1] and [s2] are
       equal, that is, contain equal elements. */
  let equal: (t, t) => bool

  /** [subset s1 s2] tests whether the set [s1] is a subset of
       the set [s2]. */
  let subset: (t, t) => bool

  /** [iter f s] applies [f] in turn to all elements of [s].
       The elements of [s] are presented to [f] in increasing order
       with respect to the ordering over the type of the elements. */
  let iter: (elt => unit, t) => unit

  /** [map f s] is the set whose elements are [f a0],[f a1]... [f
        aN], where [a0],[a1]...[aN] are the elements of [s].

       The elements are passed to [f] in increasing order
       with respect to the ordering over the type of the elements.

       If no element of [s] is changed by [f], [s] is returned
       unchanged. (If each output of [f] is physically equal to its
       input, the returned set is physically equal to [s].)
       @since 4.04.0 */
  let map: (elt => elt, t) => t

  /** [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)],
       where [x1 ... xN] are the elements of [s], in increasing order. */
  let fold: ((elt, 'a) => 'a, t, 'a) => 'a

  /** [for_all p s] checks if all elements of the set
       satisfy the predicate [p]. */
  let for_all: (elt => bool, t) => bool

  /** [exists p s] checks if at least one element of
       the set satisfies the predicate [p]. */
  let exists: (elt => bool, t) => bool

  /** [filter p s] returns the set of all elements in [s]
       that satisfy predicate [p]. If [p] satisfies every element in [s],
       [s] is returned unchanged (the result of the function is then
       physically equal to [s]).
       @before 4.03 Physical equality was not ensured.*/
  let filter: (elt => bool, t) => t

  /** [partition p s] returns a pair of sets [(s1, s2)], where
       [s1] is the set of all the elements of [s] that satisfy the
       predicate [p], and [s2] is the set of all the elements of
       [s] that do not satisfy [p]. */
  let partition: (elt => bool, t) => (t, t)

  /** Return the number of elements of a set. */
  let cardinal: t => int

  /** Return the list of all elements of the given set.
       The returned list is sorted in increasing order with respect
       to the ordering [Ord.compare], where [Ord] is the argument
       given to {!Set.Make}. */
  let elements: t => list<elt>

  /** Return the smallest element of the given set
       (with respect to the [Ord.compare] ordering), or raise
       [Not_found] if the set is empty. */
  let min_elt: t => elt

  /** Return the smallest element of the given set
       (with respect to the [Ord.compare] ordering), or [None]
       if the set is empty.
        @since 4.05
    */
  let min_elt_opt: t => option<elt>

  /** Same as {!Set.S.min_elt}, but returns the largest element of the
       given set. */
  let max_elt: t => elt

  /** Same as {!Set.S.min_elt_opt}, but returns the largest element of the
        given set.
        @since 4.05
    */
  let max_elt_opt: t => option<elt>

  /** Return one element of the given set, or raise [Not_found] if
       the set is empty. Which element is chosen is unspecified,
       but equal elements will be chosen for equal sets. */
  let choose: t => elt

  /** Return one element of the given set, or [None] if
        the set is empty. Which element is chosen is unspecified,
        but equal elements will be chosen for equal sets.
        @since 4.05
    */
  let choose_opt: t => option<elt>

  /** [split x s] returns a triple [(l, present, r)], where
          [l] is the set of elements of [s] that are
          strictly less than [x];
          [r] is the set of elements of [s] that are
          strictly greater than [x];
          [present] is [false] if [s] contains no element equal to [x],
          or [true] if [s] contains an element equal to [x]. */
  let split: (elt, t) => (t, bool, t)

  /** [find x s] returns the element of [s] equal to [x] (according
        to [Ord.compare]), or raise [Not_found] if no such element
        exists.
        @since 4.01.0 */
  let find: (elt, t) => elt

  /** [find_opt x s] returns the element of [s] equal to [x] (according
        to [Ord.compare]), or [None] if no such element
        exists.
        @since 4.05 */
  let find_opt: (elt, t) => option<elt>

  /** [find_first f s], where [f] is a monotonically increasing function,
       returns the lowest element [e] of [s] such that [f e],
       or raises [Not_found] if no such element exists.

       For example, [find_first (fun e -> Ord.compare e x >= 0) s] will return
       the first element [e] of [s] where [Ord.compare e x >= 0] (intuitively:
       [e >= x]), or raise [Not_found] if [x] is greater than any element of
       [s].

        @since 4.05
       */
  let find_first: (elt => bool, t) => elt

  /** [find_first_opt f s], where [f] is a monotonically increasing function,
       returns an option containing the lowest element [e] of [s] such that
       [f e], or [None] if no such element exists.
        @since 4.05
       */
  let find_first_opt: (elt => bool, t) => option<elt>

  /** [find_last f s], where [f] is a monotonically decreasing function,
       returns the highest element [e] of [s] such that [f e],
       or raises [Not_found] if no such element exists.
        @since 4.05
       */
  let find_last: (elt => bool, t) => elt

  /** [find_last_opt f s], where [f] is a monotonically decreasing function,
       returns an option containing the highest element [e] of [s] such that
       [f e], or [None] if no such element exists.
        @since 4.05
       */
  let find_last_opt: (elt => bool, t) => option<elt>

  /** [of_list l] creates a set from a list of elements.
        This is usually more efficient than folding [add] over the list,
        except perhaps for lists with many duplicated elements.
        @since 4.02.0 */
  let of_list: list<elt> => t
}

/** Functor building an implementation of the set structure
   given a totally ordered type. */
module Make: (Ord: OrderedType) => (S with type elt = Ord.t)
