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

/* Sets over ordered types */

module type OrderedType = {
  type t
  let compare: (t, t) => int
}

module type S = {
  type elt
  type t
  let empty: t
  let is_empty: t => bool
  let mem: (elt, t) => bool
  let add: (elt, t) => t
  let singleton: elt => t
  let remove: (elt, t) => t
  let union: (t, t) => t
  let inter: (t, t) => t
  let diff: (t, t) => t
  let compare: (t, t) => int
  let equal: (t, t) => bool
  let subset: (t, t) => bool
  let iter: (~f: elt => unit, t) => unit
  let map: (~f: elt => elt, t) => t
  let fold: (~f: (elt, 'a) => 'a, t, ~init: 'a) => 'a
  let for_all: (~f: elt => bool, t) => bool
  let exists: (~f: elt => bool, t) => bool
  let filter: (~f: elt => bool, t) => t
  let partition: (~f: elt => bool, t) => (t, t)
  let cardinal: t => int
  let elements: t => list<elt>
  let min_elt: t => elt
  let min_elt_opt: t => option<elt>
  let max_elt: t => elt
  let max_elt_opt: t => option<elt>
  let choose: t => elt
  let choose_opt: t => option<elt>
  let split: (elt, t) => (t, bool, t)
  let find: (elt, t) => elt
  let find_opt: (elt, t) => option<elt>
  let find_first: (~f: elt => bool, t) => elt
  let find_first_opt: (~f: elt => bool, t) => option<elt>
  let find_last: (~f: elt => bool, t) => elt
  let find_last_opt: (~f: elt => bool, t) => option<elt>
  let of_list: list<elt> => t
}

module Make = (Ord: OrderedType) => {
  type elt = Ord.t
  type rec t = Empty | Node({l: t, v: elt, r: t, h: int})

  /* Sets are represented by balanced binary trees (the heights of the
   children differ by at most 2 */

  let height = param =>
    switch param {
    | Empty => 0
    | Node({h}) => h
    }

  /* Creates a new node with left son l, value v and right son r.
       We must have all elements of l < v < all elements of r.
       l and r must be balanced and | height l - height r | <= 2.
       Inline expansion of height for better speed. */

  let create = (l, v, r) => {
    let hl = switch l {
    | Empty => 0
    | Node({h}) => h
    }
    let hr = switch r {
    | Empty => 0
    | Node({h}) => h
    }
    Node({
      l,
      v,
      r,
      h: if hl >= hr {
        hl + 1
      } else {
        hr + 1
      },
    })
  }

  /* Same as create, but performs one step of rebalancing if necessary.
       Assumes l and r balanced and | height l - height r | <= 3.
       Inline expansion of create for better speed in the most frequent case
       where no rebalancing is required. */

  let bal = (l, v, r) => {
    let hl = switch l {
    | Empty => 0
    | Node({h}) => h
    }
    let hr = switch r {
    | Empty => 0
    | Node({h}) => h
    }
    if hl > hr + 2 {
      switch l {
      | Empty => invalid_arg("Set.bal")
      | Node({l: ll, v: lv, r: lr}) =>
        if height(ll) >= height(lr) {
          create(ll, lv, create(lr, v, r))
        } else {
          switch lr {
          | Empty => invalid_arg("Set.bal")
          | Node({l: lrl, v: lrv, r: lrr}) => create(create(ll, lv, lrl), lrv, create(lrr, v, r))
          }
        }
      }
    } else if hr > hl + 2 {
      switch r {
      | Empty => invalid_arg("Set.bal")
      | Node({l: rl, v: rv, r: rr}) =>
        if height(rr) >= height(rl) {
          create(create(l, v, rl), rv, rr)
        } else {
          switch rl {
          | Empty => invalid_arg("Set.bal")
          | Node({l: rll, v: rlv, r: rlr}) => create(create(l, v, rll), rlv, create(rlr, rv, rr))
          }
        }
      }
    } else {
      Node({
        l,
        v,
        r,
        h: if hl >= hr {
          hl + 1
        } else {
          hr + 1
        },
      })
    }
  }

  /* Insertion of one element */

  let rec add = (x, param) =>
    switch param {
    | Empty => Node({l: Empty, v: x, r: Empty, h: 1})
    | Node({l, v, r}) as t =>
      let c = Ord.compare(x, v)
      if c == 0 {
        t
      } else if c < 0 {
        let ll = add(x, l)
        if l === ll {
          t
        } else {
          bal(ll, v, r)
        }
      } else {
        let rr = add(x, r)
        if r === rr {
          t
        } else {
          bal(l, v, rr)
        }
      }
    }

  let singleton = x => Node({l: Empty, v: x, r: Empty, h: 1})

  /* Beware: those two functions assume that the added v is *strictly*
       smaller (or bigger) than all the present elements in the tree; it
       does not test for equality with the current min (or max) element.
       Indeed, they are only used during the "join" operation which
       respects this precondition.
 */

  let rec add_min_element = (x, param) =>
    switch param {
    | Empty => singleton(x)
    | Node({l, v, r}) => bal(add_min_element(x, l), v, r)
    }

  let rec add_max_element = (x, param) =>
    switch param {
    | Empty => singleton(x)
    | Node({l, v, r}) => bal(l, v, add_max_element(x, r))
    }

  /* Same as create and bal, but no assumptions are made on the
   relative heights of l and r. */

  let rec join = (l, v, r) =>
    switch (l, r) {
    | (Empty, _) => add_min_element(v, r)
    | (_, Empty) => add_max_element(v, l)
    | (Node({l: ll, v: lv, r: lr, h: lh}), Node({l: rl, v: rv, r: rr, h: rh})) =>
      if lh > rh + 2 {
        bal(ll, lv, join(lr, v, r))
      } else if rh > lh + 2 {
        bal(join(l, v, rl), rv, rr)
      } else {
        create(l, v, r)
      }
    }

  /* Smallest and greatest element of a set */

  let rec min_elt = param =>
    switch param {
    | Empty => raise(Not_found)
    | Node({l: Empty, v}) => v
    | Node({l}) => min_elt(l)
    }

  let rec min_elt_opt = param =>
    switch param {
    | Empty => None
    | Node({l: Empty, v}) => Some(v)
    | Node({l}) => min_elt_opt(l)
    }

  let rec max_elt = param =>
    switch param {
    | Empty => raise(Not_found)
    | Node({v, r: Empty}) => v
    | Node({r}) => max_elt(r)
    }

  let rec max_elt_opt = param =>
    switch param {
    | Empty => None
    | Node({v, r: Empty}) => Some(v)
    | Node({r}) => max_elt_opt(r)
    }

  /* Remove the smallest element of the given set */

  let rec remove_min_elt = param =>
    switch param {
    | Empty => invalid_arg("Set.remove_min_elt")
    | Node({l: Empty, r}) => r
    | Node({l, v, r}) => bal(remove_min_elt(l), v, r)
    }

  /* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       Assume | height l - height r | <= 2. */

  let merge = (t1, t2) =>
    switch (t1, t2) {
    | (Empty, t) => t
    | (t, Empty) => t
    | (_, _) => bal(t1, min_elt(t2), remove_min_elt(t2))
    }

  /* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       No assumption on the heights of l and r. */

  let concat = (t1, t2) =>
    switch (t1, t2) {
    | (Empty, t) => t
    | (t, Empty) => t
    | (_, _) => join(t1, min_elt(t2), remove_min_elt(t2))
    }

  /* Splitting.  split x s returns a triple (l, present, r) where
        - l is the set of elements of s that are < x
        - r is the set of elements of s that are > x
        - present is false if s contains no element equal to x,
          or true if s contains an element equal to x. */

  let rec split = (x, param) =>
    switch param {
    | Empty => (Empty, false, Empty)
    | Node({l, v, r}) =>
      let c = Ord.compare(x, v)
      if c == 0 {
        (l, true, r)
      } else if c < 0 {
        let (ll, pres, rl) = split(x, l)
        (ll, pres, join(rl, v, r))
      } else {
        let (lr, pres, rr) = split(x, r)
        (join(l, v, lr), pres, rr)
      }
    }

  /* Implementation of the set operations */

  let empty = Empty

  let is_empty = param =>
    switch param {
    | Empty => true
    | _ => false
    }

  let rec mem = (x, param) =>
    switch param {
    | Empty => false
    | Node({l, v, r}) =>
      let c = Ord.compare(x, v)
      c == 0 ||
        mem(
          x,
          if c < 0 {
            l
          } else {
            r
          },
        )
    }

  let rec remove = (x, param) =>
    switch param {
    | Empty => Empty
    | Node({l, v, r}) as t =>
      let c = Ord.compare(x, v)
      if c == 0 {
        merge(l, r)
      } else if c < 0 {
        let ll = remove(x, l)
        if l === ll {
          t
        } else {
          bal(ll, v, r)
        }
      } else {
        let rr = remove(x, r)
        if r === rr {
          t
        } else {
          bal(l, v, rr)
        }
      }
    }

  let rec union = (s1, s2) =>
    switch (s1, s2) {
    | (Empty, t2) => t2
    | (t1, Empty) => t1
    | (Node({l: l1, v: v1, r: r1, h: h1}), Node({l: l2, v: v2, r: r2, h: h2})) =>
      if h1 >= h2 {
        if h2 == 1 {
          add(v2, s1)
        } else {
          let (l2, _, r2) = split(v1, s2)
          join(union(l1, l2), v1, union(r1, r2))
        }
      } else if h1 == 1 {
        add(v1, s2)
      } else {
        let (l1, _, r1) = split(v2, s1)
        join(union(l1, l2), v2, union(r1, r2))
      }
    }

  let rec inter = (s1, s2) =>
    switch (s1, s2) {
    | (Empty, _) => Empty
    | (_, Empty) => Empty
    | (Node({l: l1, v: v1, r: r1}), t2) =>
      switch split(v1, t2) {
      | (l2, false, r2) => concat(inter(l1, l2), inter(r1, r2))
      | (l2, true, r2) => join(inter(l1, l2), v1, inter(r1, r2))
      }
    }

  let rec diff = (s1, s2) =>
    switch (s1, s2) {
    | (Empty, _) => Empty
    | (t1, Empty) => t1
    | (Node({l: l1, v: v1, r: r1}), t2) =>
      switch split(v1, t2) {
      | (l2, false, r2) => join(diff(l1, l2), v1, diff(r1, r2))
      | (l2, true, r2) => concat(diff(l1, l2), diff(r1, r2))
      }
    }

  type rec enumeration = End | More(elt, t, enumeration)

  let rec cons_enum = (s, e) =>
    switch s {
    | Empty => e
    | Node({l, v, r}) => cons_enum(l, More(v, r, e))
    }

  let rec compare_aux = (e1, e2) =>
    switch (e1, e2) {
    | (End, End) => 0
    | (End, _) => -1
    | (_, End) => 1
    | (More(v1, r1, e1), More(v2, r2, e2)) =>
      let c = Ord.compare(v1, v2)
      if c != 0 {
        c
      } else {
        compare_aux(cons_enum(r1, e1), cons_enum(r2, e2))
      }
    }

  let compare = (s1, s2) => compare_aux(cons_enum(s1, End), cons_enum(s2, End))

  let equal = (s1, s2) => compare(s1, s2) == 0

  let rec subset = (s1, s2) =>
    switch (s1, s2) {
    | (Empty, _) => true
    | (_, Empty) => false
    | (Node({l: l1, v: v1, r: r1}), Node({l: l2, v: v2, r: r2}) as t2) =>
      let c = Ord.compare(v1, v2)
      if c == 0 {
        subset(l1, l2) && subset(r1, r2)
      } else if c < 0 {
        subset(Node({l: l1, v: v1, r: Empty, h: 0}), l2) && subset(r1, t2)
      } else {
        subset(Node({l: Empty, v: v1, r: r1, h: 0}), r2) && subset(l1, t2)
      }
    }

  let rec iter = (~f, param) =>
    switch param {
    | Empty => ()
    | Node({l, v, r}) =>
      iter(~f, l)
      f(v)
      iter(~f, r)
    }

  let rec fold = (~f, s, ~init as accu) =>
    switch s {
    | Empty => accu
    | Node({l, v, r}) => fold(~f, r, ~init=f(v, fold(~f, l, ~init=accu)))
    }

  let rec for_all = (~f as p, param) =>
    switch param {
    | Empty => true
    | Node({l, v, r}) => p(v) && (for_all(~f=p, l) && for_all(~f=p, r))
    }

  let rec exists = (~f as p, param) =>
    switch param {
    | Empty => false
    | Node({l, v, r}) => p(v) || (exists(~f=p, l) || exists(~f=p, r))
    }

  let rec filter = (~f as p, param) =>
    switch param {
    | Empty => Empty
    | Node({l, v, r}) as t =>
      /* call [p] in the expected left-to-right order */
      let l' = filter(~f=p, l)
      let pv = p(v)
      let r' = filter(~f=p, r)
      if pv {
        if l === l' && r === r' {
          t
        } else {
          join(l', v, r')
        }
      } else {
        concat(l', r')
      }
    }

  let rec partition = (~f as p, param) =>
    switch param {
    | Empty => (Empty, Empty)
    | Node({l, v, r}) =>
      /* call [p] in the expected left-to-right order */
      let (lt, lf) = partition(~f=p, l)
      let pv = p(v)
      let (rt, rf) = partition(~f=p, r)
      if pv {
        (join(lt, v, rt), concat(lf, rf))
      } else {
        (concat(lt, rt), join(lf, v, rf))
      }
    }

  let rec cardinal = param =>
    switch param {
    | Empty => 0
    | Node({l, r}) => cardinal(l) + 1 + cardinal(r)
    }

  let rec elements_aux = (accu, param) =>
    switch param {
    | Empty => accu
    | Node({l, v, r}) => elements_aux(list{v, ...elements_aux(accu, r)}, l)
    }

  let elements = s => elements_aux(list{}, s)

  let choose = min_elt

  let choose_opt = min_elt_opt

  let rec find = (x, param) =>
    switch param {
    | Empty => raise(Not_found)
    | Node({l, v, r}) =>
      let c = Ord.compare(x, v)
      if c == 0 {
        v
      } else {
        find(
          x,
          if c < 0 {
            l
          } else {
            r
          },
        )
      }
    }

  let rec find_first_aux = (v0, f, param) =>
    switch param {
    | Empty => v0
    | Node({l, v, r}) =>
      if f(v) {
        find_first_aux(v, f, l)
      } else {
        find_first_aux(v0, f, r)
      }
    }

  let rec find_first = (~f, param) =>
    switch param {
    | Empty => raise(Not_found)
    | Node({l, v, r}) =>
      if f(v) {
        find_first_aux(v, f, l)
      } else {
        find_first(~f, r)
      }
    }

  let rec find_first_opt_aux = (v0, f, param) =>
    switch param {
    | Empty => Some(v0)
    | Node({l, v, r}) =>
      if f(v) {
        find_first_opt_aux(v, f, l)
      } else {
        find_first_opt_aux(v0, f, r)
      }
    }

  let rec find_first_opt = (~f, param) =>
    switch param {
    | Empty => None
    | Node({l, v, r}) =>
      if f(v) {
        find_first_opt_aux(v, f, l)
      } else {
        find_first_opt(~f, r)
      }
    }

  let rec find_last_aux = (v0, f, param) =>
    switch param {
    | Empty => v0
    | Node({l, v, r}) =>
      if f(v) {
        find_last_aux(v, f, r)
      } else {
        find_last_aux(v0, f, l)
      }
    }

  let rec find_last = (~f, param) =>
    switch param {
    | Empty => raise(Not_found)
    | Node({l, v, r}) =>
      if f(v) {
        find_last_aux(v, f, r)
      } else {
        find_last(~f, l)
      }
    }

  let rec find_last_opt_aux = (v0, f, param) =>
    switch param {
    | Empty => Some(v0)
    | Node({l, v, r}) =>
      if f(v) {
        find_last_opt_aux(v, f, r)
      } else {
        find_last_opt_aux(v0, f, l)
      }
    }

  let rec find_last_opt = (~f, param) =>
    switch param {
    | Empty => None
    | Node({l, v, r}) =>
      if f(v) {
        find_last_opt_aux(v, f, r)
      } else {
        find_last_opt(~f, l)
      }
    }

  let rec find_opt = (x, param) =>
    switch param {
    | Empty => None
    | Node({l, v, r}) =>
      let c = Ord.compare(x, v)
      if c == 0 {
        Some(v)
      } else {
        find_opt(
          x,
          if c < 0 {
            l
          } else {
            r
          },
        )
      }
    }

  let try_join = (l, v, r) =>
    /* [join l v r] can only be called when (elements of l < v <
         elements of r); use [try_join l v r] when this property may
         not hold, but you hope it does hold in the common case */
    if (
      (l == Empty || Ord.compare(max_elt(l), v) < 0) &&
        (r == Empty || Ord.compare(v, min_elt(r)) < 0)
    ) {
      join(l, v, r)
    } else {
      union(l, add(v, r))
    }

  let rec map = (~f, param) =>
    switch param {
    | Empty => Empty
    | Node({l, v, r}) as t =>
      /* enforce left-to-right evaluation order */
      let l' = map(~f, l)
      let v' = f(v)
      let r' = map(~f, r)
      if l === l' && (v === v' && r === r') {
        t
      } else {
        try_join(l', v', r')
      }
    }

  let of_sorted_list = l => {
    let rec sub = (n, l) =>
      switch (n, l) {
      | (0, l) => (Empty, l)
      | (1, list{x0, ...l}) => (Node({l: Empty, v: x0, r: Empty, h: 1}), l)
      | (2, list{x0, x1, ...l}) => (
          Node({l: Node({l: Empty, v: x0, r: Empty, h: 1}), v: x1, r: Empty, h: 2}),
          l,
        )
      | (3, list{x0, x1, x2, ...l}) => (
          Node({
            l: Node({l: Empty, v: x0, r: Empty, h: 1}),
            v: x1,
            r: Node({l: Empty, v: x2, r: Empty, h: 1}),
            h: 2,
          }),
          l,
        )
      | (n, l) =>
        let nl = n / 2
        let (left, l) = sub(nl, l)
        switch l {
        | list{} => assert(false)
        | list{mid, ...l} =>
          let (right, l) = sub(n - nl - 1, l)
          (create(left, mid, right), l)
        }
      }

    fst(sub(List.length(l), l))
  }

  let of_list = l =>
    switch l {
    | list{} => empty
    | list{x0} => singleton(x0)
    | list{x0, x1} => add(x1, singleton(x0))
    | list{x0, x1, x2} => add(x2, add(x1, singleton(x0)))
    | list{x0, x1, x2, x3} => add(x3, add(x2, add(x1, singleton(x0))))
    | list{x0, x1, x2, x3, x4} => add(x4, add(x3, add(x2, add(x1, singleton(x0)))))
    | _ => of_sorted_list(List.sort_uniq(Ord.compare, l))
    }
}
