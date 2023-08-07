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

module type OrderedType = {
  type t
  let compare: (t, t) => int
}

module type S = {
  type key
  type t<+'a>
  let empty: t<'a>
  let is_empty: t<'a> => bool
  let mem: (key, t<'a>) => bool
  let add: (key, 'a, t<'a>) => t<'a>
  let update: (key, option<'a> => option<'a>, t<'a>) => t<'a>
  let singleton: (key, 'a) => t<'a>
  let remove: (key, t<'a>) => t<'a>
  let merge: ((key, option<'a>, option<'b>) => option<'c>, t<'a>, t<'b>) => t<'c>
  let union: ((key, 'a, 'a) => option<'a>, t<'a>, t<'a>) => t<'a>
  let compare: (('a, 'a) => int, t<'a>, t<'a>) => int
  let equal: (('a, 'a) => bool, t<'a>, t<'a>) => bool
  let iter: ((key, 'a) => unit, t<'a>) => unit
  let fold: ((key, 'a, 'b) => 'b, t<'a>, 'b) => 'b
  let for_all: ((key, 'a) => bool, t<'a>) => bool
  let exists: ((key, 'a) => bool, t<'a>) => bool
  let filter: ((key, 'a) => bool, t<'a>) => t<'a>
  let partition: ((key, 'a) => bool, t<'a>) => (t<'a>, t<'a>)
  let cardinal: t<'a> => int
  let bindings: t<'a> => list<(key, 'a)>
  let min_binding: t<'a> => (key, 'a)
  let min_binding_opt: t<'a> => option<(key, 'a)>
  let max_binding: t<'a> => (key, 'a)
  let max_binding_opt: t<'a> => option<(key, 'a)>
  let choose: t<'a> => (key, 'a)
  let choose_opt: t<'a> => option<(key, 'a)>
  let split: (key, t<'a>) => (t<'a>, option<'a>, t<'a>)
  let find: (key, t<'a>) => 'a
  let find_opt: (key, t<'a>) => option<'a>
  let find_first: (key => bool, t<'a>) => (key, 'a)
  let find_first_opt: (key => bool, t<'a>) => option<(key, 'a)>
  let find_last: (key => bool, t<'a>) => (key, 'a)
  let find_last_opt: (key => bool, t<'a>) => option<(key, 'a)>
  let map: ('a => 'b, t<'a>) => t<'b>
  let mapi: ((key, 'a) => 'b, t<'a>) => t<'b>
}

module Make = (Ord: OrderedType) => {
  type key = Ord.t

  type rec t<'a> =
    | Empty
    | Node({l: t<'a>, v: key, d: 'a, r: t<'a>, h: int})

  let height = param =>
    switch param {
    | Empty => 0
    | Node({h}) => h
    }

  let create = (l, x, d, r) => {
    let hl = height(l) and hr = height(r)
    Node({
      l,
      v: x,
      d,
      r,
      h: if hl >= hr {
        hl + 1
      } else {
        hr + 1
      },
    })
  }

  let singleton = (x, d) => Node({l: Empty, v: x, d, r: Empty, h: 1})

  let bal = (l, x, d, r) => {
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
      | Empty => invalid_arg("Map.bal")
      | Node({l: ll, v: lv, d: ld, r: lr}) =>
        if height(ll) >= height(lr) {
          create(ll, lv, ld, create(lr, x, d, r))
        } else {
          switch lr {
          | Empty => invalid_arg("Map.bal")
          | Node({l: lrl, v: lrv, d: lrd, r: lrr}) =>
            create(create(ll, lv, ld, lrl), lrv, lrd, create(lrr, x, d, r))
          }
        }
      }
    } else if hr > hl + 2 {
      switch r {
      | Empty => invalid_arg("Map.bal")
      | Node({l: rl, v: rv, d: rd, r: rr}) =>
        if height(rr) >= height(rl) {
          create(create(l, x, d, rl), rv, rd, rr)
        } else {
          switch rl {
          | Empty => invalid_arg("Map.bal")
          | Node({l: rll, v: rlv, d: rld, r: rlr}) =>
            create(create(l, x, d, rll), rlv, rld, create(rlr, rv, rd, rr))
          }
        }
      }
    } else {
      Node({
        l,
        v: x,
        d,
        r,
        h: if hl >= hr {
          hl + 1
        } else {
          hr + 1
        },
      })
    }
  }

  let empty = Empty

  let is_empty = param =>
    switch param {
    | Empty => true
    | _ => false
    }

  let rec add = (x, data, param) =>
    switch param {
    | Empty => Node({l: Empty, v: x, d: data, r: Empty, h: 1})
    | Node({l, v, d, r, h}) as m =>
      let c = Ord.compare(x, v)
      if c == 0 {
        if d === data {
          m
        } else {
          Node({l, v: x, d: data, r, h})
        }
      } else if c < 0 {
        let ll = add(x, data, l)
        if l === ll {
          m
        } else {
          bal(ll, v, d, r)
        }
      } else {
        let rr = add(x, data, r)
        if r === rr {
          m
        } else {
          bal(l, v, d, rr)
        }
      }
    }

  let rec find = (x, param) =>
    switch param {
    | Empty => raise(Not_found)
    | Node({l, v, d, r}) =>
      let c = Ord.compare(x, v)
      if c == 0 {
        d
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

  let rec find_first_aux = (v0, d0, f, param) =>
    switch param {
    | Empty => (v0, d0)
    | Node({l, v, d, r}) =>
      if f(v) {
        find_first_aux(v, d, f, l)
      } else {
        find_first_aux(v0, d0, f, r)
      }
    }

  let rec find_first = (f, param) =>
    switch param {
    | Empty => raise(Not_found)
    | Node({l, v, d, r}) =>
      if f(v) {
        find_first_aux(v, d, f, l)
      } else {
        find_first(f, r)
      }
    }

  let rec find_first_opt_aux = (v0, d0, f, param) =>
    switch param {
    | Empty => Some(v0, d0)
    | Node({l, v, d, r}) =>
      if f(v) {
        find_first_opt_aux(v, d, f, l)
      } else {
        find_first_opt_aux(v0, d0, f, r)
      }
    }

  let rec find_first_opt = (f, param) =>
    switch param {
    | Empty => None
    | Node({l, v, d, r}) =>
      if f(v) {
        find_first_opt_aux(v, d, f, l)
      } else {
        find_first_opt(f, r)
      }
    }

  let rec find_last_aux = (v0, d0, f, param) =>
    switch param {
    | Empty => (v0, d0)
    | Node({l, v, d, r}) =>
      if f(v) {
        find_last_aux(v, d, f, r)
      } else {
        find_last_aux(v0, d0, f, l)
      }
    }

  let rec find_last = (f, param) =>
    switch param {
    | Empty => raise(Not_found)
    | Node({l, v, d, r}) =>
      if f(v) {
        find_last_aux(v, d, f, r)
      } else {
        find_last(f, l)
      }
    }

  let rec find_last_opt_aux = (v0, d0, f, param) =>
    switch param {
    | Empty => Some(v0, d0)
    | Node({l, v, d, r}) =>
      if f(v) {
        find_last_opt_aux(v, d, f, r)
      } else {
        find_last_opt_aux(v0, d0, f, l)
      }
    }

  let rec find_last_opt = (f, param) =>
    switch param {
    | Empty => None
    | Node({l, v, d, r}) =>
      if f(v) {
        find_last_opt_aux(v, d, f, r)
      } else {
        find_last_opt(f, l)
      }
    }

  let rec find_opt = (x, param) =>
    switch param {
    | Empty => None
    | Node({l, v, d, r}) =>
      let c = Ord.compare(x, v)
      if c == 0 {
        Some(d)
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

  let rec min_binding = param =>
    switch param {
    | Empty => raise(Not_found)
    | Node({l: Empty, v, d}) => (v, d)
    | Node({l}) => min_binding(l)
    }

  let rec min_binding_opt = param =>
    switch param {
    | Empty => None
    | Node({l: Empty, v, d}) => Some(v, d)
    | Node({l}) => min_binding_opt(l)
    }

  let rec max_binding = param =>
    switch param {
    | Empty => raise(Not_found)
    | Node({v, d, r: Empty}) => (v, d)
    | Node({r}) => max_binding(r)
    }

  let rec max_binding_opt = param =>
    switch param {
    | Empty => None
    | Node({v, d, r: Empty}) => Some(v, d)
    | Node({r}) => max_binding_opt(r)
    }

  let rec remove_min_binding = param =>
    switch param {
    | Empty => invalid_arg("Map.remove_min_elt")
    | Node({l: Empty, r}) => r
    | Node({l, v, d, r}) => bal(remove_min_binding(l), v, d, r)
    }

  let merge = (t1, t2) =>
    switch (t1, t2) {
    | (Empty, t) => t
    | (t, Empty) => t
    | (_, _) =>
      let (x, d) = min_binding(t2)
      bal(t1, x, d, remove_min_binding(t2))
    }

  let rec remove = (x, param) =>
    switch param {
    | Empty => Empty
    | Node({l, v, d, r}) as m =>
      let c = Ord.compare(x, v)
      if c == 0 {
        merge(l, r)
      } else if c < 0 {
        let ll = remove(x, l)
        if l === ll {
          m
        } else {
          bal(ll, v, d, r)
        }
      } else {
        let rr = remove(x, r)
        if r === rr {
          m
        } else {
          bal(l, v, d, rr)
        }
      }
    }

  let rec update = (x, f, param) =>
    switch param {
    | Empty =>
      switch f(None) {
      | None => Empty
      | Some(data) => Node({l: Empty, v: x, d: data, r: Empty, h: 1})
      }
    | Node({l, v, d, r, h}) as m =>
      let c = Ord.compare(x, v)
      if c == 0 {
        switch f(Some(d)) {
        | None => merge(l, r)
        | Some(data) =>
          if d === data {
            m
          } else {
            Node({l, v: x, d: data, r, h})
          }
        }
      } else if c < 0 {
        let ll = update(x, f, l)
        if l === ll {
          m
        } else {
          bal(ll, v, d, r)
        }
      } else {
        let rr = update(x, f, r)
        if r === rr {
          m
        } else {
          bal(l, v, d, rr)
        }
      }
    }

  let rec iter = (f, param) =>
    switch param {
    | Empty => ()
    | Node({l, v, d, r}) =>
      iter(f, l)
      f(v, d)
      iter(f, r)
    }

  let rec map = (f, param) =>
    switch param {
    | Empty => Empty
    | Node({l, v, d, r, h}) =>
      let l' = map(f, l)
      let d' = f(d)
      let r' = map(f, r)
      Node({l: l', v, d: d', r: r', h})
    }

  let rec mapi = (f, param) =>
    switch param {
    | Empty => Empty
    | Node({l, v, d, r, h}) =>
      let l' = mapi(f, l)
      let d' = f(v, d)
      let r' = mapi(f, r)
      Node({l: l', v, d: d', r: r', h})
    }

  let rec fold = (f, m, accu) =>
    switch m {
    | Empty => accu
    | Node({l, v, d, r}) => fold(f, r, f(v, d, fold(f, l, accu)))
    }

  let rec for_all = (p, param) =>
    switch param {
    | Empty => true
    | Node({l, v, d, r}) => p(v, d) && (for_all(p, l) && for_all(p, r))
    }

  let rec exists = (p, param) =>
    switch param {
    | Empty => false
    | Node({l, v, d, r}) => p(v, d) || (exists(p, l) || exists(p, r))
    }

  /* Beware: those two functions assume that the added k is *strictly*
       smaller (or bigger) than all the present keys in the tree; it
       does not test for equality with the current min (or max) key.

       Indeed, they are only used during the "join" operation which
       respects this precondition.
 */

  let rec add_min_binding = (k, x, param) =>
    switch param {
    | Empty => singleton(k, x)
    | Node({l, v, d, r}) => bal(add_min_binding(k, x, l), v, d, r)
    }

  let rec add_max_binding = (k, x, param) =>
    switch param {
    | Empty => singleton(k, x)
    | Node({l, v, d, r}) => bal(l, v, d, add_max_binding(k, x, r))
    }

  /* Same as create and bal, but no assumptions are made on the
   relative heights of l and r. */

  let rec join = (l, v, d, r) =>
    switch (l, r) {
    | (Empty, _) => add_min_binding(v, d, r)
    | (_, Empty) => add_max_binding(v, d, l)
    | (Node({l: ll, v: lv, d: ld, r: lr, h: lh}), Node({l: rl, v: rv, d: rd, r: rr, h: rh})) =>
      if lh > rh + 2 {
        bal(ll, lv, ld, join(lr, v, d, r))
      } else if rh > lh + 2 {
        bal(join(l, v, d, rl), rv, rd, rr)
      } else {
        create(l, v, d, r)
      }
    }

  /* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       No assumption on the heights of l and r. */

  let concat = (t1, t2) =>
    switch (t1, t2) {
    | (Empty, t) => t
    | (t, Empty) => t
    | (_, _) =>
      let (x, d) = min_binding(t2)
      join(t1, x, d, remove_min_binding(t2))
    }

  let concat_or_join = (t1, v, d, t2) =>
    switch d {
    | Some(d) => join(t1, v, d, t2)
    | None => concat(t1, t2)
    }

  let rec split = (x, param) =>
    switch param {
    | Empty => (Empty, None, Empty)
    | Node({l, v, d, r}) =>
      let c = Ord.compare(x, v)
      if c == 0 {
        (l, Some(d), r)
      } else if c < 0 {
        let (ll, pres, rl) = split(x, l)
        (ll, pres, join(rl, v, d, r))
      } else {
        let (lr, pres, rr) = split(x, r)
        (join(l, v, d, lr), pres, rr)
      }
    }

  let rec merge = (f, s1, s2) =>
    switch (s1, s2) {
    | (Empty, Empty) => Empty
    | (Node({l: l1, v: v1, d: d1, r: r1, h: h1}), _) if h1 >= height(s2) =>
      let (l2, d2, r2) = split(v1, s2)
      concat_or_join(merge(f, l1, l2), v1, f(v1, Some(d1), d2), merge(f, r1, r2))
    | (_, Node({l: l2, v: v2, d: d2, r: r2})) =>
      let (l1, d1, r1) = split(v2, s1)
      concat_or_join(merge(f, l1, l2), v2, f(v2, d1, Some(d2)), merge(f, r1, r2))
    | _ => assert(false)
    }

  let rec union = (f, s1, s2) =>
    switch (s1, s2) {
    | (Empty, s) | (s, Empty) => s
    | (Node({l: l1, v: v1, d: d1, r: r1, h: h1}), Node({l: l2, v: v2, d: d2, r: r2, h: h2})) =>
      if h1 >= h2 {
        let (l2, d2, r2) = split(v1, s2)
        let l = union(f, l1, l2) and r = union(f, r1, r2)
        switch d2 {
        | None => join(l, v1, d1, r)
        | Some(d2) => concat_or_join(l, v1, f(v1, d1, d2), r)
        }
      } else {
        let (l1, d1, r1) = split(v2, s1)
        let l = union(f, l1, l2) and r = union(f, r1, r2)
        switch d1 {
        | None => join(l, v2, d2, r)
        | Some(d1) => concat_or_join(l, v2, f(v2, d1, d2), r)
        }
      }
    }

  let rec filter = (p, param) =>
    switch param {
    | Empty => Empty
    | Node({l, v, d, r}) as m =>
      /* call [p] in the expected left-to-right order */
      let l' = filter(p, l)
      let pvd = p(v, d)
      let r' = filter(p, r)
      if pvd {
        if l === l' && r === r' {
          m
        } else {
          join(l', v, d, r')
        }
      } else {
        concat(l', r')
      }
    }

  let rec partition = (p, param) =>
    switch param {
    | Empty => (Empty, Empty)
    | Node({l, v, d, r}) =>
      /* call [p] in the expected left-to-right order */
      let (lt, lf) = partition(p, l)
      let pvd = p(v, d)
      let (rt, rf) = partition(p, r)
      if pvd {
        (join(lt, v, d, rt), concat(lf, rf))
      } else {
        (concat(lt, rt), join(lf, v, d, rf))
      }
    }

  type rec enumeration<'a> = End | More(key, 'a, t<'a>, enumeration<'a>)

  let rec cons_enum = (m, e) =>
    switch m {
    | Empty => e
    | Node({l, v, d, r}) => cons_enum(l, More(v, d, r, e))
    }

  let compare = (cmp, m1, m2) => {
    let rec compare_aux = (e1, e2) =>
      switch (e1, e2) {
      | (End, End) => 0
      | (End, _) => -1
      | (_, End) => 1
      | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) =>
        let c = Ord.compare(v1, v2)
        if c != 0 {
          c
        } else {
          let c = cmp(d1, d2)
          if c != 0 {
            c
          } else {
            compare_aux(cons_enum(r1, e1), cons_enum(r2, e2))
          }
        }
      }
    compare_aux(cons_enum(m1, End), cons_enum(m2, End))
  }

  let equal = (cmp, m1, m2) => {
    let rec equal_aux = (e1, e2) =>
      switch (e1, e2) {
      | (End, End) => true
      | (End, _) => false
      | (_, End) => false
      | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) =>
        Ord.compare(v1, v2) == 0 && (cmp(d1, d2) && equal_aux(cons_enum(r1, e1), cons_enum(r2, e2)))
      }
    equal_aux(cons_enum(m1, End), cons_enum(m2, End))
  }

  let rec cardinal = param =>
    switch param {
    | Empty => 0
    | Node({l, r}) => cardinal(l) + 1 + cardinal(r)
    }

  let rec bindings_aux = (accu, param) =>
    switch param {
    | Empty => accu
    | Node({l, v, d, r}) => bindings_aux(list{(v, d), ...bindings_aux(accu, r)}, l)
    }

  let bindings = s => bindings_aux(list{}, s)

  let choose = min_binding

  let choose_opt = min_binding_opt
}
