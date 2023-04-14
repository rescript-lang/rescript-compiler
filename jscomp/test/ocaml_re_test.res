@@bs.config({flags: ["-w", "a", "-bs-no-bin-annot"], no_export})
let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => {
  incr(test_id)
  suites :=
    list{(loc ++ (" id " ++ string_of_int(test_id.contents)), _ => Mt.Eq(x, y)), ...suites.contents}
}

module Re_cset: {
  /*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*/

  /* Character sets, represented as sorted list of intervals */

  type c = int
  type t

  let iter: (t, ~f: (c, c) => unit) => unit

  let union: (t, t) => t
  let inter: (t, t) => t
  let diff: (t, t) => t
  let offset: (int, t) => t

  let empty: t
  let single: c => t
  let seq: (c, c) => t
  let add: (c, t) => t

  let mem: (c, t) => bool

  type hash
  let hash: t => hash

  let one_char: t => option<c>

  let fold_right: (t, ~init: 'acc, ~f: ((c, c), 'acc) => 'acc) => 'acc

  let hash_rec: t => int

  module CSetMap: Map.S with type key = (int, t)

  let cany: t

  let csingle: char => t

  let is_empty: t => bool

  let prepend: (t, list<'a>, list<(t, list<'a>)>) => list<(t, list<'a>)>

  let pick: t => c
} = {
  /*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*/

  type c = int
  type t = list<(c, c)>

  let rec union = (l, l') =>
    switch (l, l') {
    | (_, list{}) => l
    | (list{}, _) => l'
    | (list{(c1, c2), ...r}, list{(c1', c2'), ...r'}) =>
      if c2 + 1 < c1' {
        list{(c1, c2), ...union(r, l')}
      } else if c2' + 1 < c1 {
        list{(c1', c2'), ...union(l, r')}
      } else if c2 < c2' {
        union(r, list{(min(c1, c1'), c2'), ...r'})
      } else {
        union(list{(min(c1, c1'), c2), ...r}, r')
      }
    }

  let rec inter = (l, l') =>
    switch (l, l') {
    | (_, list{}) => list{}
    | (list{}, _) => list{}
    | (list{(c1, c2), ...r}, list{(c1', c2'), ...r'}) =>
      if c2 < c1' {
        inter(r, l')
      } else if c2' < c1 {
        inter(l, r')
      } else if c2 < c2' {
        list{(max(c1, c1'), c2), ...inter(r, l')}
      } else {
        list{(max(c1, c1'), c2'), ...inter(l, r')}
      }
    }

  let rec diff = (l, l') =>
    switch (l, l') {
    | (_, list{}) => l
    | (list{}, _) => list{}
    | (list{(c1, c2), ...r}, list{(c1', c2'), ...r'}) =>
      if c2 < c1' {
        list{(c1, c2), ...diff(r, l')}
      } else if c2' < c1 {
        diff(l, r')
      } else {
        let r'' = if c2' < c2 {
          list{(c2' + 1, c2), ...r}
        } else {
          r
        }
        if c1 < c1' {
          list{(c1, c1' - 1), ...diff(r'', r')}
        } else {
          diff(r'', r')
        }
      }
    }

  let single = c => list{(c, c)}

  let add = (c, l) => union(single(c), l)

  let seq = (c, c') =>
    if c <= c' {
      list{(c, c')}
    } else {
      list{(c', c)}
    }

  let rec offset = (o, l) =>
    switch l {
    | list{} => list{}
    | list{(c1, c2), ...r} => list{(c1 + o, c2 + o), ...offset(o, r)}
    }

  let empty = list{}

  let rec mem = (c: int, s) =>
    switch s {
    | list{} => false
    | list{(c1, c2), ...rem} =>
      if c <= c2 {
        c >= c1
      } else {
        mem(c, rem)
      }
    }

  /* ** */

  type hash = int

  let rec hash_rec = x =>
    switch x {
    | list{} => 0
    | list{(i, j), ...r} => i + 13 * j + 257 * hash_rec(r)
    }
  let hash = l => land(hash_rec(l), 0x3FFFFFFF)

  /* ** */

  /* let%ignore pp = Re_fmt.list print_one */

  let rec iter = (t, ~f) =>
    switch t {
    | list{} => ()
    | list{(x, y), ...xs} =>
      f(x, y)
      iter(xs, ~f)
    }

  let one_char = x =>
    switch x {
    | list{(i, j)} if i == j => Some(i)
    | _ => None
    }

  module CSetMap = Map.Make({
    type t = (int, list<(int, int)>)
    let compare = ((i, u), (j, v)) => {
      let c = compare(i, j)
      if c != 0 {
        c
      } else {
        compare(u, v)
      }
    }
  })

  let fold_right = (t, ~init, ~f) => List.fold_right(f, t, init)

  let csingle = c => single(Char.code(c))

  let cany = list{(0, 255)}

  let is_empty = x =>
    switch x {
    | list{} => true
    | _ => false
    }

  let rec prepend = (s, x, l) =>
    switch (s, l) {
    | (list{}, _) => l
    | (_r, list{}) => list{}
    | (list{(_c, c'), ...r}, list{(list{(d, _d')}, _x'), ..._r'}) if c' < d => prepend(r, x, l)
    | (list{(c, c'), ...r}, list{(list{(d, d')}, x'), ...r'}) =>
      if c <= d {
        if c' < d' {
          list{
            (list{(d, c')}, \"@"(x, x')),
            ...prepend(r, x, list{(list{(c' + 1, d')}, x'), ...r'}),
          }
        } else {
          list{(list{(d, d')}, \"@"(x, x')), ...prepend(s, x, r')}
        }
      } else if c > d' {
        list{(list{(d, d')}, x'), ...prepend(s, x, r')}
      } else {
        list{(list{(d, c - 1)}, x'), ...prepend(s, x, list{(list{(c, d')}, x'), ...r'})}
      }
    | _ => assert(false)
    }

  let pick = x =>
    switch x {
    | list{} => invalid_arg("Re_cset.pick")
    | list{(x, _), ..._} => x
    }
}
module Re_automata: {
  /*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*/

  /* Regular expressions */

  @ocaml.doc(" Categories represent the various kinds of characters that can be tested
    by look-ahead and look-behind operations.

    This is more restricted than Cset, but faster.
")
  module Category: {
    type t
    let \"++": (t, t) => t
    let from_char: char => t

    let inexistant: t
    let letter: t
    let not_letter: t
    let newline: t
    let lastnewline: t
    let search_boundary: t
  }

  type mark = int

  type sem = [#Longest | #Shortest | #First]
  type rep_kind = [#Greedy | #Non_greedy]

  module Pmark: {
    type t = private int
    let equal: (t, t) => bool
    let compare: (t, t) => int
    let gen: unit => t
  }

  type expr
  let is_eps: expr => bool

  type ids
  let create_ids: unit => ids

  let cst: (ids, Re_cset.t) => expr
  let empty: ids => expr
  let alt: (ids, list<expr>) => expr
  let seq: (ids, sem, expr, expr) => expr
  let eps: ids => expr
  let rep: (ids, rep_kind, sem, expr) => expr
  let mark: (ids, mark) => expr
  let pmark: (ids, Pmark.t) => expr
  let erase: (ids, mark, mark) => expr
  let before: (ids, Category.t) => expr
  let after: (ids, Category.t) => expr

  let rename: (ids, expr) => expr

  /* ** */

  module PmarkSet: Set.S with type elt = Pmark.t

  /* States of the automata */

  type idx = int
  module Marks: {
    type t = {
      marks: list<(mark, idx)>,
      pmarks: PmarkSet.t,
    }
  }

  module E: {
    type t
  }

  type hash
  type mark_infos = array<int>
  type status = Failed | Match(mark_infos, PmarkSet.t) | Running

  module State: {
    type t = {
      idx: idx,
      category: Category.t,
      desc: list<E.t>,
      mutable status: option<status>,
      hash: hash,
    }
    let dummy: t
    let create: (Category.t, expr) => t
    module Table: Hashtbl.S with type key = t
  }

  /* ** */

  /* Computation of the states following a given state */

  type working_area
  let create_working_area: unit => working_area
  let index_count: working_area => int

  let delta: (working_area, Category.t, Re_cset.c, State.t) => State.t
  let deriv: (
    working_area,
    Re_cset.t,
    list<(Category.t, Re_cset.t)>,
    State.t,
  ) => list<(Re_cset.t, State.t)>

  /* ** */

  let status: State.t => status
} = {
  /*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*/

  module Cset = Re_cset

  type sem = [#Longest | #Shortest | #First]

  type rep_kind = [#Greedy | #Non_greedy]

  module Category: {
    type t
    let equal: (t, t) => bool
    let compare: (t, t) => int

    let to_int: t => int

    let intersect: (t, t) => bool
    let \"++": (t, t) => t
    let from_char: char => t

    let dummy: t
    let inexistant: t
    let letter: t
    let not_letter: t
    let newline: t
    let lastnewline: t
    let search_boundary: t
  } = {
    type t = int
    let equal = (x: int, y: int) => x == y
    let compare = (x: int, y: int) => compare(x, y)
    let to_int = x => x

    let intersect = (x, y) => land(x, y) != 0
    let \"++" = (x, y) => lor(x, y)

    let dummy = -1
    let inexistant = 1
    let letter = 2
    let not_letter = 4
    let newline = 8
    let lastnewline = 16
    let search_boundary = 32

    let from_char = x =>
      /* Should match [cword] definition */
      switch x {
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '0' .. '9'
      | '_'
      | 'ª'
      | 'µ'
      | 'º'
      | 'À' .. 'Ö'
      | 'Ø' .. 'ö'
      | 'ø' .. 'ÿ' => letter
      | '\n' => \"++"(not_letter, newline)
      | _ => not_letter
      }
  }

  type mark = int
  type idx = int

  module Pmark: {
    type t = private int
    let equal: (t, t) => bool
    let compare: (t, t) => int
    let gen: unit => t
  } = {
    type t = int
    let equal = (x: int, y: int) => x == y
    let compare = (x: int, y: int) => compare(x, y)
    let r = ref(0)
    let gen = () => {
      incr(r)
      r.contents
    }
  }

  type rec expr = {id: int, def: def}

  and def =
    | Cst(Cset.t)
    | Alt(list<expr>)
    | Seq(sem, expr, expr)
    | Eps
    | Rep(rep_kind, sem, expr)
    | Mark(int)
    | Erase(int, int)
    | Before(Category.t)
    | After(Category.t)
    | Pmark(Pmark.t)

  module PmarkSet = Set.Make(Pmark)

  let hash_combine = (h, accu) => accu * 65599 + h

  module Marks = {
    type t = {
      marks: list<(int, int)>,
      pmarks: PmarkSet.t,
    }

    let empty = {marks: list{}, pmarks: PmarkSet.empty}

    let rec merge_marks_offset = (old, x) =>
      switch x {
      | list{} => old
      | list{(i, v), ...rem} =>
        let nw' = merge_marks_offset(List.remove_assq(i, old), rem)
        if v == -2 {
          nw'
        } else {
          list{(i, v), ...nw'}
        }
      }

    let merge = (old, nw) => {
      marks: merge_marks_offset(old.marks, nw.marks),
      pmarks: PmarkSet.union(old.pmarks, nw.pmarks),
    }

    let rec hash_marks_offset = (l, accu) =>
      switch l {
      | list{} => accu
      | list{(a, i), ...r} => hash_marks_offset(r, hash_combine(a, hash_combine(i, accu)))
      }

    let hash = (m, accu) => hash_marks_offset(m.marks, hash_combine(Hashtbl.hash(m.pmarks), accu))

    let rec marks_set_idx = (idx, x) =>
      switch x {
      | list{(a, -1), ...rem} => list{(a, idx), ...marks_set_idx(idx, rem)}
      | marks => marks
      }

    let marks_set_idx = (marks, idx) => {...marks, marks: marks_set_idx(idx, marks.marks)}
  }

  /* ** */

  /* let%ignore rec pp ch e =
  let open Re_fmt in
  match e.def with
    Cst l ->
    sexp ch "cst" Cset.pp l;
  | Alt l ->
    sexp ch "alt" (list pp) l
  | Seq (k, e, e') ->
    sexp ch "seq" (triple pp_sem pp pp) (k, e, e')
  | Eps ->
    str ch "eps"
  | Rep (_rk, k, e) ->
    sexp ch "rep" (pair pp_sem pp) (k, e)
  | Mark i ->
    sexp ch "mark" int i
  | Pmark i ->
    sexp ch "pmark" int (i :> int)
  | Erase (b, e) ->
    sexp ch "erase" (pair int int) (b, e)
  | Before c ->
    sexp ch "before" Category.pp c
  | After c ->
    sexp ch "after" Category.pp c

 */
  /* ** */

  let rec first = (f, x) =>
    switch x {
    | list{} => None
    | list{x, ...r} =>
      switch f(x) {
      | None => first(f, r)
      | Some(_) as res => res
      }
    }

  /* ** */

  type ids = ref<int>
  let create_ids = () => ref(0)

  let eps_expr = {id: 0, def: Eps}

  let mk_expr = (ids, def) => {
    incr(ids)
    {id: ids.contents, def}
  }

  let empty = ids => mk_expr(ids, Alt(list{}))

  let cst = (ids, s) =>
    if Re_cset.is_empty(s) {
      empty(ids)
    } else {
      mk_expr(ids, Cst(s))
    }

  let alt = (ids, x) =>
    switch x {
    | list{} => empty(ids)
    | list{c} => c
    | l => mk_expr(ids, Alt(l))
    }

  let seq = (ids, kind, x, y) =>
    switch (x.def, y.def) {
    | (Alt(list{}), _) => x
    | (_, Alt(list{})) => y
    | (Eps, _) => y
    | (_, Eps) if kind == #First => x
    | _ => mk_expr(ids, Seq(kind, x, y))
    }

  let is_eps = expr =>
    switch expr.def {
    | Eps => true
    | _ => false
    }

  let eps = ids => mk_expr(ids, Eps)

  let rep = (ids, kind, sem, x) => mk_expr(ids, Rep(kind, sem, x))

  let mark = (ids, m) => mk_expr(ids, Mark(m))

  let pmark = (ids, i) => mk_expr(ids, Pmark(i))

  let erase = (ids, m, m') => mk_expr(ids, Erase(m, m'))

  let before = (ids, c) => mk_expr(ids, Before(c))

  let after = (ids, c) => mk_expr(ids, After(c))

  /* ** */

  let rec rename = (ids, x) =>
    switch x.def {
    | Cst(_) | Eps | Mark(_) | Pmark(_) | Erase(_) | Before(_) | After(_) => mk_expr(ids, x.def)
    | Alt(l) => mk_expr(ids, Alt(List.map(rename(ids), l)))
    | Seq(k, y, z) => mk_expr(ids, Seq(k, rename(ids, y), rename(ids, z)))
    | Rep(g, k, y) => mk_expr(ids, Rep(g, k, rename(ids, y)))
    }

  /* ** */

  type hash = int
  type mark_infos = array<int>
  type status = Failed | Match(mark_infos, PmarkSet.t) | Running

  module E = {
    type rec t =
      | TSeq(list<t>, expr, sem)
      | TExp(Marks.t, expr)
      | TMatch(Marks.t)

    let rec equal = (l1, l2) =>
      switch (l1, l2) {
      | (list{}, list{}) => true
      | (list{TSeq(l1', e1, _), ...r1}, list{TSeq(l2', e2, _), ...r2}) =>
        e1.id == e2.id && (equal(l1', l2') && equal(r1, r2))
      | (list{TExp(marks1, e1), ...r1}, list{TExp(marks2, e2), ...r2}) =>
        e1.id == e2.id && (marks1 == marks2 && equal(r1, r2))
      | (list{TMatch(marks1), ...r1}, list{TMatch(marks2), ...r2}) =>
        marks1 == marks2 && equal(r1, r2)
      | _ => false
      }

    let rec hash = (l, accu) =>
      switch l {
      | list{} => accu
      | list{TSeq(l', e, _), ...r} =>
        hash(r, hash_combine(0x172a1bce, hash_combine(e.id, hash(l', accu))))
      | list{TExp(marks, e), ...r} =>
        hash(r, hash_combine(0x2b4c0d77, hash_combine(e.id, Marks.hash(marks, accu))))
      | list{TMatch(marks), ...r} => hash(r, hash_combine(0x1c205ad5, Marks.hash(marks, accu)))
      }

    let texp = (marks, x) => TExp(marks, x)

    let tseq = (kind, x, y, rem) =>
      switch x {
      | list{} => rem
      | list{TExp(marks, {def: Eps, _})} => list{TExp(marks, y), ...rem}
      | _ => list{TSeq(x, y, kind), ...rem}
      }
  }

  module State = {
    type t = {
      idx: idx,
      category: Category.t,
      desc: list<E.t>,
      mutable status: option<status>,
      hash: hash,
    }

    let dummy = {
      idx: -1,
      category: Category.dummy,
      desc: list{},
      status: None,
      hash: -1,
    }

    let hash = (idx, cat, desc) =>
      land(E.hash(desc, hash_combine(idx, hash_combine(Category.to_int(cat), 0))), 0x3FFFFFFF)

    let mk = (idx, cat, desc) => {
      idx,
      category: cat,
      desc,
      status: None,
      hash: hash(idx, cat, desc),
    }

    let create = (cat, e) => mk(0, cat, list{E.TExp(Marks.empty, e)})

    let equal = (x, y) =>
      (x.hash: int) == y.hash &&
        ((x.idx: int) == y.idx &&
        (Category.equal(x.category, y.category) && E.equal(x.desc, y.desc)))

    let compare = (x, y) => {
      let c = compare((x.hash: int), y.hash)
      if c != 0 {
        c
      } else {
        let c = Category.compare(x.category, y.category)
        if c != 0 {
          c
        } else {
          compare(x.desc, y.desc)
        }
      }
    }

    type t' = t
    module Table = Hashtbl.Make({
      type t = t'
      let equal = equal
      let hash = t => t.hash
    })
  }

  /* *** Find a free index *** */

  type working_area = ref<array<bool>>

  let create_working_area = () => ref([false])

  let index_count = w => Array.length(w.contents)

  let reset_table = a => Array.fill(a, 0, Array.length(a), false)

  let rec mark_used_indices = tbl =>
    List.iter(x =>
      switch x {
      | E.TSeq(l, _, _) => mark_used_indices(tbl, l)
      | E.TExp(marks, _)
      | E.TMatch(marks) =>
        List.iter(((_, i)) =>
          if i >= 0 {
            tbl[i] = true
          }
        , marks.Marks.marks)
      }
    )

  let rec find_free = (tbl, idx, len) =>
    if idx == len || !tbl[idx] {
      idx
    } else {
      find_free(tbl, idx + 1, len)
    }

  let free_index = (tbl_ref, l) => {
    let tbl = tbl_ref.contents
    reset_table(tbl)
    mark_used_indices(tbl, l)
    let len = Array.length(tbl)
    let idx = find_free(tbl, 0, len)
    if idx == len {
      tbl_ref := Array.make(2 * len, false)
    }
    idx
  }

  /* *** Computation of the next state *** */

  let remove_matches = List.filter(x =>
    switch x {
    | E.TMatch(_) => false
    | _ => true
    }
  )

  let rec split_at_match_rec = (l', x) =>
    switch x {
    | list{} => assert(false)
    | list{E.TMatch(_), ...r} => (List.rev(l'), remove_matches(r))
    | list{x, ...r} => split_at_match_rec(list{x, ...l'}, r)
    }

  let split_at_match = l => split_at_match_rec(list{}, l)

  let rec remove_duplicates = (prev, l, y) =>
    switch l {
    | list{} => (list{}, prev)
    | list{E.TMatch(_) as x, ..._} => /* Truncate after first match */
      (list{x}, prev)
    | list{E.TSeq(l', x, kind), ...r} =>
      let (l'', prev') = remove_duplicates(prev, l', x)
      let (r', prev'') = remove_duplicates(prev', r, y)
      (E.tseq(kind, l'', x, r'), prev'')
    | list{E.TExp(_marks, {def: Eps, _}) as e, ...r} =>
      if List.memq(y.id, prev) {
        remove_duplicates(prev, r, y)
      } else {
        let (r', prev') = remove_duplicates(list{y.id, ...prev}, r, y)
        (list{e, ...r'}, prev')
      }
    | list{E.TExp(_marks, x) as e, ...r} =>
      if List.memq(x.id, prev) {
        remove_duplicates(prev, r, y)
      } else {
        let (r', prev') = remove_duplicates(list{x.id, ...prev}, r, y)
        (list{e, ...r'}, prev')
      }
    }

  let rec set_idx = (idx, x) =>
    switch x {
    | list{} => list{}
    | list{E.TMatch(marks), ...r} =>
      list{E.TMatch(Marks.marks_set_idx(marks, idx)), ...set_idx(idx, r)}
    | list{E.TSeq(l', x, kind), ...r} => list{E.TSeq(set_idx(idx, l'), x, kind), ...set_idx(idx, r)}
    | list{E.TExp(marks, x), ...r} =>
      list{E.TExp(Marks.marks_set_idx(marks, idx), x), ...set_idx(idx, r)}
    }

  let filter_marks = (b, e, marks) => {
    ...marks,
    Marks.marks: List.filter(((i, _)) => i < b || i > e, marks.Marks.marks),
  }

  let rec delta_1 = (marks, c, ~next_cat, ~prev_cat, x, rem) =>
    switch x.def {
    | Cst(s) =>
      if Cset.mem(c, s) {
        list{E.texp(marks, eps_expr), ...rem}
      } else {
        rem
      }
    | Alt(l) => delta_2(marks, c, ~next_cat, ~prev_cat, l, rem)
    | Seq(kind, y, z) =>
      let y' = delta_1(marks, c, ~next_cat, ~prev_cat, y, list{})
      delta_seq(c, ~next_cat, ~prev_cat, kind, y', z, rem)
    | Rep(rep_kind, kind, y) =>
      let y' = delta_1(marks, c, ~next_cat, ~prev_cat, y, list{})
      let (y'', marks') = switch first(x =>
        switch x {
        | E.TMatch(marks) => Some(marks)
        | _ => None
        }
      , y') {
      | None => (y', marks)
      | Some(marks') => (remove_matches(y'), marks')
      }

      switch rep_kind {
      | #Greedy => E.tseq(kind, y'', x, list{E.TMatch(marks'), ...rem})
      | #Non_greedy => list{E.TMatch(marks), ...E.tseq(kind, y'', x, rem)}
      }
    | Eps => list{E.TMatch(marks), ...rem}
    | Mark(i) =>
      let marks = {...marks, Marks.marks: list{(i, -1), ...List.remove_assq(i, marks.Marks.marks)}}
      list{E.TMatch(marks), ...rem}
    | Pmark(i) =>
      let marks = {...marks, Marks.pmarks: PmarkSet.add(i, marks.Marks.pmarks)}
      list{E.TMatch(marks), ...rem}
    | Erase(b, e) => list{E.TMatch(filter_marks(b, e, marks)), ...rem}
    | Before(cat'') =>
      if Category.intersect(next_cat, cat'') {
        list{E.TMatch(marks), ...rem}
      } else {
        rem
      }
    | After(cat'') =>
      if Category.intersect(prev_cat, cat'') {
        list{E.TMatch(marks), ...rem}
      } else {
        rem
      }
    }

  and delta_2 = (marks, c, ~next_cat, ~prev_cat, l, rem) =>
    switch l {
    | list{} => rem
    | list{y, ...r} =>
      delta_1(marks, c, ~next_cat, ~prev_cat, y, delta_2(marks, c, ~next_cat, ~prev_cat, r, rem))
    }

  and delta_seq = (c, ~next_cat, ~prev_cat, kind, y, z, rem) =>
    switch first(x =>
      switch x {
      | E.TMatch(marks) => Some(marks)
      | _ => None
      }
    , y) {
    | None => E.tseq(kind, y, z, rem)
    | Some(marks) =>
      switch kind {
      | #Longest =>
        E.tseq(kind, remove_matches(y), z, delta_1(marks, c, ~next_cat, ~prev_cat, z, rem))
      | #Shortest =>
        delta_1(marks, c, ~next_cat, ~prev_cat, z, E.tseq(kind, remove_matches(y), z, rem))
      | #First =>
        let (y', y'') = split_at_match(y)
        E.tseq(kind, y', z, delta_1(marks, c, ~next_cat, ~prev_cat, z, E.tseq(kind, y'', z, rem)))
      }
    }

  let rec delta_3 = (c, ~next_cat, ~prev_cat, x, rem) =>
    switch x {
    | E.TSeq(y, z, kind) =>
      let y' = delta_4(c, ~next_cat, ~prev_cat, y, list{})
      delta_seq(c, ~next_cat, ~prev_cat, kind, y', z, rem)
    | E.TExp(marks, e) => delta_1(marks, c, ~next_cat, ~prev_cat, e, rem)
    | E.TMatch(_) => list{x, ...rem}
    }

  and delta_4 = (c, ~next_cat, ~prev_cat, l, rem) =>
    switch l {
    | list{} => rem
    | list{y, ...r} => delta_3(c, ~next_cat, ~prev_cat, y, delta_4(c, ~next_cat, ~prev_cat, r, rem))
    }

  let delta = (tbl_ref, next_cat, char, st) => {
    let prev_cat = st.State.category
    let (expr', _) = remove_duplicates(
      list{},
      delta_4(char, ~next_cat, ~prev_cat, st.State.desc, list{}),
      eps_expr,
    )
    let idx = free_index(tbl_ref, expr')
    let expr'' = set_idx(idx, expr')
    State.mk(idx, next_cat, expr'')
  }

  /* ** */

  let rec red_tr = x =>
    switch x {
    | (list{} | list{_}) as l => l
    | list{(s1, st1) as tr1, (s2, st2) as tr2, ...rem} =>
      if State.equal(st1, st2) {
        red_tr(list{(Cset.union(s1, s2), st1), ...rem})
      } else {
        list{tr1, ...red_tr(list{tr2, ...rem})}
      }
    }

  let simpl_tr = l =>
    List.sort(
      ((s1, _), (s2, _)) => compare(s1, s2),
      red_tr(List.sort(((_, st1), (_, st2)) => State.compare(st1, st2), l)),
    )

  /* ** */

  let prepend_deriv = List.fold_right(((s, x), l) => Cset.prepend(s, x, l))

  let rec restrict = (s, x) =>
    switch x {
    | list{} => list{}
    | list{(s', x'), ...rem} =>
      let s'' = Cset.inter(s, s')
      if Cset.is_empty(s'') {
        restrict(s, rem)
      } else {
        list{(s'', x'), ...restrict(s, rem)}
      }
    }

  let rec remove_marks = (b, e, rem) =>
    if b > e {
      rem
    } else {
      remove_marks(b, e - 1, list{(e, -2), ...rem})
    }

  let rec prepend_marks_expr = (m, x) =>
    switch x {
    | E.TSeq(l, e', s) => E.TSeq(prepend_marks_expr_lst(m, l), e', s)
    | E.TExp(m', e') => E.TExp(Marks.merge(m, m'), e')
    | E.TMatch(m') => E.TMatch(Marks.merge(m, m'))
    }

  and prepend_marks_expr_lst = (m, l) => List.map(prepend_marks_expr(m), l)

  let prepend_marks = m => List.map(((s, x)) => (s, prepend_marks_expr_lst(m, x)))

  let rec deriv_1 = (all_chars, categories, marks, cat, x, rem) =>
    switch x.def {
    | Cst(s) => Cset.prepend(s, list{E.texp(marks, eps_expr)}, rem)
    | Alt(l) => deriv_2(all_chars, categories, marks, cat, l, rem)
    | Seq(kind, y, z) =>
      let y' = deriv_1(all_chars, categories, marks, cat, y, list{(all_chars, list{})})
      deriv_seq(all_chars, categories, cat, kind, y', z, rem)
    | Rep(rep_kind, kind, y) =>
      let y' = deriv_1(all_chars, categories, marks, cat, y, list{(all_chars, list{})})
      List.fold_right(((s, z), rem) => {
        let (z', marks') = switch first(x =>
          switch x {
          | E.TMatch(marks) => Some(marks)
          | _ => None
          }
        , z) {
        | None => (z, marks)
        | Some(marks') => (remove_matches(z), marks')
        }

        Cset.prepend(
          s,
          switch rep_kind {
          | #Greedy => E.tseq(kind, z', x, list{E.TMatch(marks')})
          | #Non_greedy => list{E.TMatch(marks), ...E.tseq(kind, z', x, list{})}
          },
          rem,
        )
      }, y', rem)
    | Eps => Cset.prepend(all_chars, list{E.TMatch(marks)}, rem)
    | Mark(i) =>
      Cset.prepend(
        all_chars,
        list{
          E.TMatch({
            ...marks,
            Marks.marks: list{(i, -1), ...List.remove_assq(i, marks.Marks.marks)},
          }),
        },
        rem,
      )
    | Pmark(_) => Cset.prepend(all_chars, list{E.TMatch(marks)}, rem)
    | Erase(b, e) =>
      Cset.prepend(
        all_chars,
        list{
          E.TMatch({
            ...marks,
            Marks.marks: remove_marks(b, e, filter_marks(b, e, marks).Marks.marks),
          }),
        },
        rem,
      )
    | Before(cat') => Cset.prepend(List.assq(cat', categories), list{E.TMatch(marks)}, rem)
    | After(cat') =>
      if Category.intersect(cat, cat') {
        Cset.prepend(all_chars, list{E.TMatch(marks)}, rem)
      } else {
        rem
      }
    }

  and deriv_2 = (all_chars, categories, marks, cat, l, rem) =>
    switch l {
    | list{} => rem
    | list{y, ...r} =>
      deriv_1(
        all_chars,
        categories,
        marks,
        cat,
        y,
        deriv_2(all_chars, categories, marks, cat, r, rem),
      )
    }

  and deriv_seq = (all_chars, categories, cat, kind, y, z, rem) =>
    if List.exists(((_s, xl)) => List.exists(x =>
        switch x {
        | E.TMatch(_) => true
        | _ => false
        }
      , xl), y) {
      let z' = deriv_1(all_chars, categories, Marks.empty, cat, z, list{(all_chars, list{})})
      List.fold_right(((s, y), rem) =>
        switch first(x =>
          switch x {
          | E.TMatch(marks) => Some(marks)
          | _ => None
          }
        , y) {
        | None => Cset.prepend(s, E.tseq(kind, y, z, list{}), rem)
        | Some(marks) =>
          let z'' = prepend_marks(marks, z')
          switch kind {
          | #Longest =>
            Cset.prepend(
              s,
              E.tseq(kind, remove_matches(y), z, list{}),
              prepend_deriv(restrict(s, z''), rem),
            )
          | #Shortest =>
            prepend_deriv(
              restrict(s, z''),
              Cset.prepend(s, E.tseq(kind, remove_matches(y), z, list{}), rem),
            )
          | #First =>
            let (y', y'') = split_at_match(y)
            Cset.prepend(
              s,
              E.tseq(kind, y', z, list{}),
              prepend_deriv(restrict(s, z''), Cset.prepend(s, E.tseq(kind, y'', z, list{}), rem)),
            )
          }
        }
      , y, rem)
    } else {
      List.fold_right(((s, xl), rem) => Cset.prepend(s, E.tseq(kind, xl, z, list{}), rem), y, rem)
    }

  let rec deriv_3 = (all_chars, categories, cat, x, rem) =>
    switch x {
    | E.TSeq(y, z, kind) =>
      let y' = deriv_4(all_chars, categories, cat, y, list{(all_chars, list{})})
      deriv_seq(all_chars, categories, cat, kind, y', z, rem)
    | E.TExp(marks, e) => deriv_1(all_chars, categories, marks, cat, e, rem)
    | E.TMatch(_) => Cset.prepend(all_chars, list{x}, rem)
    }

  and deriv_4 = (all_chars, categories, cat, l, rem) =>
    switch l {
    | list{} => rem
    | list{y, ...r} =>
      deriv_3(all_chars, categories, cat, y, deriv_4(all_chars, categories, cat, r, rem))
    }

  let deriv = (tbl_ref, all_chars, categories, st) => {
    let der = deriv_4(
      all_chars,
      categories,
      st.State.category,
      st.State.desc,
      list{(all_chars, list{})},
    )
    simpl_tr(List.fold_right(((s, expr), rem) => {
        let (expr', _) = remove_duplicates(list{}, expr, eps_expr)
        let idx = free_index(tbl_ref, expr')
        let expr'' = set_idx(idx, expr')
        List.fold_right(((cat', s'), rem) => {
          let s'' = Cset.inter(s, s')
          if Cset.is_empty(s'') {
            rem
          } else {
            list{(s'', State.mk(idx, cat', expr'')), ...rem}
          }
        }, categories, rem)
      }, der, list{}))
  }

  /* ** */

  let flatten_match = m => {
    let ma = List.fold_left((ma, (i, _)) => max(ma, i), -1, m)
    let res = Array.make(ma + 1, -1)
    List.iter(((i, v)) => res[i] = v, m)
    res
  }

  let status = s =>
    switch s.State.status {
    | Some(st) => st
    | None =>
      let st = switch s.State.desc {
      | list{} => Failed
      | list{E.TMatch(m), ..._} => Match(flatten_match(m.Marks.marks), m.Marks.pmarks)
      | _ => Running
      }

      s.State.status = Some(st)
      st
    }
}
module Re: {
  @@ocaml.text(
    /*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*/

    " Module [Re]: regular expressions commons "
  )

  @ocaml.doc(" Regular expression ")
  type t

  @ocaml.doc(" Compiled regular expression ")
  type re

  @ocaml.doc(" Information about groups in a match. ")
  type groups

  @@ocaml.text(" {2 Compilation and execution of a regular expression} ")

  @ocaml.doc(" Compile a regular expression into an executable version that can be
    used to match strings, e.g. with {!exec}. ")
  let compile: t => re

  @ocaml.doc(" [exec re str] matches [str] against the compiled expression [re],
    and returns the matched groups if any.
    @param pos optional beginning of the string (default 0)
    @param len length of the substring of [str] that can be matched (default [-1],
      meaning to the end of the string
    @raise Not_found if the regular expression can't be found in [str]
")
  let exec: (
    ~pos: int /* Default: 0 */=?,
    ~len: int /* Default: -1 (until end of string) */=?,
    re,
    string,
  ) => groups

  @ocaml.doc(" Similar to {!exec}, but returns an option instead of using an exception. ")
  let exec_opt: (
    ~pos: int /* Default: 0 */=?,
    ~len: int /* Default: -1 (until end of string) */=?,
    re,
    string,
  ) => option<groups>

  @ocaml.doc(" Similar to {!exec}, but returns [true] if the expression matches,
    and [false] if it doesn't ")
  let execp: (
    ~pos: int /* Default: 0 */=?,
    ~len: int /* Default: -1 (until end of string) */=?,
    re,
    string,
  ) => bool

  @ocaml.doc(" More detailed version of {!exec_p} ")
  let exec_partial: (
    ~pos: int /* Default: 0 */=?,
    ~len: int /* Default: -1 (until end of string) */=?,
    re,
    string,
  ) => [#Full | #Partial | #Mismatch]

  @ocaml.doc(" Manipulate matching groups. ")
  module Group: {
    @ocaml.doc(" Information about groups in a match. ")
    type t = groups

    @ocaml.doc(" Raise [Not_found] if the group did not match ")
    let get: (t, int) => string

    @ocaml.doc(" Raise [Not_found] if the group did not match ")
    let offset: (t, int) => (int, int)

    @ocaml.doc(" Return the start of the match. Raise [Not_found] if the group did not match. ")
    let start: (t, int) => int

    @ocaml.doc(" Return the end of the match. Raise [Not_found] if the group did not match. ")
    let stop: (t, int) => int

    @ocaml.doc(" Return the empty string for each group which did not match ")
    let all: t => array<string>

    @ocaml.doc(" Return [(-1,-1)] for each group which did not match ")
    let all_offset: t => array<(int, int)>

    @ocaml.doc(" Test whether a group matched ")
    let test: (t, int) => bool

    @ocaml.doc(" Returns the total number of groups defined - matched or not.
      This function is experimental. ")
    let nb_groups: t => int

    /* val pp : Format.formatter -> t -> unit */
  }

  @ocaml.doc(" Marks ")
  module Mark: {
    @ocaml.doc(" Mark id ")
    type t

    @ocaml.doc(" Tell if a mark was matched. ")
    let test: (Group.t, t) => bool

    module Set: Set.S with type elt = t

    @ocaml.doc(" Return all the mark matched. ")
    let all: Group.t => Set.t

    let equal: (t, t) => bool
    let compare: (t, t) => int
  }

  @@ocaml.text(" {2 High Level Operations} ")

  type gen<'a> = unit => option<'a>

  @ocaml.doc(" Repeatedly calls {!exec} on the given string, starting at given
    position and length.")
  let all: (~pos: int=?, ~len: int=?, re, string) => list<Group.t>

  @ocaml.doc(" Same as {!all} but returns a generator ")
  let all_gen: (~pos: int=?, ~len: int=?, re, string) => gen<Group.t>

  @ocaml.doc(" Same as {!all}, but extracts the matched substring rather than
    returning the whole group. This basically iterates over matched
    strings ")
  let matches: (~pos: int=?, ~len: int=?, re, string) => list<string>

  @ocaml.doc(" Same as {!matches}, but returns a generator. ")
  let matches_gen: (~pos: int=?, ~len: int=?, re, string) => gen<string>

  @ocaml.doc(" [split re s] splits [s] into chunks separated by [re]. It yields
    the chunks themselves, not the separator. For instance
    this can be used with a whitespace-matching re such as [\"[\t ]+\"]. ")
  let split: (~pos: int=?, ~len: int=?, re, string) => list<string>

  let split_gen: (~pos: int=?, ~len: int=?, re, string) => gen<string>

  type split_token = [
    | @ocaml.doc(" Text between delimiters ") #Text(string)
    | @ocaml.doc(" Delimiter ") #Delim(Group.t)
  ]

  let split_full: (~pos: int=?, ~len: int=?, re, string) => list<split_token>

  let split_full_gen: (~pos: int=?, ~len: int=?, re, string) => gen<split_token>

  @ocaml.doc(" [replace ~all re ~f s] iterates on [s], and replaces every occurrence
    of [re] with [f substring] where [substring] is the current match.
    If [all = false], then only the first occurrence of [re] is replaced. ")
  let replace: (
    ~pos: int=?,
    ~len: int=?,
    ~all: bool=?,
    re,
    ~f: Group.t => string /* how to replace */,
    string,
  ) => string

  @ocaml.doc(" [replace_string ~all re ~by s] iterates on [s], and replaces every
    occurrence of [re] with [by]. If [all = false], then only the first
    occurrence of [re] is replaced. ")
  let replace_string: (~pos: int=?, ~len: int=?, ~all: bool=?, re, ~by: string, string) => string

  @@ocaml.text(" {2 String expressions (literal match)} ")

  let str: string => t
  let char: char => t

  @@ocaml.text(" {2 Basic operations on regular expressions} ")

  @ocaml.doc(" Alternative ")
  let alt: list<t> => t

  @ocaml.doc(" Sequence ")
  let seq: list<t> => t

  @ocaml.doc(" Match nothing ")
  let empty: t

  @ocaml.doc(" Empty word ")
  let epsilon: t

  @ocaml.doc(" 0 or more matches ")
  let rep: t => t

  @ocaml.doc(" 1 or more matches ")
  let rep1: t => t

  @ocaml.doc(" [repn re i j] matches [re] at least [i] times
    and at most [j] times, bounds included.
    [j = None] means no upper bound.
")
  let repn: (t, int, option<int>) => t

  @ocaml.doc(" 0 or 1 matches ")
  let opt: t => t

  @@ocaml.text(" {2 String, line, word} ")

  @ocaml.doc(" Beginning of line ")
  let bol: t

  @ocaml.doc(" End of line ")
  let eol: t

  @ocaml.doc(" Beginning of word ")
  let bow: t

  @ocaml.doc(" End of word ")
  let eow: t

  @ocaml.doc(" Beginning of string ")
  let bos: t

  @ocaml.doc(" End of string ")
  let eos: t

  @ocaml.doc(" Last end of line or end of string ")
  let leol: t

  @ocaml.doc(" Initial position ")
  let start: t

  @ocaml.doc(" Final position ")
  let stop: t

  @ocaml.doc(" Word ")
  let word: t => t

  @ocaml.doc(" Not at a word boundary ")
  let not_boundary: t

  @ocaml.doc(" Only matches the whole string ")
  let whole_string: t => t

  @@ocaml.text(" {2 Match semantics} ")

  @ocaml.doc(" Longest match ")
  let longest: t => t

  @ocaml.doc(" Shortest match ")
  let shortest: t => t

  @ocaml.doc(" First match ")
  let first: t => t

  @@ocaml.text(" {2 Repeated match modifiers} ")

  @ocaml.doc(" Greedy ")
  let greedy: t => t

  @ocaml.doc(" Non-greedy ")
  let non_greedy: t => t

  @@ocaml.text(" {2 Groups (or submatches)} ")

  @ocaml.doc(" Delimit a group ")
  let group: t => t

  @ocaml.doc(" Remove all groups ")
  let no_group: t => t

  @ocaml.doc(" when matching against [nest e], only the group matching in the
       last match of e will be considered as matching ")
  let nest: t => t

  @ocaml.doc(" Mark a regexp. the markid can then be used to know if this regexp was used. ")
  let mark: t => (Mark.t, t)

  @@ocaml.text(" {2 Character sets} ")

  @ocaml.doc(" Any character of the string ")
  let set: string => t

  @ocaml.doc(" Character ranges ")
  let rg: (char, char) => t

  @ocaml.doc(" Intersection of character sets ")
  let inter: list<t> => t

  @ocaml.doc(" Difference of character sets ")
  let diff: (t, t) => t

  @ocaml.doc(" Complement of union ")
  let compl: list<t> => t

  @@ocaml.text(" {2 Predefined character sets} ")

  @ocaml.doc(" Any character ")
  let any: t

  @ocaml.doc(" Any character but a newline ")
  let notnl: t

  let alnum: t
  let wordc: t
  let alpha: t
  let ascii: t
  let blank: t
  let cntrl: t
  let digit: t
  let graph: t
  let lower: t
  let print: t
  let punct: t
  let space: t
  let upper: t
  let xdigit: t

  @@ocaml.text(" {2 Case modifiers} ")

  @ocaml.doc(" Case sensitive matching ")
  let case: t => t

  @ocaml.doc(" Case insensitive matching ")
  let no_case: t => t

  @@ocaml.text(
    /* ** */

    " {2 Internal debugging}  "
  )

  @@ocaml.text(
    /* val pp : Format.formatter -> t -> unit */

    /* val pp_re : Format.formatter -> re -> unit */

    " Alias for {!pp_re}. Deprecated "
  )
  @@ocaml.text(
    /* val print_re : Format.formatter -> re -> unit */

    " {2 Experimental functions}. "
  )

  @ocaml.doc(" [witness r] generates a string [s] such that [execp (compile r) s] is
    true ")
  let witness: t => string

  @@ocaml.text(" {2 Deprecated functions} ")

  @ocaml.doc(" Alias for {!Group.t}. Deprecated ")
  type substrings = Group.t

  @ocaml.doc(" Same as {!Group.get}. Deprecated ")
  let get: (Group.t, int) => string

  @ocaml.doc(" Same as {!Group.offset}. Deprecated ")
  let get_ofs: (Group.t, int) => (int, int)

  @ocaml.doc(" Same as {!Group.all}. Deprecated ")
  let get_all: Group.t => array<string>

  @ocaml.doc(" Same as {!Group.all_offset}. Deprecated ")
  let get_all_ofs: Group.t => array<(int, int)>

  @ocaml.doc(" Same as {!Group.test}. Deprecated ")
  let test: (Group.t, int) => bool

  @ocaml.doc(" Alias for {!Mark.t}. Deprecated ")
  type markid = Mark.t

  @ocaml.doc(" Same as {!Mark.test}. Deprecated ")
  let marked: (Group.t, Mark.t) => bool

  @ocaml.doc(" Same as {!Mark.all}. Deprecated ")
  let mark_set: Group.t => Mark.Set.t
} = {
  /*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*/

  module Cset = Re_cset
  module Automata = Re_automata
  module MarkSet = Automata.PmarkSet
  module Category = Automata.Category

  let rec iter = (n, f, v) =>
    if n == 0 {
      v
    } else {
      iter(n - 1, f, f(v))
    }

  /* ** */

  let unknown = -2
  let break = -3

  /* Result of a successful match. */
  type groups = {
    s: string,
    /* Input string. Matched strings are substrings of s */
    marks: Automata.mark_infos,
    /* Mapping from group indices to positions in gpos. group i has positions 2*i
     - 1, 2*i + 1 in gpos. If the group wasn't matched, then its corresponding
     values in marks will be -1,-1 */
    pmarks: MarkSet.t,
    /* Marks positions. i.e. those marks created with Re.marks */
    gpos: array<int>,
    /* Group positions. Adjacent elements are (start, stop) of group match.
     indexed by the values in marks. So group i in an re would be the substring:

     start = t.gpos.(marks.(2*i)) - 1
     stop = t.gpos.(marks.(2*i + 1)) - 1 */
    gcount: int,
    /* Number of groups the regular expression contains. Matched or not */
  }

  type match_info =
    | Match(groups)
    | Failed
    | Running

  type rec state = {
    idx: int,
    /* Index of the current position in the position table.
       Not yet computed transitions point to a dummy state where
       [idx] is set to [unknown];
       If [idx] is set to [break] for states that either always
       succeed or always fail. */
    real_idx: int,
    /* The real index, in case [idx] is set to [break] */
    next: array<state>,
    /* Transition table, indexed by color */
    mutable final: list<(Category.t, (Automata.idx, Automata.status))>,
    /* Mapping from the category of the next character to
       - the index where the next position should be saved
       - possibly, the list of marks (and the corresponding indices)
         corresponding to the best match */
    desc: Automata.State.t,
    /* Description of this state of the automata */
  }

  /* Automata (compiled regular expression) */
  type re = {
    initial: Automata.expr,
    /* The whole regular expression */
    mutable initial_states: list<(Category.t, state)>,
    /* Initial states, indexed by initial category */
    cols: Bytes.t,
    /* Color table */
    col_repr: Bytes.t,
    /* Table from colors to one character of this color */
    ncol: int,
    /* Number of colors. */
    lnl: int,
    /* Color of the last newline */
    tbl: Automata.working_area,
    /* Temporary table used to compute the first available index
     when computing a new state */
    states: Automata.State.Table.t<state>,
    /* States of the deterministic automata */
    group_count: int,
    /* Number of groups in the regular expression */
  }

  /* let print_re = pp_re */

  /* Information used during matching */
  type info = {
    re: re,
    /* The automata */
    i_cols: Bytes.t,
    /* Color table ([x.i_cols = x.re.cols])
     Shortcut used for performance reasons */
    mutable positions: array<int>,
    /* Array of mark positions
     The mark are off by one for performance reasons */
    pos: int,
    /* Position where the match is started */
    last: int,
    /* Position where the match should stop */
  }

  /* ** */

  let category = (re, c) =>
    if c == -1 {
      Category.inexistant
    } /* Special category for the last newline */
    else if c == re.lnl {
      open Category
      \"++"(\"++"(lastnewline, newline), not_letter)
    } else {
      Category.from_char(Bytes.get(re.col_repr, c))
    }

  /* ** */

  let dummy_next = []

  let unknown_state = {
    idx: unknown,
    real_idx: 0,
    next: dummy_next,
    final: list{},
    desc: Automata.State.dummy,
  }

  let mk_state = (ncol, desc) => {
    let break_state = switch Automata.status(desc) {
    | Automata.Running => false
    | Automata.Failed
    | Automata.Match(_) => true
    }

    {
      idx: if break_state {
        break
      } else {
        desc.Automata.State.idx
      },
      real_idx: desc.Automata.State.idx,
      next: if break_state {
        dummy_next
      } else {
        Array.make(ncol, unknown_state)
      },
      final: list{},
      desc,
    }
  }

  let find_state = (re, desc) =>
    try Automata.State.Table.find(re.states, desc) catch {
    | Not_found =>
      let st = mk_state(re.ncol, desc)
      Automata.State.Table.add(re.states, desc, st)
      st
    }

  /* *** Match with marks *** */

  let delta = (info, cat, c, st) => {
    let desc = Automata.delta(info.re.tbl, cat, c, st.desc)
    let len = Array.length(info.positions)
    if desc.Automata.State.idx == len && len > 0 {
      let pos = info.positions
      info.positions = Array.make(2 * len, 0)
      Array.blit(pos, 0, info.positions, 0, len)
    }
    desc
  }

  let validate = (info, s: string, pos, st) => {
    let c = Char.code(Bytes.get(info.i_cols, Char.code(String.get(s, pos))))
    let cat = category(info.re, c)
    let desc' = delta(info, cat, c, st)
    let st' = find_state(info.re, desc')
    st.next[c] = st'
  }

  /*
let rec loop info s pos st =
  if pos < info.last then
    let st' = st.next.(Char.code info.i_cols.[Char.code s.[pos]]) in
    let idx = st'.idx in
    if idx >= 0 then begin
      info.positions.(idx) <- pos;
      loop info s (pos + 1) st'
    end else if idx = break then begin
      info.positions.(st'.real_idx) <- pos;
      st'
    end else begin (* Unknown *)
      validate info s pos st;
      loop info s pos st
    end
  else
    st
*/

  let rec loop = (info, s: string, pos, st) =>
    if pos < info.last {
      let st' = st.next[Char.code(Bytes.get(info.i_cols, Char.code(String.get(s, pos))))]
      loop2(info, s, pos, st, st')
    } else {
      st
    }

  and loop2 = (info, s, pos, st, st') =>
    if st'.idx >= 0 {
      let pos = pos + 1
      if pos < info.last {
        /* It is important to place these reads before the write */
        /* But then, we don't have enough registers left to store the
         right position.  So, we store the position plus one. */
        let st'' = st'.next[Char.code(Bytes.get(info.i_cols, Char.code(String.get(s, pos))))]
        info.positions[st'.idx] = pos
        loop2(info, s, pos, st', st'')
      } else {
        info.positions[st'.idx] = pos
        st'
      }
    } else if st'.idx == break {
      info.positions[st'.real_idx] = pos + 1
      st'
    } else {
      /* Unknown */
      validate(info, s, pos, st)
      loop(info, s, pos, st)
    }

  let rec loop_no_mark = (info, s, pos, last, st) =>
    if pos < last {
      let st' = st.next[Char.code(Bytes.get(info.i_cols, Char.code(String.get(s, pos))))]
      if st'.idx >= 0 {
        loop_no_mark(info, s, pos + 1, last, st')
      } else if st'.idx == break {
        st'
      } else {
        /* Unknown */
        validate(info, s, pos, st)
        loop_no_mark(info, s, pos, last, st)
      }
    } else {
      st
    }

  let final = (info, st, cat) =>
    try List.assq(cat, st.final) catch {
    | Not_found =>
      let st' = delta(info, cat, -1, st)
      let res = (st'.Automata.State.idx, Automata.status(st'))
      st.final = list{(cat, res), ...st.final}
      res
    }

  let find_initial_state = (re, cat) =>
    try List.assq(cat, re.initial_states) catch {
    | Not_found =>
      let st = find_state(re, Automata.State.create(cat, re.initial))
      re.initial_states = list{(cat, st), ...re.initial_states}
      st
    }

  let get_color = (re, s: string, pos) =>
    if pos < 0 {
      -1
    } else {
      let slen = String.length(s)
      if pos >= slen {
        -1
      } else if pos == slen - 1 && (re.lnl != -1 && String.get(s, pos) == '\n') {
        /* Special case for the last newline */
        re.lnl
      } else {
        Char.code(Bytes.get(re.cols, Char.code(String.get(s, pos))))
      }
    }

  let rec handle_last_newline = (info, pos, st, groups) => {
    let st' = st.next[info.re.lnl]
    if st'.idx >= 0 {
      if groups {
        info.positions[st'.idx] = pos + 1
      }
      st'
    } else if st'.idx == break {
      if groups {
        info.positions[st'.real_idx] = pos + 1
      }
      st'
    } else {
      /* Unknown */
      let c = info.re.lnl
      let real_c = Char.code(Bytes.get(info.i_cols, Char.code('\n')))
      let cat = category(info.re, c)
      let desc' = delta(info, cat, real_c, st)
      let st' = find_state(info.re, desc')
      st.next[c] = st'
      handle_last_newline(info, pos, st, groups)
    }
  }

  let rec scan_str = (info, s: string, initial_state, groups) => {
    let pos = info.pos
    let last = info.last
    if (
      last == String.length(s) &&
        (info.re.lnl != -1 &&
        (last > pos && String.get(s, last - 1) == '\n'))
    ) {
      let info = {...info, last: last - 1}
      let st = scan_str(info, s, initial_state, groups)
      if st.idx == break {
        st
      } else {
        handle_last_newline(info, last - 1, st, groups)
      }
    } else if groups {
      loop(info, s, pos, initial_state)
    } else {
      loop_no_mark(info, s, pos, last, initial_state)
    }
  }

  let match_str = (~groups, ~partial, re, s, ~pos, ~len) => {
    let slen = String.length(s)
    let last = if len == -1 {
      slen
    } else {
      pos + len
    }
    let info = {
      re,
      i_cols: re.cols,
      pos,
      last,
      positions: if groups {
        let n = Automata.index_count(re.tbl) + 1
        if n <= 10 {
          [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
        } else {
          Array.make(n, 0)
        }
      } else {
        []
      },
    }

    let initial_cat = if pos == 0 {
      open Category
      \"++"(search_boundary, inexistant)
    } else {
      open Category
      \"++"(search_boundary, category(re, get_color(re, s, pos - 1)))
    }
    let initial_state = find_initial_state(re, initial_cat)
    let st = scan_str(info, s, initial_state, groups)
    let res = if st.idx == break || partial {
      Automata.status(st.desc)
    } else {
      let final_cat = if last == slen {
        open Category
        \"++"(search_boundary, inexistant)
      } else {
        open Category
        \"++"(search_boundary, category(re, get_color(re, s, last)))
      }
      let (idx, res) = final(info, st, final_cat)
      if groups {
        info.positions[idx] = last + 1
      }
      res
    }

    switch res {
    | Automata.Match(marks, pmarks) =>
      Match({s, marks, pmarks, gpos: info.positions, gcount: re.group_count})
    | Automata.Failed => Failed
    | Automata.Running => Running
    }
  }

  let mk_re = (init, cols, col_repr, ncol, lnl, group_count) => {
    initial: init,
    initial_states: list{},
    cols,
    col_repr,
    ncol,
    lnl,
    tbl: Automata.create_working_area(),
    states: Automata.State.Table.create(97),
    group_count,
  }

  /* *** Character sets *** */

  let cseq = (c, c') => Cset.seq(Char.code(c), Char.code(c'))
  let cadd = (c, s) => Cset.add(Char.code(c), s)

  let trans_set = (cache, cm, s) =>
    switch Cset.one_char(s) {
    | Some(i) => Cset.csingle(Bytes.get(cm, i))
    | None =>
      let v = (Cset.hash_rec(s), s)
      try Cset.CSetMap.find(v, cache.contents) catch {
      | Not_found =>
        let l = Cset.fold_right(
          s,
          ~f=((i, j), l) => Cset.union(cseq(Bytes.get(cm, i), Bytes.get(cm, j)), l),
          ~init=Cset.empty,
        )

        cache := Cset.CSetMap.add(v, l, cache.contents)
        l
      }
    }

  /* ** */

  type rec regexp =
    | Set(Cset.t)
    | Sequence(list<regexp>)
    | Alternative(list<regexp>)
    | Repeat(regexp, int, option<int>)
    | Beg_of_line
    | End_of_line
    | Beg_of_word
    | End_of_word
    | Not_bound
    | Beg_of_str
    | End_of_str
    | Last_end_of_line
    | Start
    | Stop
    | Sem(Automata.sem, regexp)
    | Sem_greedy(Automata.rep_kind, regexp)
    | Group(regexp)
    | No_group(regexp)
    | Nest(regexp)
    | Case(regexp)
    | No_case(regexp)
    | Intersection(list<regexp>)
    | Complement(list<regexp>)
    | Difference(regexp, regexp)
    | Pmark(Automata.Pmark.t, regexp)

  /* let %ignore rec pp fmt t =
  let open Re_fmt in
  let var s re = sexp fmt s pp re in
  let seq s rel = sexp fmt s (list pp) rel in
  match t with
  | Set s ->  sexp fmt "Set" Cset.pp s
  | Sequence sq -> seq "Sequence" sq
  | Alternative alt -> seq "Alternative" alt
  | Repeat (re, start, stop) ->
    let pp' fmt () = fprintf fmt "%a@ %d%a" pp re   start   optint stop in
    sexp fmt "Repeat" pp' ()
  | Beg_of_line      -> str fmt "Beg_of_line"
  | End_of_line      -> str fmt "End_of_line"
  | Beg_of_word      -> str fmt "Beg_of_word"
  | End_of_word      -> str fmt "End_of_word"
  | Not_bound        -> str fmt "Not_bound"
  | Beg_of_str       -> str fmt "Beg_of_str"
  | End_of_str       -> str fmt "End_of_str"
  | Last_end_of_line -> str fmt "Last_end_of_line"
  | Start            -> str fmt "Start"
  | Stop             -> str fmt "Stop"
  | Sem (sem, re)    ->
    sexp fmt "Sem" (pair Automata.pp_sem pp) (sem, re)
  | Sem_greedy (k, re) ->
    sexp fmt "Sem_greedy" (pair Automata.pp_rep_kind pp) (k, re)
  | Group c        -> var "Group" c
  | No_group c     -> var "No_group" c
  | Nest c         -> var "Nest" c
  | Case c         -> var "Case" c
  | No_case c      -> var "No_case" c
  | Intersection c -> seq "Intersection" c
  | Complement c   -> seq "Complement" c
  | Difference (a, b) -> sexp fmt "Difference" (pair pp pp) (a, b)
  | Pmark (m, r)      -> sexp fmt "Pmark" (pair Automata.Pmark.pp pp) (m, r)
 */
  let rec is_charset = x =>
    switch x {
    | Set(_) => true
    | Alternative(l) | Intersection(l) | Complement(l) => List.for_all(is_charset, l)
    | Difference(r, r') => is_charset(r) && is_charset(r')
    | Sem(_, r)
    | Sem_greedy(_, r)
    | No_group(r)
    | Case(r)
    | No_case(r) =>
      is_charset(r)
    | Sequence(_)
    | Repeat(_)
    | Beg_of_line
    | End_of_line
    | Beg_of_word
    | End_of_word
    | Beg_of_str
    | End_of_str
    | Not_bound
    | Last_end_of_line
    | Start
    | Stop
    | Group(_)
    | Nest(_)
    | Pmark(_, _) => false
    }

  /* *** Colormap *** */

  /* XXX Use a better algorithm allowing non-contiguous regions? */
  let split = (s, cm) =>
    Re_cset.iter(s, ~f=(i, j) => {
      Bytes.set(cm, i, '')
      Bytes.set(cm, j + 1, '')
    })

  let cupper = Cset.union(cseq('A', 'Z'), Cset.union(cseq('À', 'Ö'), cseq('Ø', 'Þ')))
  let clower = Cset.offset(32, cupper)
  let calpha = List.fold_right(cadd, list{'ª', 'µ', 'º', 'ß', 'ÿ'}, Cset.union(clower, cupper))
  let cdigit = cseq('0', '9')
  let calnum = Cset.union(calpha, cdigit)
  let cword = cadd('_', calnum)

  let colorize = (c, regexp) => {
    let lnl = ref(false)
    let rec colorize = regexp =>
      switch regexp {
      | Set(s) => split(s, c)
      | Sequence(l) => List.iter(colorize, l)
      | Alternative(l) => List.iter(colorize, l)
      | Repeat(r, _, _) => colorize(r)
      | Beg_of_line | End_of_line => split(Cset.csingle('\n'), c)
      | Beg_of_word
      | End_of_word
      | Not_bound =>
        split(cword, c)
      | Beg_of_str
      | End_of_str
      | Start
      | Stop => ()
      | Last_end_of_line => lnl := true
      | Sem(_, r)
      | Sem_greedy(_, r)
      | Group(r)
      | No_group(r)
      | Nest(r)
      | Pmark(_, r) =>
        colorize(r)
      | Case(_)
      | No_case(_)
      | Intersection(_)
      | Complement(_)
      | Difference(_) =>
        assert(false)
      }

    colorize(regexp)
    lnl.contents
  }

  let make_cmap = () => Bytes.make(257, ' ')

  let flatten_cmap = cm => {
    let c = Bytes.create(256)
    let col_repr = Bytes.create(256)
    let v = ref(0)
    Bytes.set(c, 0, ' ')
    Bytes.set(col_repr, 0, ' ')
    for i in 1 to 255 {
      if Bytes.get(cm, i) != ' ' {
        incr(v)
      }
      Bytes.set(c, i, Char.chr(v.contents))
      Bytes.set(col_repr, v.contents, Char.chr(i))
    }
    (c, Bytes.sub(col_repr, 0, v.contents + 1), v.contents + 1)
  }

  /* *** Compilation *** */

  let rec equal = (x1, x2) =>
    switch (x1, x2) {
    | (Set(s1), Set(s2)) => s1 == s2
    | (Sequence(l1), Sequence(l2)) => eq_list(l1, l2)
    | (Alternative(l1), Alternative(l2)) => eq_list(l1, l2)
    | (Repeat(x1', i1, j1), Repeat(x2', i2, j2)) => i1 == i2 && (j1 == j2 && equal(x1', x2'))
    | (Beg_of_line, Beg_of_line)
    | (End_of_line, End_of_line)
    | (Beg_of_word, Beg_of_word)
    | (End_of_word, End_of_word)
    | (Not_bound, Not_bound)
    | (Beg_of_str, Beg_of_str)
    | (End_of_str, End_of_str)
    | (Last_end_of_line, Last_end_of_line)
    | (Start, Start)
    | (Stop, Stop) => true
    | (Sem(sem1, x1'), Sem(sem2, x2')) => sem1 == sem2 && equal(x1', x2')
    | (Sem_greedy(k1, x1'), Sem_greedy(k2, x2')) => k1 == k2 && equal(x1', x2')
    | (Group(_), Group(_)) => /* Do not merge groups! */
      false
    | (No_group(x1'), No_group(x2')) => equal(x1', x2')
    | (Nest(x1'), Nest(x2')) => equal(x1', x2')
    | (Case(x1'), Case(x2')) => equal(x1', x2')
    | (No_case(x1'), No_case(x2')) => equal(x1', x2')
    | (Intersection(l1), Intersection(l2)) => eq_list(l1, l2)
    | (Complement(l1), Complement(l2)) => eq_list(l1, l2)
    | (Difference(x1', x1''), Difference(x2', x2'')) => equal(x1', x2') && equal(x1'', x2'')
    | (Pmark(m1, r1), Pmark(m2, r2)) => Automata.Pmark.equal(m1, m2) && equal(r1, r2)
    | _ => false
    }

  and eq_list = (l1, l2) =>
    switch (l1, l2) {
    | (list{}, list{}) => true
    | (list{x1, ...r1}, list{x2, ...r2}) => equal(x1, x2) && eq_list(r1, r2)
    | _ => false
    }

  let sequence = x =>
    switch x {
    | list{x} => x
    | l => Sequence(l)
    }

  let rec merge_sequences = x =>
    switch x {
    | list{} => list{}
    | list{Alternative(l'), ...r} => merge_sequences(\"@"(l', r))
    | list{Sequence(list{x, ...y}), ...r} =>
      switch merge_sequences(r) {
      | list{Sequence(list{x', ...y'}), ...r'} if equal(x, x') =>
        list{Sequence(list{x, Alternative(list{sequence(y), sequence(y')})}), ...r'}
      | r' => list{Sequence(list{x, ...y}), ...r'}
      }
    | list{x, ...r} => list{x, ...merge_sequences(r)}
    }

  module A = Automata

  let enforce_kind = (ids, kind, kind', cr) =>
    switch (kind, kind') {
    | (#First, #First) => cr
    | (#First, k) => A.seq(ids, k, cr, A.eps(ids))
    | _ => cr
    }

  /* XXX should probably compute a category mask */
  let rec translate = (ids, kind, ign_group, ign_case, greedy, pos, cache, c, x) =>
    switch x {
    | Set(s) => (A.cst(ids, trans_set(cache, c, s)), kind)
    | Sequence(l) => (trans_seq(ids, kind, ign_group, ign_case, greedy, pos, cache, c, l), kind)
    | Alternative(l) =>
      switch merge_sequences(l) {
      | list{r'} =>
        let (cr, kind') = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r')
        (enforce_kind(ids, kind, kind', cr), kind)
      | merged_sequences => (A.alt(ids, List.map(r' => {
              let (cr, kind') = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r')
              enforce_kind(ids, kind, kind', cr)
            }, merged_sequences)), kind)
      }
    | Repeat(r', i, j) =>
      let (cr, kind') = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r')
      let rem = switch j {
      | None => A.rep(ids, greedy, kind', cr)
      | Some(j) =>
        let f = switch greedy {
        | #Greedy => rem => A.alt(ids, list{A.seq(ids, kind', A.rename(ids, cr), rem), A.eps(ids)})
        | #Non_greedy =>
          rem => A.alt(ids, list{A.eps(ids), A.seq(ids, kind', A.rename(ids, cr), rem)})
        }

        iter(j - i, f, A.eps(ids))
      }

      (iter(i, rem => A.seq(ids, kind', A.rename(ids, cr), rem), rem), kind)
    | Beg_of_line => (
        A.after(
          ids,
          {
            open Category
            \"++"(inexistant, newline)
          },
        ),
        kind,
      )
    | End_of_line => (
        A.before(
          ids,
          {
            open Category
            \"++"(inexistant, newline)
          },
        ),
        kind,
      )
    | Beg_of_word => (
        A.seq(
          ids,
          #First,
          A.after(
            ids,
            {
              open Category
              \"++"(inexistant, not_letter)
            },
          ),
          A.before(
            ids,
            {
              open Category
              \"++"(inexistant, letter)
            },
          ),
        ),
        kind,
      )
    | End_of_word => (
        A.seq(
          ids,
          #First,
          A.after(
            ids,
            {
              open Category
              \"++"(inexistant, letter)
            },
          ),
          A.before(
            ids,
            {
              open Category
              \"++"(inexistant, not_letter)
            },
          ),
        ),
        kind,
      )
    | Not_bound => (
        A.alt(
          ids,
          list{
            A.seq(ids, #First, A.after(ids, Category.letter), A.before(ids, Category.letter)),
            A.seq(ids, #First, A.after(ids, Category.letter), A.before(ids, Category.letter)),
          },
        ),
        kind,
      )
    | Beg_of_str => (A.after(ids, Category.inexistant), kind)
    | End_of_str => (A.before(ids, Category.inexistant), kind)
    | Last_end_of_line => (
        A.before(
          ids,
          {
            open Category
            \"++"(inexistant, lastnewline)
          },
        ),
        kind,
      )
    | Start => (A.after(ids, Category.search_boundary), kind)
    | Stop => (A.before(ids, Category.search_boundary), kind)
    | Sem(kind', r') =>
      let (cr, kind'') = translate(ids, kind', ign_group, ign_case, greedy, pos, cache, c, r')
      (enforce_kind(ids, kind', kind'', cr), kind')
    | Sem_greedy(greedy', r') =>
      translate(ids, kind, ign_group, ign_case, greedy', pos, cache, c, r')
    | Group(r') =>
      if ign_group {
        translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r')
      } else {
        let p = pos.contents
        pos := pos.contents + 2
        let (cr, kind') = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r')
        (A.seq(ids, #First, A.mark(ids, p), A.seq(ids, #First, cr, A.mark(ids, p + 1))), kind')
      }
    | No_group(r') => translate(ids, kind, true, ign_case, greedy, pos, cache, c, r')
    | Nest(r') =>
      let b = pos.contents
      let (cr, kind') = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r')

      let e = pos.contents - 1
      if e < b {
        (cr, kind')
      } else {
        (A.seq(ids, #First, A.erase(ids, b, e), cr), kind')
      }
    | Difference(_) | Complement(_) | Intersection(_) | No_case(_) | Case(_) => assert(false)
    | Pmark(i, r') =>
      let (cr, kind') = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r')
      (A.seq(ids, #First, A.pmark(ids, i), cr), kind')
    }

  and trans_seq = (ids, kind, ign_group, ign_case, greedy, pos, cache, c, x) =>
    switch x {
    | list{} => A.eps(ids)
    | list{r} =>
      let (cr', kind') = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r)
      enforce_kind(ids, kind, kind', cr')
    | list{r, ...rem} =>
      let (cr', kind') = translate(ids, kind, ign_group, ign_case, greedy, pos, cache, c, r)
      let cr'' = trans_seq(ids, kind, ign_group, ign_case, greedy, pos, cache, c, rem)
      if A.is_eps(cr'') {
        cr'
      } else if A.is_eps(cr') {
        cr''
      } else {
        A.seq(ids, kind', cr', cr'')
      }
    }

  /* *** Case *** */

  let case_insens = s =>
    Cset.union(
      s,
      Cset.union(Cset.offset(32, Cset.inter(s, cupper)), Cset.offset(-32, Cset.inter(s, clower))),
    )

  let as_set = x =>
    switch x {
    | Set(s) => s
    | _ => assert(false)
    }

  /* XXX Should split alternatives into (1) charsets and (2) more
   complex regular expressions; alternative should therefore probably
   be flatten here */
  let rec handle_case = (ign_case, x) =>
    switch x {
    | Set(s) =>
      Set(
        if ign_case {
          case_insens(s)
        } else {
          s
        },
      )
    | Sequence(l) => Sequence(List.map(handle_case(ign_case), l))
    | Alternative(l) =>
      let l' = List.map(handle_case(ign_case), l)
      if is_charset(Alternative(l')) {
        Set(List.fold_left((s, r) => Cset.union(s, as_set(r)), Cset.empty, l'))
      } else {
        Alternative(l')
      }
    | Repeat(r, i, j) => Repeat(handle_case(ign_case, r), i, j)
    | (Beg_of_line
      | End_of_line
      | Beg_of_word
      | End_of_word
      | Not_bound
      | Beg_of_str
      | End_of_str
      | Last_end_of_line
      | Start
      | Stop) as r => r
    | Sem(k, r) =>
      let r' = handle_case(ign_case, r)
      if is_charset(r') {
        r'
      } else {
        Sem(k, r')
      }
    | Sem_greedy(k, r) =>
      let r' = handle_case(ign_case, r)
      if is_charset(r') {
        r'
      } else {
        Sem_greedy(k, r')
      }
    | Group(r) => Group(handle_case(ign_case, r))
    | No_group(r) =>
      let r' = handle_case(ign_case, r)
      if is_charset(r') {
        r'
      } else {
        No_group(r')
      }
    | Nest(r) =>
      let r' = handle_case(ign_case, r)
      if is_charset(r') {
        r'
      } else {
        Nest(r')
      }
    | Case(r) => handle_case(false, r)
    | No_case(r) => handle_case(true, r)
    | Intersection(l) =>
      let l' = List.map(r => handle_case(ign_case, r), l)
      Set(List.fold_left((s, r) => Cset.inter(s, as_set(r)), Cset.cany, l'))
    | Complement(l) =>
      let l' = List.map(r => handle_case(ign_case, r), l)
      Set(Cset.diff(Cset.cany, List.fold_left((s, r) => Cset.union(s, as_set(r)), Cset.empty, l')))
    | Difference(r, r') =>
      Set(
        Cset.inter(
          as_set(handle_case(ign_case, r)),
          Cset.diff(Cset.cany, as_set(handle_case(ign_case, r'))),
        ),
      )
    | Pmark(i, r) => Pmark(i, handle_case(ign_case, r))
    }

  /* ** */

  let compile_1 = regexp => {
    let regexp = handle_case(false, regexp)
    let c = make_cmap()
    let need_lnl = colorize(c, regexp)
    let (col, col_repr, ncol) = flatten_cmap(c)
    let lnl = if need_lnl {
      ncol
    } else {
      -1
    }
    let ncol = if need_lnl {
      ncol + 1
    } else {
      ncol
    }
    let ids = A.create_ids()
    let pos = ref(0)
    let (r, kind) = translate(
      ids,
      #First,
      false,
      false,
      #Greedy,
      pos,
      ref(Cset.CSetMap.empty),
      col,
      regexp,
    )
    let r = enforce_kind(ids, #First, kind, r)
    /* Format.eprintf "<%d %d>@." !ids ncol; */
    mk_re(r, col, col_repr, ncol, lnl, pos.contents / 2)
  }

  /* ** */

  let rec anchored = x =>
    switch x {
    | Sequence(l) => List.exists(anchored, l)
    | Alternative(l) => List.for_all(anchored, l)
    | Repeat(r, i, _) => i > 0 && anchored(r)
    | Set(_)
    | Beg_of_line
    | End_of_line
    | Beg_of_word
    | End_of_word
    | Not_bound
    | End_of_str
    | Last_end_of_line
    | Stop
    | Intersection(_)
    | Complement(_)
    | Difference(_) => false
    | Beg_of_str | Start => true
    | Sem(_, r)
    | Sem_greedy(_, r)
    | Group(r)
    | No_group(r)
    | Nest(r)
    | Case(r)
    | No_case(r)
    | Pmark(_, r) =>
      anchored(r)
    }

  /* ** */

  type t = regexp

  let str = s => {
    let l = ref(list{})
    for i in String.length(s) - 1 downto 0 {
      l := list{Set(Cset.csingle(String.get(s, i))), ...l.contents}
    }
    Sequence(l.contents)
  }
  let char = c => Set(Cset.csingle(c))

  let alt = x =>
    switch x {
    | list{r} => r
    | l => Alternative(l)
    }
  let seq = x =>
    switch x {
    | list{r} => r
    | l => Sequence(l)
    }

  let empty = alt(list{})
  let epsilon = seq(list{})
  let repn = (r, i, j) => {
    if i < 0 {
      invalid_arg("Re.repn")
    }
    switch j {
    | Some(j) if j < i => invalid_arg("Re.repn")
    | _ => ()
    }
    Repeat(r, i, j)
  }
  let rep = r => repn(r, 0, None)
  let rep1 = r => repn(r, 1, None)
  let opt = r => repn(r, 0, Some(1))
  let bol = Beg_of_line
  let eol = End_of_line
  let bow = Beg_of_word
  let eow = End_of_word
  let word = r => seq(list{bow, r, eow})
  let not_boundary = Not_bound
  let bos = Beg_of_str
  let eos = End_of_str
  let whole_string = r => seq(list{bos, r, eos})
  let leol = Last_end_of_line
  let start = Start
  let stop = Stop
  let longest = r => Sem(#Longest, r)
  let shortest = r => Sem(#Shortest, r)
  let first = r => Sem(#First, r)
  let greedy = r => Sem_greedy(#Greedy, r)
  let non_greedy = r => Sem_greedy(#Non_greedy, r)
  let group = r => Group(r)
  let no_group = r => No_group(r)
  let nest = r => Nest(r)
  let mark = r => {
    let i = Automata.Pmark.gen()
    (i, Pmark(i, r))
  }

  let set = str => {
    let s = ref(Cset.empty)
    for i in 0 to String.length(str) - 1 {
      s := Cset.union(Cset.csingle(String.get(str, i)), s.contents)
    }
    Set(s.contents)
  }

  let rg = (c, c') => Set(cseq(c, c'))

  let inter = l => {
    let r = Intersection(l)
    if is_charset(r) {
      r
    } else {
      invalid_arg("Re.inter")
    }
  }

  let compl = l => {
    let r = Complement(l)
    if is_charset(r) {
      r
    } else {
      invalid_arg("Re.compl")
    }
  }

  let diff = (r, r') => {
    let r'' = Difference(r, r')
    if is_charset(r'') {
      r''
    } else {
      invalid_arg("Re.diff")
    }
  }

  let any = Set(Cset.cany)
  let notnl = Set(Cset.diff(Cset.cany, Cset.csingle('\n')))

  let lower = alt(list{rg('a', 'z'), char('µ'), rg('ß', 'ö'), rg('ø', 'ÿ')})
  let upper = alt(list{rg('A', 'Z'), rg('À', 'Ö'), rg('Ø', 'Þ')})
  let alpha = alt(list{lower, upper, char('ª'), char('º')})
  let digit = rg('0', '9')
  let alnum = alt(list{alpha, digit})
  let wordc = alt(list{alnum, char('_')})
  let ascii = rg(' ', '')
  let blank = set("\t ")
  let cntrl = alt(list{rg(' ', ''), rg('', '')})
  let graph = alt(list{rg('!', '~'), rg(' ', 'ÿ')})
  let print = alt(list{rg(' ', '~'), rg(' ', 'ÿ')})
  let punct = alt(list{
    rg('!', '/'),
    rg(':', '@'),
    rg('[', '`'),
    rg('{', '~'),
    rg(' ', '©'),
    rg('«', '´'),
    rg('¶', '¹'),
    rg('»', '¿'),
    char('×'),
    char('÷'),
  })
  let space = alt(list{char(' '), rg('\t', '\r')})
  let xdigit = alt(list{digit, rg('a', 'f'), rg('A', 'F')})

  let case = r => Case(r)
  let no_case = r => No_case(r)

  /* ** */

  let compile = r =>
    compile_1(
      if anchored(r) {
        group(r)
      } else {
        seq(list{shortest(rep(any)), group(r)})
      },
    )

  let exec_internal = (name, ~pos=0, ~len=-1, ~groups, re, s) => {
    if pos < 0 || (len < -1 || pos + len > String.length(s)) {
      invalid_arg(name)
    }
    match_str(~groups, ~partial=false, re, s, ~pos, ~len)
  }

  let exec = (~pos=?, ~len=?, re, s) =>
    switch exec_internal("Re.exec", ~pos?, ~len?, ~groups=true, re, s) {
    | Match(substr) => substr
    | _ => raise(Not_found)
    }

  let exec_opt = (~pos=?, ~len=?, re, s) =>
    switch exec_internal("Re.exec_opt", ~pos?, ~len?, ~groups=true, re, s) {
    | Match(substr) => Some(substr)
    | _ => None
    }

  let execp = (~pos=?, ~len=?, re, s) =>
    switch exec_internal(~groups=false, "Re.execp", ~pos?, ~len?, re, s) {
    | Match(_substr) => true
    | _ => false
    }

  let exec_partial = (~pos=?, ~len=?, re, s) =>
    switch exec_internal(~groups=false, "Re.exec_partial", ~pos?, ~len?, re, s) {
    | Match(_) => #Full
    | Running => #Partial
    | Failed => #Mismatch
    }

  module Group = {
    type t = groups

    let offset = (t, i) => {
      if 2 * i + 1 >= Array.length(t.marks) {
        raise(Not_found)
      }
      let m1 = t.marks[2 * i]
      if m1 == -1 {
        raise(Not_found)
      }
      let p1 = t.gpos[m1] - 1
      let p2 = t.gpos[t.marks[2 * i + 1]] - 1
      (p1, p2)
    }

    let get = (t, i) => {
      let (p1, p2) = offset(t, i)
      String.sub(t.s, p1, p2 - p1)
    }

    let start = (subs, i) => fst(offset(subs, i))

    let stop = (subs, i) => snd(offset(subs, i))

    let test = (t, i) =>
      if 2 * i >= Array.length(t.marks) {
        false
      } else {
        let idx = t.marks[2 * i]
        idx != -1
      }

    let dummy_offset = (-1, -1)

    let all_offset = t => {
      let res = Array.make(t.gcount, dummy_offset)
      for i in 0 to Array.length(t.marks) / 2 - 1 {
        let m1 = t.marks[2 * i]
        if m1 != -1 {
          let p1 = t.gpos[m1]
          let p2 = t.gpos[t.marks[2 * i + 1]]
          res[i] = (p1 - 1, p2 - 1)
        }
      }
      res
    }

    let dummy_string = ""

    let all = t => {
      let res = Array.make(t.gcount, dummy_string)
      for i in 0 to Array.length(t.marks) / 2 - 1 {
        let m1 = t.marks[2 * i]
        if m1 != -1 {
          let p1 = t.gpos[m1]
          let p2 = t.gpos[t.marks[2 * i + 1]]
          res[i] = String.sub(t.s, p1 - 1, p2 - p1)
        }
      }
      res
    }

    /* let%ignore pp fmt t =
    let matches =
      let offsets = all_offset t in
      let strs = all t in
      Array.to_list (
        Array.init (Array.length strs) (fun i -> strs.(i), offsets.(i))
      ) in
    let open Re_fmt in
    let pp_match fmt (str, (start, stop)) =
      fprintf fmt "@[(%s (%d %d))@]" str start stop in
    sexp fmt "Group" (list pp_match) matches */

    let nb_groups = t => t.gcount
  }

  module Mark = {
    type t = Automata.Pmark.t

    let test = ({pmarks, _}, p) => Automata.PmarkSet.mem(p, pmarks)

    let all = s => s.pmarks

    module Set = MarkSet

    let equal = Automata.Pmark.equal

    let compare = Automata.Pmark.compare
  }

  type gen<'a> = unit => option<'a>

  let all_gen = (~pos=0, ~len=?, re, s) => {
    if pos < 0 {
      invalid_arg("Re.all")
    }
    /* index of the first position we do not consider.
     !pos < limit is an invariant */
    let limit = switch len {
    | None => String.length(s)
    | Some(l) =>
      if l < 0 || pos + l > String.length(s) {
        invalid_arg("Re.all")
      }
      pos + l
    }

    /* iterate on matches. When a match is found, search for the next
     one just after its end */
    let pos = ref(pos)
    () =>
      if pos.contents >= limit {
        None /* no more matches */
      } else {
        switch match_str(
          ~groups=true,
          ~partial=false,
          re,
          s,
          ~pos=pos.contents,
          ~len=limit - pos.contents,
        ) {
        | Match(substr) =>
          let (p1, p2) = Group.offset(substr, 0)
          pos := if p1 == p2 {
              p2 + 1
            } else {
              p2
            }
          Some(substr)
        | Running
        | Failed =>
          None
        }
      }
  }

  let all = (~pos=?, ~len=?, re, s) => {
    let l = ref(list{})
    let g = all_gen(~pos?, ~len?, re, s)
    let rec iter = () =>
      switch g() {
      | None => List.rev(l.contents)
      | Some(sub) =>
        l := list{sub, ...l.contents}
        iter()
      }
    iter()
  }

  let matches_gen = (~pos=?, ~len=?, re, s) => {
    let g = all_gen(~pos?, ~len?, re, s)
    () =>
      switch g() {
      | None => None
      | Some(sub) => Some(Group.get(sub, 0))
      }
  }

  let matches = (~pos=?, ~len=?, re, s) => {
    let l = ref(list{})
    let g = all_gen(~pos?, ~len?, re, s)
    let rec iter = () =>
      switch g() {
      | None => List.rev(l.contents)
      | Some(sub) =>
        l := list{Group.get(sub, 0), ...l.contents}
        iter()
      }
    iter()
  }

  type split_token = [
    | #Text(string)
    | #Delim(groups)
  ]

  let split_full_gen = (~pos=0, ~len=?, re, s) => {
    if pos < 0 {
      invalid_arg("Re.split")
    }
    let limit = switch len {
    | None => String.length(s)
    | Some(l) =>
      if l < 0 || pos + l > String.length(s) {
        invalid_arg("Re.split")
      }
      pos + l
    }

    /* i: start of delimited string
     pos: first position after last match of [re]
     limit: first index we ignore (!pos < limit is an invariant) */
    let pos0 = pos
    let state = ref(#Idle)
    let i = ref(pos) and pos = ref(pos)
    let next = () =>
      switch state.contents {
      | #Idle if pos.contents >= limit =>
        if i.contents < limit {
          let sub = String.sub(s, i.contents, limit - i.contents)
          incr(i)
          Some(#Text(sub))
        } else {
          None
        }
      | #Idle =>
        switch match_str(
          ~groups=true,
          ~partial=false,
          re,
          s,
          ~pos=pos.contents,
          ~len=limit - pos.contents,
        ) {
        | Match(substr) =>
          let (p1, p2) = Group.offset(substr, 0)
          pos := if p1 == p2 {
              p2 + 1
            } else {
              p2
            }
          let old_i = i.contents
          i := p2
          if p1 > pos0 {
            /* string does not start by a delimiter */
            let text = String.sub(s, old_i, p1 - old_i)
            state := #Yield(#Delim(substr))
            Some(#Text(text))
          } else {
            Some(#Delim(substr))
          }
        | Running => None
        | Failed =>
          if i.contents < limit {
            let text = String.sub(s, i.contents, limit - i.contents)
            i := limit
            Some(#Text(text)) /* yield last string */
          } else {
            None
          }
        }
      | #Yield(x) =>
        state := #Idle
        Some(x)
      }
    next
  }

  let split_full = (~pos=?, ~len=?, re, s) => {
    let l = ref(list{})
    let g = split_full_gen(~pos?, ~len?, re, s)
    let rec iter = () =>
      switch g() {
      | None => List.rev(l.contents)
      | Some(s) =>
        l := list{s, ...l.contents}
        iter()
      }
    iter()
  }

  let split_gen = (~pos=?, ~len=?, re, s) => {
    let g = split_full_gen(~pos?, ~len?, re, s)
    let rec next = () =>
      switch g() {
      | None => None
      | Some(#Delim(_)) => next()
      | Some(#Text(s)) => Some(s)
      }
    next
  }

  let split = (~pos=?, ~len=?, re, s) => {
    let l = ref(list{})
    let g = split_full_gen(~pos?, ~len?, re, s)
    let rec iter = () =>
      switch g() {
      | None => List.rev(l.contents)
      | Some(#Delim(_)) => iter()
      | Some(#Text(s)) =>
        l := list{s, ...l.contents}
        iter()
      }
    iter()
  }

  let replace = (~pos=0, ~len=?, ~all=true, re, ~f, s) => {
    if pos < 0 {
      invalid_arg("Re.replace")
    }
    let limit = switch len {
    | None => String.length(s)
    | Some(l) =>
      if l < 0 || pos + l > String.length(s) {
        invalid_arg("Re.replace")
      }
      pos + l
    }

    /* buffer into which we write the result */
    let buf = Buffer.create(String.length(s))
    /* iterate on matched substrings. */
    let rec iter = pos =>
      if pos < limit {
        switch match_str(~groups=true, ~partial=false, re, s, ~pos, ~len=limit - pos) {
        | Match(substr) =>
          let (p1, p2) = Group.offset(substr, 0)
          /* add string between previous match and current match */
          Buffer.add_substring(buf, s, pos, p1 - pos)
          /* what should we replace the matched group with? */
          let replacing = f(substr)
          Buffer.add_string(buf, replacing)
          if all {
            /* if we matched a non-char e.g. ^ we must manually advance by 1 */
            iter(
              if p1 == p2 {
                /* a non char could be past the end of string. e.g. $ */
                if p2 < limit {
                  Buffer.add_char(buf, String.get(s, p2))
                }
                p2 + 1
              } else {
                p2
              },
            )
          } else {
            Buffer.add_substring(buf, s, p2, limit - p2)
          }
        | Running => ()
        | Failed => Buffer.add_substring(buf, s, pos, limit - pos)
        }
      }

    iter(pos)
    Buffer.contents(buf)
  }

  let replace_string = (~pos=?, ~len=?, ~all=?, re, ~by, s) =>
    replace(~pos?, ~len?, ~all?, re, s, ~f=_ => by)

  let witness = t => {
    let rec witness = x =>
      switch x {
      | Set(c) => String.make(1, Char.chr(Cset.pick(c)))
      | Sequence(xs) => String.concat("", List.map(witness, xs))
      | Alternative(list{x, ..._}) => witness(x)
      | Alternative(list{}) => assert(false)
      | Repeat(r, from, _to) =>
        let w = witness(r)
        let b = Buffer.create(String.length(w) * from)
        for _i in 1 to from {
          Buffer.add_string(b, w)
        }
        Buffer.contents(b)
      | No_case(r) => witness(r)
      | Intersection(_)
      | Complement(_)
      | Difference(_, _) =>
        assert(false)
      | Group(r)
      | No_group(r)
      | Nest(r)
      | Sem(_, r)
      | Pmark(_, r)
      | Case(r)
      | Sem_greedy(_, r) =>
        witness(r)
      | Beg_of_line
      | End_of_line
      | Beg_of_word
      | End_of_word
      | Not_bound
      | Beg_of_str
      | Last_end_of_line
      | Start
      | Stop
      | End_of_str => ""
      }
    witness(handle_case(false, t))
  }

  @@ocaml.text(" {2 Deprecated functions} ")

  type substrings = groups

  let get = Group.get
  let get_ofs = Group.offset
  let get_all = Group.all
  let get_all_ofs = Group.all_offset
  let test = Group.test

  type markid = Mark.t

  let marked = Mark.test
  let mark_set = Mark.all

  /* ******************************** */

  /*
Information about the previous character:
- does not exists
- is a letter
- is not a letter
- is a newline
- is last newline

Beginning of word:
- previous is not a letter or does not exist
- current is a letter or does not exist

End of word:
- previous is a letter or does not exist
- current is not a letter or does not exist

Beginning of line:
- previous is a newline or does not exist

Beginning of buffer:
- previous does not exist

End of buffer
- current does not exist

End of line
- current is a newline or does not exist
*/

  /*
Rep: e = T,e | ()
  - semantics of the comma (shortest/longest/first)
  - semantics of the union (greedy/non-greedy)

Bounded repetition
  a{0,3} = (a,(a,a?)?)?
*/
}
module Re_perl: {
  @@ocaml.text(
    /*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*/

    " Perl-style regular expressions "
  )

  exception Parse_error
  @ocaml.doc(" Errors that can be raised during the parsing of the regular expression ")
  exception Not_supported

  type opt = [
    | #Ungreedy
    | #Dotall
    | #Dollar_endonly
    | #Multiline
    | #Anchored
    | #Caseless
  ]

  @ocaml.doc(" Parsing of a Perl-style regular expression ")
  let re: (~opts: list<opt>=?, string) => Re.t

  @ocaml.doc(" Regular expression compilation ")
  let compile: Re.t => Re.re

  @ocaml.doc(" (Same as [Re.compile]) ")
  let compile_pat: (~opts: list<opt>=?, string) => Re.re
} = {
  /*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*/

  exception Parse_error
  exception Not_supported

  let posix_class_of_string = x =>
    switch x {
    | "alnum" => Re.alnum
    | "ascii" => Re.ascii
    | "blank" => Re.blank
    | "cntrl" => Re.cntrl
    | "digit" => Re.digit
    | "lower" => Re.lower
    | "print" => Re.print
    | "space" => Re.space
    | "upper" => Re.upper
    | "word" => Re.wordc
    | "punct" => Re.punct
    | "graph" => Re.graph
    | "xdigit" => Re.xdigit
    | class_ => invalid_arg("Invalid pcre class: " ++ class_)
    }

  let posix_class_strings = list{
    "alnum",
    "ascii",
    "blank",
    "cntrl",
    "digit",
    "lower",
    "print",
    "space",
    "upper",
    "word",
    "punct",
    "graph",
    "xdigit",
  }

  let parse = (multiline, dollar_endonly, dotall, ungreedy, s) => {
    let i = ref(0)
    let l = String.length(s)
    let eos = () => i.contents == l
    let test = c => !eos() && String.get(s, i.contents) == c
    let accept = c => {
      let r = test(c)
      if r {
        incr(i)
      }
      r
    }
    let accept_s = s' => {
      let len = String.length(s')
      try {
        for j in 0 to len - 1 {
          try if String.get(s', j) != String.get(s, i.contents + j) {
            raise(Exit)
          } catch {
          | _ => raise(Exit)
          }
        }
        i := i.contents + len
        true
      } catch {
      | Exit => false
      }
    }
    let get = () => {
      let r = String.get(s, i.contents)
      incr(i)
      r
    }
    let unget = () => decr(i)
    let greedy_mod = r => {
      let gr = accept('?')
      let gr = if ungreedy {
        !gr
      } else {
        gr
      }
      if gr {
        Re.non_greedy(r)
      } else {
        Re.greedy(r)
      }
    }

    let rec regexp = () => regexp'(branch())
    and regexp' = left =>
      if accept('|') {
        regexp'(Re.alt(list{left, branch()}))
      } else {
        left
      }
    and branch = () => branch'(list{})
    and branch' = left =>
      if eos() || (test('|') || test(')')) {
        Re.seq(List.rev(left))
      } else {
        branch'(list{piece(), ...left})
      }
    and piece = () => {
      let r = atom()
      if accept('*') {
        greedy_mod(Re.rep(r))
      } else if accept('+') {
        greedy_mod(Re.rep1(r))
      } else if accept('?') {
        greedy_mod(Re.opt(r))
      } else if accept('{') {
        switch integer() {
        | Some(i) =>
          let j = if accept(',') {
            integer()
          } else {
            Some(i)
          }
          if !accept('}') {
            raise(Parse_error)
          }
          switch j {
          | Some(j) if j < i => raise(Parse_error)
          | _ => ()
          }
          greedy_mod(Re.repn(r, i, j))
        | None =>
          unget()
          r
        }
      } else {
        r
      }
    }
    and atom = () =>
      if accept('.') {
        if dotall {
          Re.any
        } else {
          Re.notnl
        }
      } else if accept('(') {
        if accept('?') {
          if accept(':') {
            let r = regexp()
            if !accept(')') {
              raise(Parse_error)
            }
            r
          } else if accept('#') {
            comment()
          } else {
            raise(Parse_error)
          }
        } else {
          let r = regexp()
          if !accept(')') {
            raise(Parse_error)
          }
          Re.group(r)
        }
      } else if accept('^') {
        if multiline {
          Re.bol
        } else {
          Re.bos
        }
      } else if accept('$') {
        if multiline {
          Re.eol
        } else if dollar_endonly {
          Re.leol
        } else {
          Re.eos
        }
      } else if accept('[') {
        if accept('^') {
          Re.compl(bracket(list{}))
        } else {
          Re.alt(bracket(list{}))
        }
      } else if accept('\\') {
        /* XXX
   - Back-references
   - \cx (control-x), \e, \f, \n, \r, \t, \xhh, \ddd
*/
        if eos() {
          raise(Parse_error)
        }
        switch get() {
        | 'w' => Re.alt(list{Re.alnum, Re.char('_')})
        | 'W' => Re.compl(list{Re.alnum, Re.char('_')})
        | 's' => Re.space
        | 'S' => Re.compl(list{Re.space})
        | 'd' => Re.digit
        | 'D' => Re.compl(list{Re.digit})
        | 'b' => Re.alt(list{Re.bow, Re.eow})
        | 'B' => Re.not_boundary
        | 'A' => Re.bos
        | 'Z' => Re.leol
        | 'z' => Re.eos
        | 'G' => Re.start
        | 'a' .. 'z' | 'A' .. 'Z' => raise(Parse_error)
        | '0' .. '9' => raise(Not_supported)
        | c => Re.char(c)
        }
      } else {
        if eos() {
          raise(Parse_error)
        }
        switch get() {
        | '*' | '+' | '?' | '{' | '\\' => raise(Parse_error)
        | c => Re.char(c)
        }
      }
    and integer = () =>
      if eos() {
        None
      } else {
        switch get() {
        | '0' .. '9' as d => integer'(Char.code(d) - Char.code('0'))
        | _ =>
          unget()
          None
        }
      }
    and integer' = i =>
      if eos() {
        Some(i)
      } else {
        switch get() {
        | '0' .. '9' as d =>
          let i' = 10 * i + (Char.code(d) - Char.code('0'))
          if i' < i {
            raise(Parse_error)
          }
          integer'(i')
        | _ =>
          unget()
          Some(i)
        }
      }
    and bracket = s =>
      if s != list{} && accept(']') {
        s
      } else {
        switch char() {
        | #Char(c) =>
          if accept('-') {
            if accept(']') {
              list{Re.char(c), Re.char('-'), ...s}
            } else {
              switch char() {
              | #Char(c') => bracket(list{Re.rg(c, c'), ...s})
              | #Set(st') => list{Re.char(c), Re.char('-'), st', ...s}
              }
            }
          } else {
            bracket(list{Re.char(c), ...s})
          }
        | #Set(st) => bracket(list{st, ...s})
        }
      }
    and char = () => {
      if eos() {
        raise(Parse_error)
      }
      let c = get()
      if c == '[' {
        if accept('=') {
          raise(Not_supported)
        }
        if accept(':') {
          let compl = accept('^')
          let cls = try List.find(accept_s, posix_class_strings) catch {
          | Not_found => raise(Parse_error)
          }
          if !accept_s(":]") {
            raise(Parse_error)
          }
          let re = {
            let posix_class = posix_class_of_string(cls)
            if compl {
              Re.compl(list{posix_class})
            } else {
              posix_class
            }
          }
          #Set(re)
        } else if accept('.') {
          if eos() {
            raise(Parse_error)
          }
          let c = get()
          if !accept('.') {
            raise(Not_supported)
          }
          if !accept(']') {
            raise(Parse_error)
          }
          #Char(c)
        } else {
          #Char(c)
        }
      } else if c == '\\' {
        let c = get()
        /* XXX
   \127, ...
*/
        switch c {
        | 'b' => #Char('\b')
        | 'n' => #Char('\n') /* XXX */
        | 'r' => #Char('\r') /* XXX */
        | 't' => #Char('\t') /* XXX */
        | 'w' => #Set(Re.alt(list{Re.alnum, Re.char('_')}))
        | 'W' => #Set(Re.compl(list{Re.alnum, Re.char('_')}))
        | 's' => #Set(Re.space)
        | 'S' => #Set(Re.compl(list{Re.space}))
        | 'd' => #Set(Re.digit)
        | 'D' => #Set(Re.compl(list{Re.digit}))
        | 'a' .. 'z' | 'A' .. 'Z' => raise(Parse_error)
        | '0' .. '9' => raise(Not_supported)
        | _ => #Char(c)
        }
      } else {
        #Char(c)
      }
    }
    and comment = () =>
      if accept(')') {
        Re.epsilon
      } else {
        incr(i)
        comment()
      }

    let res = regexp()
    if !eos() {
      raise(Parse_error)
    }
    res
  }

  type opt = [
    | #Ungreedy
    | #Dotall
    | #Dollar_endonly
    | #Multiline
    | #Anchored
    | #Caseless
  ]

  let re = (~opts=list{}, s) => {
    let r = parse(
      List.memq(#Multiline, opts),
      List.memq(#Dollar_endonly, opts),
      List.memq(#Dotall, opts),
      List.memq(#Ungreedy, opts),
      s,
    )

    let r = if List.memq(#Anchored, opts) {
      Re.seq(list{Re.start, r})
    } else {
      r
    }
    let r = if List.memq(#Caseless, opts) {
      Re.no_case(r)
    } else {
      r
    }
    r
  }

  let compile = Re.compile
  let compile_pat = (~opts=list{}, s) => compile(re(~opts, s))
}
module Re_pcre: {
  type regexp = Re.re

  type flag = [#CASELESS | #MULTILINE | #ANCHORED]

  type groups = Re.groups

  @ocaml.doc(" Result of a {!Pcre.full_split} ")
  type split_result =
    | @ocaml.doc(" Text part of splitted string ") Text(string)
    | @ocaml.doc(" Delimiter part of splitted string ") Delim(string)
    | @ocaml.doc(" Subgroup of matched delimiter (subgroup_nr, subgroup_str) ") Group(int, string)
    | @ocaml.doc(" Unmatched subgroup ") NoGroup

  @ocaml.doc(" [re ~flags s] creates the regexp [s] using the pcre syntax. ")
  let re: (~flags: list<flag>=?, string) => Re.t

  @ocaml.doc(" [re ~flags s] compiles the regexp [s] using the pcre syntax. ")
  let regexp: (~flags: list<flag>=?, string) => regexp

  @ocaml.doc(" [extract ~rex s] executes [rex] on [s] and returns the matching groups. ")
  let extract: (~rex: regexp, string) => array<string>

  @ocaml.doc(" Equivalent to {!Re.exec}. ")
  let exec: (~rex: regexp, ~pos: int=?, string) => groups

  @ocaml.doc(" Equivalent to {!Re.Group.get}. ")
  let get_substring: (groups, int) => string

  @ocaml.doc(" Equivalent to {!Re.Group.offset}. ")
  let get_substring_ofs: (groups, int) => (int, int)

  @ocaml.doc(" Equivalent to {!Re.execp}. ")
  let pmatch: (~rex: regexp, string) => bool

  let substitute: (~rex: Re.re, ~subst: string => string, string) => string

  let full_split: (~max: int=?, ~rex: regexp, string) => list<split_result>

  let split: (~rex: regexp, string) => list<string>

  let quote: string => string

  @@ocaml.text(" {2 Deprecated} ")

  type substrings = Re.groups
} = {
  type regexp = Re.re

  type flag = [#CASELESS | #MULTILINE | #ANCHORED]

  type split_result =
    | Text(string)
    | Delim(string)
    | Group(int, string)
    | NoGroup

  type groups = Re.groups

  let re = (~flags=list{}, pat) => {
    let opts = List.map(x =>
      switch x {
      | #CASELESS => #Caseless
      | #MULTILINE => #Multiline
      | #ANCHORED => #Anchored
      }
    , flags)
    Re_perl.re(~opts, pat)
  }

  let regexp = (~flags=?, pat) => Re.compile(re(~flags?, pat))

  let extract = (~rex, s) => Re.Group.all(Re.exec(rex, s))

  let exec = (~rex, ~pos=?, s) => Re.exec(rex, ~pos?, s)

  let get_substring = (s, i) => Re.Group.get(s, i)

  let get_substring_ofs = (s, i) => Re.Group.offset(s, i)

  let pmatch = (~rex, s) => Re.execp(rex, s)

  let substitute = (~rex, ~subst, str) => {
    let b = Buffer.create(1024)
    let rec loop = pos =>
      if pos >= String.length(str) {
        Buffer.contents(b)
      } else if Re.execp(~pos, rex, str) {
        let ss = Re.exec(~pos, rex, str)
        let (start, fin) = Re.Group.offset(ss, 0)
        let pat = Re.Group.get(ss, 0)
        Buffer.add_substring(b, str, pos, start - pos)
        Buffer.add_string(b, subst(pat))
        loop(fin)
      } else {
        Buffer.add_substring(b, str, pos, String.length(str) - pos)
        loop(String.length(str))
      }

    loop(0)
  }

  let split = (~rex, str) => {
    let rec loop = (accu, pos) =>
      if pos >= String.length(str) {
        List.rev(accu)
      } else if Re.execp(~pos, rex, str) {
        let ss = Re.exec(~pos, rex, str)
        let (start, fin) = Re.Group.offset(ss, 0)
        let s = String.sub(str, pos, start - pos)
        loop(list{s, ...accu}, fin)
      } else {
        let s = String.sub(str, pos, String.length(str) - pos)
        loop(list{s, ...accu}, String.length(str))
      }
    loop(list{}, 0)
  }

  /* From PCRE */
  let string_unsafe_sub = (s, ofs, len) => {
    let r = Bytes.create(len)
    Bytes.blit(s, ofs, r, 0, len)
    Bytes.unsafe_to_string(r)
  }

  let quote = s => {
    let len = String.length(s)
    let buf = Bytes.create(lsl(len, 1))
    let pos = ref(0)
    for i in 0 to len - 1 {
      switch String.unsafe_get(s, i) {
      | ('\\'
        | '^'
        | '$'
        | '.'
        | '['
        | '|'
        | '('
        | ')'
        | '?'
        | '*'
        | '+'
        | '{') as c =>
        Bytes.unsafe_set(buf, pos.contents, '\\')
        incr(pos)
        Bytes.unsafe_set(buf, pos.contents, c)
        incr(pos)
      | c =>
        Bytes.unsafe_set(buf, pos.contents, c)
        incr(pos)
      }
    }
    string_unsafe_sub(buf, 0, pos.contents)
  }

  let full_split = (~max=0, ~rex, s) =>
    if String.length(s) == 0 {
      list{}
    } else if max == 1 {
      list{Text(s)}
    } else {
      let results = Re.split_full(rex, s)
      let matches = List.map(x =>
        switch x {
        | #Text(s) => list{Text(s)}
        | #Delim(d) =>
          let matches = Re.Group.all_offset(d)
          let delim = Re.Group.get(d, 0)
          list{
            Delim(delim),
            ...{
              let l = ref(list{})
              for i in 1 to Array.length(matches) - 1 {
                l :=
                  list{
                    if matches[i] == (-1, -1) {
                      NoGroup
                    } else {
                      Group(i, Re.Group.get(d, i))
                    },
                    ...l.contents,
                  }
              }
              List.rev(l.contents)
            },
          }
        }
      , results)
      List.concat(matches)
    }

  type substrings = Re.groups
}
module Xx = {
  let _ = {
    let s = String.make(1024 * 1024 - 1, 'a') ++ "b"

    eq(__LOC__, Re.get(Re_pcre.exec(~rex=Re_pcre.regexp("aa?b"), s), 0), "aab")
  }
}

Mt.from_pair_suites(__MODULE__, suites.contents)
