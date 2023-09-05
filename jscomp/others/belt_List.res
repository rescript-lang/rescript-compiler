/* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

/*
  perf is not everything, there are better memory represenations

  ```
  type 'a cell = {
    mutable head : 'a;
    mutable tail : 'a opt_cell
  }

  and 'a opt_cell = 'a cell Js.null

  and 'a t = {
    length : int ;
    data : 'a opt_cell
  }
  ```
  However,
  - people use List not because of its perf, but its
    convenience, in that case, pattern match and compatibility seems
    more attractive, we could keep a mutable list
  - The built in types would indicate that
    its construtor is immutable, a better optimizer would break such code

  ```
  type 'a t = {
    head : 'a;
    mutable tail : 'a t | int
  }
  ```
  In the future, we could come up with a safer version
  ```
  type 'a t =
  | Nil
  | Cons of { hd : 'a ; mutable tail : 'a t }
  ```
*/

@@bs.config({flags: ["-bs-noassertfalse"]})

type t<'a> = list<'a>

module A = Belt_Array

external mutableCell: ('a, t<'a>) => t<'a> = "#makemutablelist"

/*
  `mutableCell x []` == `x`
  but tell the compiler that is a mutable cell, so it wont
  be mis-inlined in the future
   dont inline a binding to mutable cell, it is mutable
*/
/* INVARIANT: relies on Literals.tl (internal representation) */
@set external unsafeMutateTail: (t<'a>, t<'a>) => unit = "tl"

/*
   - the cell is not empty
*/

let head = x =>
  switch x {
  | list{} => None
  | list{x, ..._} => Some(x)
  }

let headExn = x =>
  switch x {
  | list{} => raise(Not_found)
  | list{x, ..._} => x
  }

let tail = x =>
  switch x {
  | list{} => None
  | list{_, ...xs} => Some(xs)
  }

let tailExn = x =>
  switch x {
  | list{} => raise(Not_found)
  | list{_, ...t} => t
  }

let add = (xs, x) => list{x, ...xs}

/* Assume `n >=0` */
let rec nthAux = (x, n) =>
  switch x {
  | list{h, ...t} =>
    if n == 0 {
      Some(h)
    } else {
      nthAux(t, n - 1)
    }
  | _ => None
  }

let rec nthAuxAssert = (x, n) =>
  switch x {
  | list{h, ...t} =>
    if n == 0 {
      h
    } else {
      nthAuxAssert(t, n - 1)
    }
  | _ => raise(Not_found)
  }

let get = (x, n) =>
  if n < 0 {
    None
  } else {
    nthAux(x, n)
  }

let getExn = (x, n) =>
  if n < 0 {
    raise(Not_found)
  } else {
    nthAuxAssert(x, n)
  }

let rec partitionAux = (p, cell, precX, precY) =>
  switch cell {
  | list{} => ()
  | list{h, ...t} =>
    let next = mutableCell(h, list{})
    if p(. h) {
      unsafeMutateTail(precX, next)
      partitionAux(p, t, next, precY)
    } else {
      unsafeMutateTail(precY, next)
      partitionAux(p, t, precX, next)
    }
  }

let rec splitAux = (cell, precX, precY) =>
  switch cell {
  | list{} => ()
  | list{(a, b), ...t} =>
    let nextA = mutableCell(a, list{})
    let nextB = mutableCell(b, list{})
    unsafeMutateTail(precX, nextA)
    unsafeMutateTail(precY, nextB)
    splitAux(t, nextA, nextB)
  }

/* return the tail pointer so it can continue copy other
   list
*/
let rec copyAuxCont = (cellX, prec) =>
  switch cellX {
  | list{} => prec
  | list{h, ...t} =>
    let next = mutableCell(h, list{})
    unsafeMutateTail(prec, next)
    copyAuxCont(t, next)
  }

let rec copyAuxWitFilter = (f, cellX, prec) =>
  switch cellX {
  | list{} => ()
  | list{h, ...t} =>
    if f(. h) {
      let next = mutableCell(h, list{})
      unsafeMutateTail(prec, next)
      copyAuxWitFilter(f, t, next)
    } else {
      copyAuxWitFilter(f, t, prec)
    }
  }

let rec copyAuxWithFilterIndex = (f, cellX, prec, i) =>
  switch cellX {
  | list{} => ()
  | list{h, ...t} =>
    if f(. h, i) {
      let next = mutableCell(h, list{})
      unsafeMutateTail(prec, next)
      copyAuxWithFilterIndex(f, t, next, i + 1)
    } else {
      copyAuxWithFilterIndex(f, t, prec, i + 1)
    }
  }

let rec copyAuxWitFilterMap = (f, cellX, prec) =>
  switch cellX {
  | list{} => ()
  | list{h, ...t} =>
    switch f(. h) {
    | Some(h) =>
      let next = mutableCell(h, list{})
      unsafeMutateTail(prec, next)
      copyAuxWitFilterMap(f, t, next)
    | None => copyAuxWitFilterMap(f, t, prec)
    }
  }

let rec removeAssocAuxWithMap = (cellX, x, prec, f) =>
  switch cellX {
  | list{} => false
  | list{(a, _) as h, ...t} =>
    if f(. a, x) {
      unsafeMutateTail(prec, t)
      true
    } else {
      let next = mutableCell(h, list{})
      unsafeMutateTail(prec, next)
      removeAssocAuxWithMap(t, x, next, f)
    }
  }

let rec setAssocAuxWithMap = (cellX, x, k, prec, eq) =>
  switch cellX {
  | list{} => false
  | list{(a, _) as h, ...t} =>
    if eq(. a, x) {
      unsafeMutateTail(prec, list{(x, k), ...t})
      true
    } else {
      let next = mutableCell(h, list{})
      unsafeMutateTail(prec, next)
      setAssocAuxWithMap(t, x, k, next, eq)
    }
  }

let rec copyAuxWithMap = (cellX, prec, f) =>
  switch cellX {
  | list{} => ()
  | list{h, ...t} =>
    let next = mutableCell(f(. h), list{})
    unsafeMutateTail(prec, next)
    copyAuxWithMap(t, next, f)
  }

let rec zipAux = (cellX, cellY, prec) =>
  switch (cellX, cellY) {
  | (list{h1, ...t1}, list{h2, ...t2}) =>
    let next = mutableCell((h1, h2), list{})
    unsafeMutateTail(prec, next)
    zipAux(t1, t2, next)
  | (list{}, _) | (_, list{}) => ()
  }

let rec copyAuxWithMap2 = (f, cellX, cellY, prec) =>
  switch (cellX, cellY) {
  | (list{h1, ...t1}, list{h2, ...t2}) =>
    let next = mutableCell(f(. h1, h2), list{})
    unsafeMutateTail(prec, next)
    copyAuxWithMap2(f, t1, t2, next)
  | (list{}, _) | (_, list{}) => ()
  }

let rec copyAuxWithMapI = (f, i, cellX, prec) =>
  switch cellX {
  | list{h, ...t} =>
    let next = mutableCell(f(. i, h), list{})
    unsafeMutateTail(prec, next)
    copyAuxWithMapI(f, i + 1, t, next)
  | list{} => ()
  }

let rec takeAux = (n, cell, prec) =>
  if n == 0 {
    true
  } else {
    switch cell {
    | list{} => false
    | list{x, ...xs} =>
      let cell = mutableCell(x, list{})
      unsafeMutateTail(prec, cell)
      takeAux(n - 1, xs, cell)
    }
  }

let rec splitAtAux = (n, cell, prec) =>
  if n == 0 {
    Some(cell)
  } else {
    switch cell {
    | list{} => None
    | list{x, ...xs} =>
      let cell = mutableCell(x, list{})
      unsafeMutateTail(prec, cell)
      splitAtAux(n - 1, xs, cell)
    }
  }

/* invarint `n >= 0` */
let take = (lst, n) =>
  if n < 0 {
    None
  } else if n == 0 {
    Some(list{})
  } else {
    switch lst {
    | list{} => None
    | list{x, ...xs} =>
      let cell = mutableCell(x, list{})
      let has = takeAux(n - 1, xs, cell)
      if has {
        Some(cell)
      } else {
        None
      }
    }
  }
/* invariant `n >= 0 ` */
let rec dropAux = (l, n) =>
  if n == 0 {
    Some(l)
  } else {
    switch l {
    | list{_, ...tl} => dropAux(tl, n - 1)
    | list{} => None
    }
  }

let drop = (lst, n) =>
  if n < 0 {
    None
  } else {
    dropAux(lst, n)
  }

let splitAt = (lst, n) =>
  if n < 0 {
    None
  } else if n == 0 {
    Some(list{}, lst)
  } else {
    switch lst {
    | list{} => None
    | list{x, ...xs} =>
      let cell = mutableCell(x, list{})
      let rest = splitAtAux(n - 1, xs, cell)
      switch rest {
      | Some(rest) => Some(cell, rest)
      | None => None
      }
    }
  }

let concat = (xs, ys) =>
  switch xs {
  | list{} => ys
  | list{h, ...t} =>
    let cell = mutableCell(h, list{})
    unsafeMutateTail(copyAuxCont(t, cell), ys)
    cell
  }

let mapU = (xs, f) =>
  switch xs {
  | list{} => list{}
  | list{h, ...t} =>
    let cell = mutableCell(f(. h), list{})
    copyAuxWithMap(t, cell, f)
    cell
  }

let map = (xs, f) => mapU(xs, (. x) => f(x))

let zipByU = (l1, l2, f) =>
  switch (l1, l2) {
  | (list{a1, ...l1}, list{a2, ...l2}) =>
    let cell = mutableCell(f(. a1, a2), list{})
    copyAuxWithMap2(f, l1, l2, cell)
    cell
  | (list{}, _) | (_, list{}) => list{}
  }

let zipBy = (l1, l2, f) => zipByU(l1, l2, (. x, y) => f(x, y))

let mapWithIndexU = (xs, f) =>
  switch xs {
  | list{} => list{}
  | list{h, ...t} =>
    let cell = mutableCell(f(. 0, h), list{})
    copyAuxWithMapI(f, 1, t, cell)
    cell
  }

let mapWithIndex = (xs, f) => mapWithIndexU(xs, (. i, x) => f(i, x))

let makeByU = (n, f) =>
  if n <= 0 {
    list{}
  } else {
    let headX = mutableCell(f(. 0), list{})
    let cur = ref(headX)
    let i = ref(1)
    while i.contents < n {
      let v = mutableCell(f(. i.contents), list{})
      unsafeMutateTail(cur.contents, v)
      cur.contents = v
      i.contents = i.contents + 1
    }

    headX
  }

let makeBy = (n, f) => makeByU(n, (. x) => f(x))

let make = (type a, n, v: a): list<a> =>
  if n <= 0 {
    list{}
  } else {
    let headX = mutableCell(v, list{})
    let cur = ref(headX)
    let i = ref(1)
    while i.contents < n {
      let v = mutableCell(v, list{})
      unsafeMutateTail(cur.contents, v)
      cur.contents = v
      i.contents = i.contents + 1
    }

    headX
  }

let rec lengthAux = (x, acc) =>
  switch x {
  | list{} => acc
  | list{_, ...t} => lengthAux(t, acc + 1)
  }

let length = xs => lengthAux(xs, 0)
let size = length

let rec fillAux = (arr, i, x) =>
  switch x {
  | list{} => ()
  | list{h, ...t} =>
    A.setUnsafe(arr, i, h)
    fillAux(arr, i + 1, t)
  }

let rec fromArrayAux = (a, i, res) =>
  if i < 0 {
    res
  } else {
    fromArrayAux(a, i - 1, list{A.getUnsafe(a, i), ...res})
  }

let fromArray = a => fromArrayAux(a, A.length(a) - 1, list{})

let toArray = (x: t<_>) => {
  let len = length(x)
  let arr = A.makeUninitializedUnsafe(len)
  fillAux(arr, 0, x)
  arr
}

let shuffle = xs => {
  let v = toArray(xs)
  A.shuffleInPlace(v)
  fromArray(v)
}

/* let rec fillAuxMap arr i x f =
  match x with
  | [] -> ()
  | h::t ->
    A.setUnsafe arr i (f h [@bs]) ;
    fillAuxMap arr (i + 1) t f */

/* module J = Js_json */
/* type json = J.t */
/* let toJson x f = */
/* let len = length x in */
/* let arr = Belt_Array.makeUninitializedUnsafe len in */
/* fillAuxMap arr 0 x f; */
/* J.array arr */

/* TODO: best practice about raising excpetion
   1. raise OCaml exception, no stacktrace
   2. raise JS exception, how to pattern match
*/

let rec reverseConcat = (l1, l2) =>
  switch l1 {
  | list{} => l2
  | list{a, ...l} => reverseConcat(l, list{a, ...l2})
  }

let reverse = l => reverseConcat(l, list{})

let rec flattenAux = (prec, xs) =>
  switch xs {
  | list{} => unsafeMutateTail(prec, list{})
  | list{h, ...r} => flattenAux(copyAuxCont(h, prec), r)
  }

let rec flatten = xs =>
  switch xs {
  | list{} => list{}
  | list{list{}, ...xs} => flatten(xs)
  | list{list{h, ...t}, ...r} =>
    let cell = mutableCell(h, list{})
    flattenAux(copyAuxCont(t, cell), r)
    cell
  }

let concatMany = xs =>
  switch xs {
  | [] => list{}
  | [x] => x
  | _ =>
    let len = A.length(xs)
    let v = ref(A.getUnsafe(xs, len - 1))
    for i in len - 2 downto 0 {
      v.contents = concat(A.getUnsafe(xs, i), v.contents)
    }
    v.contents
  }

let rec mapRevAux = (f, accu, xs) =>
  switch xs {
  | list{} => accu
  | list{a, ...l} => mapRevAux(f, list{f(. a), ...accu}, l)
  }

let mapReverseU = (l, f) => mapRevAux(f, list{}, l)

let mapReverse = (l, f) => mapReverseU(l, (. x) => f(x))

let rec forEachU = (xs, f) =>
  switch xs {
  | list{} => ()
  | list{a, ...l} =>
    f(. a)->ignore
    forEachU(l, f)
  }

let forEach = (xs, f) => forEachU(xs, (. x) => f(x))

let rec iteri = (xs, i, f) =>
  switch xs {
  | list{} => ()
  | list{a, ...l} =>
    f(. i, a)->ignore
    iteri(l, i + 1, f)
  }

let forEachWithIndexU = (l, f) => iteri(l, 0, f)
let forEachWithIndex = (l, f) => forEachWithIndexU(l, (. i, x) => f(i, x))

let rec reduceU = (l, accu, f) =>
  switch l {
  | list{} => accu
  | list{a, ...l} => reduceU(l, f(. accu, a), f)
  }

let reduce = (l, accu, f) => reduceU(l, accu, (. acc, x) => f(acc, x))

let rec reduceReverseUnsafeU = (l, accu, f) =>
  switch l {
  | list{} => accu
  | list{a, ...l} => f(. reduceReverseUnsafeU(l, accu, f), a)
  }

let reduceReverseU = (type a b, l: list<a>, acc: b, f) => {
  let len = length(l)
  if len < 1000 {
    reduceReverseUnsafeU(l, acc, f)
  } else {
    A.reduceReverseU(toArray(l), acc, f)
  }
}

let reduceReverse = (l, accu, f) => reduceReverseU(l, accu, (. a, b) => f(a, b))

let rec reduceWithIndexAuxU = (l, acc, f, i) =>
  switch l {
  | list{} => acc
  | list{x, ...xs} => reduceWithIndexAuxU(xs, f(. acc, x, i), f, i + 1)
  }

let reduceWithIndexU = (l, acc, f) => reduceWithIndexAuxU(l, acc, f, 0)

let reduceWithIndex = (l, acc, f) => reduceWithIndexU(l, acc, (. acc, x, i) => f(acc, x, i))

let rec mapRevAux2 = (l1, l2, accu, f) =>
  switch (l1, l2) {
  | (list{a1, ...l1}, list{a2, ...l2}) => mapRevAux2(l1, l2, list{f(. a1, a2), ...accu}, f)
  | (_, list{}) | (list{}, _) => accu
  }

let mapReverse2U = (l1, l2, f) => mapRevAux2(l1, l2, list{}, f)

let mapReverse2 = (l1, l2, f) => mapReverse2U(l1, l2, (. a, b) => f(a, b))

let rec forEach2U = (l1, l2, f) =>
  switch (l1, l2) {
  | (list{a1, ...l1}, list{a2, ...l2}) =>
    f(. a1, a2)->ignore
    forEach2U(l1, l2, f)
  | (list{}, _) | (_, list{}) => ()
  }

let forEach2 = (l1, l2, f) => forEach2U(l1, l2, (. a, b) => f(a, b))

let rec reduce2U = (l1, l2, accu, f) =>
  switch (l1, l2) {
  | (list{a1, ...l1}, list{a2, ...l2}) => reduce2U(l1, l2, f(. accu, a1, a2), f)
  | (list{}, _) | (_, list{}) => accu
  }

let reduce2 = (l1, l2, acc, f) => reduce2U(l1, l2, acc, (. a, b, c) => f(a, b, c))

let rec reduceReverse2UnsafeU = (l1, l2, accu, f) =>
  switch (l1, l2) {
  | (list{}, list{}) => accu
  | (list{a1, ...l1}, list{a2, ...l2}) => f(. reduceReverse2UnsafeU(l1, l2, accu, f), a1, a2)
  | (_, list{}) | (list{}, _) => accu
  }

let reduceReverse2U = (type a b c, l1: list<a>, l2: list<b>, acc: c, f) => {
  let len = length(l1)
  if len < 1000 {
    reduceReverse2UnsafeU(l1, l2, acc, f)
  } else {
    A.reduceReverse2U(toArray(l1), toArray(l2), acc, f)
  }
}

let reduceReverse2 = (l1, l2, acc, f) => reduceReverse2U(l1, l2, acc, (. a, b, c) => f(a, b, c))

let rec everyU = (xs, p) =>
  switch xs {
  | list{} => true
  | list{a, ...l} => p(. a) && everyU(l, p)
  }

let every = (xs, p) => everyU(xs, (. x) => p(x))

let rec someU = (xs, p) =>
  switch xs {
  | list{} => false
  | list{a, ...l} => p(. a) || someU(l, p)
  }

let some = (xs, p) => someU(xs, (. x) => p(x))

let rec every2U = (l1, l2, p) =>
  switch (l1, l2) {
  | (_, list{}) | (list{}, _) => true
  | (list{a1, ...l1}, list{a2, ...l2}) => p(. a1, a2) && every2U(l1, l2, p)
  }

let every2 = (l1, l2, p) => every2U(l1, l2, (. a, b) => p(a, b))

let rec cmpByLength = (l1, l2) =>
  switch (l1, l2) {
  | (list{}, list{}) => 0
  | (_, list{}) => 1
  | (list{}, _) => -1
  | (list{_, ...l1s}, list{_, ...l2s}) => cmpByLength(l1s, l2s)
  }

let rec cmpU = (l1, l2, p) =>
  switch (l1, l2) {
  | (list{}, list{}) => 0
  | (_, list{}) => 1
  | (list{}, _) => -1
  | (list{a1, ...l1}, list{a2, ...l2}) =>
    let c = p(. a1, a2)
    if c == 0 {
      cmpU(l1, l2, p)
    } else {
      c
    }
  }

let cmp = (l1, l2, f) => cmpU(l1, l2, (. x, y) => f(x, y))

let rec eqU = (l1, l2, p) =>
  switch (l1, l2) {
  | (list{}, list{}) => true
  | (_, list{})
  | (list{}, _) => false
  | (list{a1, ...l1}, list{a2, ...l2}) =>
    if p(. a1, a2) {
      eqU(l1, l2, p)
    } else {
      false
    }
  }
let eq = (l1, l2, f) => eqU(l1, l2, (. x, y) => f(x, y))

let rec some2U = (l1, l2, p) =>
  switch (l1, l2) {
  | (list{}, _) | (_, list{}) => false
  | (list{a1, ...l1}, list{a2, ...l2}) => p(. a1, a2) || some2U(l1, l2, p)
  }

let some2 = (l1, l2, p) => some2U(l1, l2, (. a, b) => p(a, b))

let rec hasU = (xs, x, eq) =>
  switch xs {
  | list{} => false
  | list{a, ...l} => eq(. a, x) || hasU(l, x, eq)
  }

let has = (xs, x, eq) => hasU(xs, x, (. a, b) => eq(a, b))

let rec getAssocU = (xs, x, eq) =>
  switch xs {
  | list{} => None
  | list{(a, b), ...l} =>
    if eq(. a, x) {
      Some(b)
    } else {
      getAssocU(l, x, eq)
    }
  }

let getAssoc = (xs, x, eq) => getAssocU(xs, x, (. a, b) => eq(a, b))

let rec hasAssocU = (xs, x, eq) =>
  switch xs {
  | list{} => false
  | list{(a, _), ...l} => eq(. a, x) || hasAssocU(l, x, eq)
  }

let hasAssoc = (xs, x, eq) => hasAssocU(xs, x, (. a, b) => eq(a, b))

let removeAssocU = (xs, x, eq) =>
  switch xs {
  | list{} => list{}
  | list{(a, _) as pair, ...l} =>
    if eq(. a, x) {
      l
    } else {
      let cell = mutableCell(pair, list{})
      let removed = removeAssocAuxWithMap(l, x, cell, eq)
      if removed {
        cell
      } else {
        xs
      }
    }
  }

let removeAssoc = (xs, x, eq) => removeAssocU(xs, x, (. a, b) => eq(a, b))

let setAssocU = (xs, x, k, eq) =>
  switch xs {
  | list{} => list{(x, k)}
  | list{(a, _) as pair, ...l} =>
    if eq(. a, x) {
      list{(x, k), ...l}
    } else {
      let cell = mutableCell(pair, list{})
      let replaced = setAssocAuxWithMap(l, x, k, cell, eq)
      if replaced {
        cell
      } else {
        list{(x, k), ...xs}
      }
    }
  }

let setAssoc = (xs, x, k, eq) => setAssocU(xs, x, k, (. a, b) => eq(a, b))

let sortU = (xs, cmp) => {
  let arr = toArray(xs)
  Belt_SortArray.stableSortInPlaceByU(arr, cmp)
  fromArray(arr)
}

let sort = (xs, cmp) => sortU(xs, (. x, y) => cmp(x, y))

let rec getByU = (xs, p) =>
  switch xs {
  | list{} => None
  | list{x, ...l} =>
    if p(. x) {
      Some(x)
    } else {
      getByU(l, p)
    }
  }

let getBy = (xs, p) => getByU(xs, (. a) => p(a))

let rec keepU = (xs, p) =>
  switch xs {
  | list{} => list{}
  | list{h, ...t} =>
    if p(. h) {
      let cell = mutableCell(h, list{})
      copyAuxWitFilter(p, t, cell)
      cell
    } else {
      keepU(t, p)
    }
  }

let keep = (xs, p) => keepU(xs, (. x) => p(x))

let filter = keep

let keepWithIndexU = (xs, p) => {
  let rec auxKeepWithIndex = (xs, p, i) =>
    switch xs {
    | list{} => list{}
    | list{h, ...t} =>
      if p(. h, i) {
        let cell = mutableCell(h, list{})
        copyAuxWithFilterIndex(p, t, cell, i + 1)
        cell
      } else {
        auxKeepWithIndex(t, p, i + 1)
      }
    }
  auxKeepWithIndex(xs, p, 0)
}

let keepWithIndex = (xs, p) => keepWithIndexU(xs, (. x, i) => p(x, i))

let filterWithIndex = keepWithIndex

let rec keepMapU = (xs, p) =>
  switch xs {
  | list{} => list{}
  | list{h, ...t} =>
    switch p(. h) {
    | Some(h) =>
      let cell = mutableCell(h, list{})
      copyAuxWitFilterMap(p, t, cell)
      cell
    | None => keepMapU(t, p)
    }
  }

let keepMap = (xs, p) => keepMapU(xs, (. x) => p(x))

let partitionU = (l, p) =>
  switch l {
  | list{} => (list{}, list{})
  | list{h, ...t} =>
    let nextX = mutableCell(h, list{})
    let nextY = mutableCell(h, list{})
    let b = p(. h)
    partitionAux(p, t, nextX, nextY)
    if b {
      (
        nextX,
        switch nextY {
        | list{_, ...tail} => tail
        | list{} => assert(false)
        },
      )
    } else {
      (
        switch nextX {
        | list{_, ...tail} => tail
        | list{} => assert(false)
        },
        nextY,
      )
    }
  }

let partition = (l, p) => partitionU(l, (. x) => p(x))

let unzip = xs =>
  switch xs {
  | list{} => (list{}, list{})
  | list{(x, y), ...l} =>
    let cellX = mutableCell(x, list{})
    let cellY = mutableCell(y, list{})
    splitAux(l, cellX, cellY)
    (cellX, cellY)
  }

let zip = (l1, l2) =>
  switch (l1, l2) {
  | (_, list{}) | (list{}, _) => list{}
  | (list{a1, ...l1}, list{a2, ...l2}) =>
    let cell = mutableCell((a1, a2), list{})
    zipAux(l1, l2, cell)
    cell
  }
