/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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

@@warning("-3")

type t<'a> = list<'a>

let rec lengthAux = (len, x) =>
  switch x {
  | list{} => len
  | list{_, ...l} => lengthAux(len + 1, l)
  }

let length = l => lengthAux(0, l)

let cons = (x, xs) => list{x, ...xs}

let isEmpty = x => x == list{}

let hd = x =>
  switch x {
  | list{} => None
  | list{a, ..._} => Some(a)
  }

let tl = x =>
  switch x {
  | list{} => None
  | list{_, ...l} => Some(l)
  }

let nth = (l, n) =>
  if n < 0 {
    None
  } else {
    let rec nth_aux = (l, n) =>
      switch l {
      | list{} => None
      | list{a, ...l} =>
        if n == 0 {
          Some(a)
        } else {
          nth_aux(l, n - 1)
        }
      }
    nth_aux(l, n)
  }

let rec revAppend = (l1, l2) =>
  switch l1 {
  | list{} => l2
  | list{a, ...l} => revAppend(l, list{a, ...l2})
  }

let rev = l => revAppend(l, list{})

let rec mapRevAux = (f, acc, ls) =>
  switch ls {
  | list{} => acc
  | list{a, ...l} => mapRevAux(f, list{f(. a), ...acc}, l)
  }

let mapRev = (f, ls) => mapRevAux(f, list{}, ls)

let map = (f, ls) => rev(mapRevAux(f, list{}, ls))

let rec iter = (f, x) =>
  switch x {
  | list{} => ()
  | list{a, ...l} =>
    f(. a)
    iter(f, l)
  }

let rec iteri = (i, f, x) =>
  switch x {
  | list{} => ()
  | list{a, ...l} =>
    f(. i, a)
    iteri(i + 1, f, l)
  }

let iteri = (f, l) => iteri(0, f, l)

let rec foldLeft = (f, accu, l) =>
  switch l {
  | list{} => accu
  | list{a, ...l} => foldLeft(f, f(. accu, a), l)
  }

let foldRightMaxStack = 1000

let rec tailLoop = (f, acc, x) =>
  switch x {
  | list{} => acc
  | list{h, ...t} => tailLoop(f, f(. h, acc), t)
  }

let foldRight = (f, l, init) => {
  let rec loop = (n, x) =>
    switch x {
    | list{} => init
    | list{h, ...t} =>
      if n < foldRightMaxStack {
        f(. h, loop(n + 1, t))
      } else {
        f(. h, tailLoop(f, init, rev(t)))
      }
    }

  loop(0, l)
}

let rec flattenAux = (acc, lx) =>
  switch lx {
  | list{} => rev(acc)
  | list{y, ...ys} => flattenAux(revAppend(y, acc), ys)
  }

let flatten = lx => flattenAux(list{}, lx)

let rec filterRevAux = (f, acc, xs) =>
  switch xs {
  | list{} => acc
  | list{y, ...ys} =>
    switch f(. y) {
    | false => filterRevAux(f, acc, ys)
    | true => filterRevAux(f, list{y, ...acc}, ys)
    }
  }

let filter = (f, xs) => rev(filterRevAux(f, list{}, xs))

let rec filterMapRevAux = (f: (. 'a) => option<'b>, acc, xs) =>
  switch xs {
  | list{} => acc
  | list{y, ...ys} =>
    switch f(. y) {
    | None => filterMapRevAux(f, acc, ys)
    | Some(z) => filterMapRevAux(f, list{z, ...acc}, ys)
    }
  }

let filterMap = (f, xs) => rev(filterMapRevAux(f, list{}, xs))

let rec countByAux = (f, acc, xs) =>
  switch xs {
  | list{} => acc
  | list{y, ...ys} =>
    countByAux(
      f,
      if f(. y) {
        acc + 1
      } else {
        acc
      },
      ys,
    )
  }

let countBy = (f, xs) => countByAux(f, 0, xs)

let init = (n, f) => Js_vector.toList(Js_vector.init(n, f))

@new external createUnsafe: int => array<'a> = "Array"

let toVector = xs =>
  switch xs {
  | list{} => []
  | l =>
    let a = createUnsafe(length(l))
    let rec fill = (i, x) =>
      switch x {
      | list{} => a
      | list{hd, ...tl} =>
        Js_array2.unsafe_set(a, i, hd)
        fill(i + 1, tl)
      }
    fill(0, l)
  }

let rec equal = (cmp, xs, ys) =>
  switch (xs, ys) {
  | (list{}, list{}) => true
  | (list{x, ...xs}, list{y, ...ys}) =>
    if cmp(. x, y) {
      equal(cmp, xs, ys)
    } else {
      false
    }
  | (_, _) => false
  }
