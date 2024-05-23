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

type t<'a> = array<'a>

external length: array<'a> => int = "%array_length"
external get: (array<'a>, int) => 'a = "%array_safe_get"
external set: (array<'a>, int, 'a) => unit = "%array_safe_set"
external make: (int, 'a) => array<'a> = "?make_vect"
external unsafe_get: (t<'a>, int) => 'a = "%array_unsafe_get"
external unsafe_set: (t<'a>, int, 'a) => unit = "%array_unsafe_set"

/** 
**param** a array

**param** p predicate
*/
let filterInPlace = (p, a) => {
  let i = ref(0)
  let j = ref(0)
  while i.contents < Js_array2.length(a) {
    let v = Js_array2.unsafe_get(a, i.contents)
    if p(. v) {
      Js_array2.unsafe_set(a, j.contents, v)
      j.contents = j.contents + 1
    }
    i.contents = i.contents + 1
  }
  Js_array2.removeFromInPlace(a, ~pos=j.contents)->ignore
}

let empty = a => Js_array2.removeFromInPlace(a, ~pos=0)->ignore

let pushBack = (x, xs) => Js_array2.push(xs, x)->ignore

/** Find by JS (===)  equality */
let memByRef = (x, xs) => Js_array2.indexOf(xs, x) >= 0

let iter = (f, xs) =>
  for i in 0 to Js_array2.length(xs) - 1 {
    f(. Js_array2.unsafe_get(xs, i))
  }

let iteri = (f, a) =>
  for i in 0 to length(a) - 1 {
    f(. i, unsafe_get(a, i))
  }

@new external createUnsafe: int => t<'a> = "Array"

/* let ofList xs = */
/* match xs with */
/* | [] -> [||] */
/* | l -> */
/* let a = createUnsafe (Js_list.length l) in */
/* let rec fill i = function */
/* | [] -> a */
/* | hd::tl -> Array.unsafe_set a i hd; fill (i+1) tl in */
/* fill 0 l */

let toList = a => {
  let rec tolist = (i, res) =>
    if i < 0 {
      res
    } else {
      tolist(i - 1, list{unsafe_get(a, i), ...res})
    }
  tolist(length(a) - 1, list{})
}

let init = (n, f) => {
  let v = createUnsafe(n)
  for i in 0 to n - 1 {
    unsafe_set(v, i, f(. i))
  }
  v
}

let copy = x => {
  let len = length(x)
  let b = createUnsafe(len)
  for i in 0 to len - 1 {
    unsafe_set(b, i, unsafe_get(x, i))
  }
  b
}

let map = (f, a) => {
  let l = Js_array2.length(a)
  let r = createUnsafe(l)
  for i in 0 to l - 1 {
    unsafe_set(r, i, f(. unsafe_get(a, i)))
  }
  r
}

let foldLeft = (f, x, a) => {
  let r = ref(x)
  for i in 0 to length(a) - 1 {
    r.contents = f(. r.contents, unsafe_get(a, i))
  }
  r.contents
}

let foldRight = (f, a, x) => {
  let r = ref(x)
  for i in length(a) - 1 downto 0 {
    r.contents = f(. unsafe_get(a, i), r.contents)
  }
  r.contents
}

let mapi = (f, a) => {
  let l = length(a)
  if l == 0 {
    []
  } else {
    let r = createUnsafe(l)
    for i in 0 to l - 1 {
      unsafe_set(r, i, f(. i, unsafe_get(a, i)))
    }
    r
  }
}

let append = (x, a) => Js_array2.concat(a, [x])

/* TODO: add `append` */
