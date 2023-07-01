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

@send external dup: (array<'a>, @as(0) _) => array<'a> = "slice"

%%private(let {unsafe_get, unsafe_set} = module(Caml_array_extern))

let sub = (x: array<'a>, offset: int, len: int) => {
  let result = Caml_array_extern.new_uninitialized(len)
  let j = {contents: 0} and i = {contents: offset}
  while j.contents < len {
    result->unsafe_set(j.contents, x->unsafe_get(i.contents))
    j.contents = j.contents + 1
    i.contents = i.contents + 1
  }
  result
}

let rec len = (acc, l) =>
  switch l {
  | list{} => acc
  | list{x, ...xs} => len(Caml_array_extern.length(x) + acc, xs)
  }

let rec fill = (arr, i, l) =>
  switch l {
  | list{} => ()
  | list{x, ...xs} =>
    let l = Caml_array_extern.length(x)
    let k = {contents: i}
    let j = {contents: 0}
    while j.contents < l {
      arr->unsafe_set(k.contents, x->unsafe_get(j.contents))
      k.contents = k.contents + 1
      j.contents = j.contents + 1
    }
    fill(arr, k.contents, xs)
  }

let concat = (l: list<array<'a>>): array<'a> => {
  let v = len(0, l)
  let result = Caml_array_extern.new_uninitialized(v)
  fill(result, 0, l)
  result
}

let set = (xs, index, newval) =>
  if index < 0 || index >= Caml_array_extern.length(xs) {
    raise(Invalid_argument("index out of bounds"))
  } else {
    xs->unsafe_set(index, newval)
  }

let get = (xs, index) =>
  if index < 0 || index >= Caml_array_extern.length(xs) {
    raise(Invalid_argument("index out of bounds"))
  } else {
    xs->unsafe_get(index)
  }

let make = (len, init) => {
  let b = Caml_array_extern.new_uninitialized(len)
  for i in 0 to len - 1 {
    b->unsafe_set(i, init)
  }
  b
}

let make_float = len => {
  let b = Caml_array_extern.new_uninitialized(len)
  for i in 0 to len - 1 {
    b->unsafe_set(i, 0.)
  }
  b
}

let blit = (a1, i1, a2, i2, len) =>
  if i2 <= i1 {
    for j in 0 to len - 1 {
      a2->unsafe_set(j + i2, a1->unsafe_get(j + i1))
    }
  } else {
    for j in len - 1 downto 0 {
      a2->unsafe_set(j + i2, a1->unsafe_get(j + i1))
    }
  }
