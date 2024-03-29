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

@new external new_uninitialized: int => bytes = "Array"

external unsafe_get: (bytes, int) => char = "%bytes_unsafe_get"
external unsafe_set: (bytes, int, char) => unit = "%bytes_unsafe_set"
external length: bytes => int = "%bytes_length"

let set = (s, i, ch) =>
  if i < 0 || i >= length(s) {
    raise(Invalid_argument("index out of bounds"))
  } else {
    s->unsafe_set(i, ch)
  }

let get = (s, i) =>
  if i < 0 || i >= length(s) {
    raise(Invalid_argument("index out of bounds"))
  } else {
    s->unsafe_get(i)
  }

let create = (len): bytes =>
  /* Node raise [RangeError] exception */
  if len < 0 {
    raise(Invalid_argument("String.create"))
  } else {
    let result = new_uninitialized(len)
    for i in 0 to len - 1 {
      result->unsafe_set(i, ' ')
    }
    result
  }

let rec bytes_compare_aux = (s1: bytes, s2: bytes, off, len, def) =>
  if off < len {
    let (a, b) = (s1->unsafe_get(off), s2->unsafe_get(off))
    if a > b {
      1
    } else if a < b {
      -1
    } else {
      bytes_compare_aux(s1, s2, off + 1, len, def)
    }
  } else {
    def
  }

/* code path could be using a tuple if we can eliminate the tuple allocation for code below
   {[
     let (len, v) = 
       if len1 = len2 then (..,...)
       else (.., .)
   ]}

*/
let bytes_compare = (s1: bytes, s2: bytes): int => {
  let (len1, len2) = (length(s1), length(s2))
  if len1 == len2 {
    bytes_compare_aux(s1, s2, 0, len1, 0)
  } else if len1 < len2 {
    bytes_compare_aux(s1, s2, 0, len1, -1)
  } else {
    bytes_compare_aux(s1, s2, 0, len2, 1)
  }
}

let rec bytes_equal_aux = (s1: bytes, s2, off: int, len) =>
  if off == len {
    true
  } else {
    let (a, b) = (s1->unsafe_get(off), s2->unsafe_get(off))
    a == b && bytes_equal_aux(s1, s2, off + 1, len)
  }

let bytes_equal = (s1: bytes, s2: bytes): bool => {
  let (len1, len2) = (length(s1), length(s2))
  len1 == len2 && bytes_equal_aux(s1, s2, 0, len1)
}

let bytes_greaterthan = (s1: bytes, s2) => bytes_compare(s1, s2) > 0

let bytes_greaterequal = (s1: bytes, s2) => bytes_compare(s1, s2) >= 0

let bytes_lessthan = (s1: bytes, s2) => bytes_compare(s1, s2) < 0

let bytes_lessequal = (s1: bytes, s2) => bytes_compare(s1, s2) <= 0
