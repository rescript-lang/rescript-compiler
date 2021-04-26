
(* Copyright (C) 2017 Authors of ReScript
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


(** This is a specialized module for [`Belt_SortArray`](), the docs in that module also
    applies here, except the comparator is fixed and inlined
*)

#ifdef TYPE_INT
type element = int
#elif defined TYPE_STRING
type element = string
#else
[%error "unknown type"]
#endif

val strictlySortedLength: element array -> int
(**
  The same as [`Belt_SortArray.strictlySortedLength`]() except the comparator is fixed

  **return** `+n` means increasing order  `-n` means negative order
*)


val isSorted: element array  -> bool
(** `sorted xs` return true if `xs` is in non strict increasing order *)

val stableSortInPlace: element array -> unit
(**
  The same as [`Belt_SortArray.stableSortInPlaceBy`]() except the comparator is fixed
*)

val stableSort: element array -> element array
(** The same as [`Belt_SortArray.stableSortBy`]() except the comparator is fixed *)

val binarySearch: element array -> element -> int
(**
  If value is not found and value is less than one or more elements in array,
  the negative number returned is the bitwise complement of the index of the first element
  that is larger than value.

  If value is not found and value is greater than all elements in array,
  the negative number returned is the bitwise complement of
  (the index of the last element plus 1)

  for example, if `key` is smaller than all elements return `-1` since `lnot (-1) = 0`
  if `key` is larger than all elements return `- (len + 1)` since `lnot (-(len+1)) = len`
*)

(**/**)
val union:
  element array -> int -> int ->
  element array -> int -> int ->
  element array -> int
  -> int
(**
  `union src src1ofs src1len src2 src2ofs src2len dst dstofs cmp`
  assume `src` and `src2` is strictly sorted.
  for equivalent elements, it is picked from `src`
  also assume that `dst` is large enough to store all elements
*)

val intersect:
  element array -> int -> int ->
  element array -> int -> int ->
  element array -> int
  -> int

val diff:
  element array -> int -> int ->
  element array -> int -> int ->
  element array -> int
  -> int
(**/**)
