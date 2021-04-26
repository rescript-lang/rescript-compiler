
# 2 "others/hashset.cppo.mli"
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

(**
  This module is [`Belt.HashSet`]() specialized with key type to be a primitive type.

  It is more efficient in general, the  API is the same with [`Belt.HashSet`]() except its key type is fixed,
  and identity is not needed(using the built-in one)

  **See** [`Belt.HashSet`]()
*)


# 37 "others/hashset.cppo.mli"
type key = string


# 45 "others/hashset.cppo.mli"
type t

val make: hintSize:int -> t

val clear: t -> unit

val isEmpty: t -> bool

val add:  t -> key -> unit

val copy: t -> t

val has: t -> key -> bool

val remove: t -> key -> unit

val forEachU: t -> (key  -> unit [@bs]) ->  unit
val forEach: t -> (key  -> unit) ->  unit

val reduceU: t -> 'c -> ( 'c -> key -> 'c [@bs]) ->   'c
val reduce: t -> 'c -> ( 'c -> key -> 'c) ->   'c

val size: t -> int

val logStats: t -> unit

val toArray: t -> key array

val fromArray: key array -> t

val mergeMany: t -> key array -> unit

val getBucketHistogram: t -> int array
