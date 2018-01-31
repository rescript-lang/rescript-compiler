(* Copyright (C) 2018 Authors of BuckleScript
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


type ('a, 'id) t 

(** The type of hash tables from type ['a] to type ['b]. *)

type ('a, 'id) dict = ('a, 'id) Bs_Dict.hashable

val make:  int -> dict:('a,'id) dict ->  ('a, 'id) t
val clear: ('a, 'id) t -> unit
val isEmpty: _ t -> bool

val add: ('a, 'id) t -> 'a -> unit

val copy: ('a, 'id) t -> ('a, 'id) t

val has: ('a, 'id) t -> 'a -> bool

val remove: ('a, 'id) t -> 'a -> unit

val forEach: ('a, 'id) t -> ('a  -> unit [@bs]) ->  unit
(** Order unspecified. *)

val reduce: ('a, 'id) t -> 'c -> ('c -> 'a  ->  'c [@bs]) -> 'c
(** Order unspecified. *)

val size: ('a, 'id) t -> int  

val logStats: _ t -> unit

val toArray: ('a,'id) t -> 'a array 

val ofArray: 'a array -> dict:('a,'id) dict -> ('a,'id) t 

val mergeMany: ('a,'id) t -> 'a array -> unit

val getBucketHistogram: _ t -> int array

module Int = Bs_HashSetInt
module String = Bs_HashSetString
