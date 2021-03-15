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

module C = Belt_internalBucketsType

type ('a,'b) bucket = {
  mutable key : 'a;
  mutable value : 'b;
  mutable next : ('a,'b) bucket C.opt
}  
and ('hash, 'eq, 'a, 'b) t = ('hash, 'eq, ('a,'b) bucket) C.container  


val copy : ('hash, 'eq, 'a, 'b) t -> ('hash, 'eq, 'a, 'b) t

val forEachU: (_, _, 'a, 'b) t -> ('a -> 'b -> 'c [@bs]) -> unit
val forEach: (_, _, 'a, 'b) t -> ('a -> 'b -> 'c) -> unit

val reduceU: (_, _, 'a, 'b) t -> 'c -> ('c -> 'a -> 'b -> 'c [@bs]) -> 'c
val reduce: (_, _, 'a, 'b) t -> 'c -> ('c -> 'a -> 'b -> 'c) -> 'c  

val logStats : _ t -> unit


val keepMapInPlaceU: (_, _, 'a, 'b) t -> ('a -> 'b -> 'b option [@bs]) -> unit
val keepMapInPlace: (_, _, 'a, 'b) t -> ('a -> 'b -> 'b option) -> unit

val fillArray : int -> ('a * 'b) array -> ('a, 'b) bucket -> int

val keysToArray : (_, _, 'a, _) t -> 'a array

val valuesToArray : (_, _, _, 'b) t -> 'b array

val toArray : (_, _, 'a, 'b) t -> ('a * 'b) array

val getBucketHistogram : _ t -> int array
