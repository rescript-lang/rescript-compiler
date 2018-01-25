(* Copyright (C) 2017 Authors of BuckleScript
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

module C = Bs_internalBucketsType
  
type ('a,'b) bucket = {
  mutable key : 'a;
  mutable value : 'b;
  mutable next : ('a,'b) bucket C.opt
}  
and ('a, 'b) t0 = ('a,'b) bucket C.container  
[@@bs.deriving abstract]

val copy : ('a, 'b) t0 -> ('a, 'b) t0


val forEach0 : ('a, 'b) t0 -> ('a -> 'b -> 'c [@bs]) -> unit

val reduce0 :
  ('a, 'b) t0 -> 'c -> ('c -> 'a -> 'b -> 'c [@bs]) -> 'c
val logStats0 : ('a, 'b) t0 -> unit

  
val filterMapInplace0 :
  ('a, 'b) t0 -> ('a -> 'b -> 'b option [@bs]) -> unit

val fillArray : int -> ('a * 'b) array -> ('a, 'b) bucket -> int

val keys0 : ('a, 'b) t0 -> 'a array

val values0 : ('a, 'b) t0 -> 'b array

val toArray0 : ('a, 'b) t0 -> ('a * 'b) array

val getBucketHistogram : ('a,'b) t0 -> int array
