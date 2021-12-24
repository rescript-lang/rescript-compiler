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

(** First in last out stack.

    This module implements stacks, with in-place modification.
*)

type 'a t


val make: unit -> 'a t
(** @return a new stack, initially empty. *)

val clear: 'a t -> unit
(** Discard all elements from the stack. *)
val copy : 'a t -> 'a t
(** [copy x] O(1) operation, return a new stack  *)

val push : 'a t -> 'a -> unit

val popUndefined : 'a t -> 'a Js.undefined

val pop : 'a t -> 'a option 

val topUndefined : 'a t -> 'a Js.undefined

val top : 'a t -> 'a option 

val isEmpty : 'a t -> bool

val size : 'a t -> int

val forEachU : 'a t -> ('a -> unit [@bs] ) -> unit
val forEach : 'a t -> ('a -> unit ) -> unit


val dynamicPopIterU : 'a t -> ('a ->  unit [@bs]) -> unit
val dynamicPopIter : 'a t -> ('a ->  unit ) -> unit   
(** [dynamicPopIter s f ]
    apply [f] to each element of [s]. The item is poped 
    before applying [f], [s] will be empty  after this opeartion.
    This function is useful for worklist algorithm
*)

