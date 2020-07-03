
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


(* This internal module 
  contains methods which does not rely on ordering. 
  Such methods could be shared between 
  [generic set/specalized set] whether mutable or immutable depends on use cases
*)
type 'value t = 'value node Js.null
and 'value node  = private {
  value : 'value; 
  height : int;
  mutable left : 'value t;
  mutable right : 'value t;
} [@@bs.deriving abstract]

type ('a, 'b) cmp = ('a, 'b) Belt_Id.cmp
(* TODO: node is used in [subset] *)
external toOpt : 'a Js.null -> 'a option = "#null_to_opt"
external return : 'a -> 'a Js.null = "%identity"
external empty : 'a Js.null = "#null"



val copy : 'a t -> 'a t
val create : 'a t -> 'a -> 'a t -> 'a t
val bal : 'a t -> 'a -> 'a t -> 'a t
val singleton : 'a -> 'a t


val minimum : 'a t -> 'a option
val minUndefined : 'a t -> 'a Js.undefined
val maximum : 'a t -> 'a option
val maxUndefined : 'a t -> 'a Js.undefined

val removeMinAuxWithRef : 'a node -> 'a ref -> 'a t
(* [removeMinAuxWithRef n cell] return a new node with
   minimum removed and stored in cell *)
val empty : 'a t
val isEmpty : 'a t -> bool

val stackAllLeft : 'a t -> 'a node list -> 'a node list

val forEachU: 'a t -> ('a -> unit [@bs]) -> unit
val forEach: 'a t -> ('a -> unit) -> unit  

val reduceU: 'a t -> 'b -> ('b -> 'a -> 'b [@bs]) -> 'b
val reduce: 'a t -> 'b -> ('b -> 'a -> 'b ) -> 'b

val everyU: 'a t -> ('a -> bool [@bs]) -> bool
val every: 'a t -> ('a -> bool ) -> bool  

val someU: 'a t -> ('a -> bool [@bs]) -> bool
val some: 'a t -> ('a -> bool ) -> bool  

val joinShared : 'a t -> 'a -> 'a t -> 'a t
val concatShared : 'a t -> 'a t -> 'a t

val keepSharedU: 'a t -> ('a -> bool [@bs]) -> 'a t
val keepShared: 'a t -> ('a -> bool ) -> 'a t    

val keepCopyU: 'a t -> ('a -> bool  [@bs]) -> 'a t
val keepCopy: 'a t -> ('a -> bool ) -> 'a t    

val partitionSharedU: 'a t -> ('a -> bool [@bs]) -> 'a t * 'a t
val partitionShared: 'a t -> ('a -> bool) -> 'a t * 'a t

val partitionCopyU: 'a t -> ('a -> bool [@bs]) -> 'a t * 'a t
val partitionCopy: 'a t -> ('a -> bool ) -> 'a t * 'a t

val lengthNode: 'a node -> int   
val size: 'a t -> int

val toList: 'a t -> 'a list
val checkInvariantInternal: _ t -> unit
(**
   {b raise} when invariant is not held
*)  
val fillArray: 'a node -> int -> 'a array -> int 
val toArray: 'a t -> 'a array
val fromSortedArrayAux : 'a array -> int -> int -> 'a t
val fromSortedArrayRevAux : 'a array -> int -> int -> 'a t
val fromSortedArrayUnsafe : 'a array -> 'a t
val has:  'a t ->  'a -> cmp:('a, 'b) cmp -> bool
val cmp: 'a t -> 'a t -> cmp:('a, 'b) cmp -> int
val eq:  'a t -> 'a t -> cmp:('a, 'b) cmp -> bool
val subset :  'a t -> 'a t -> cmp:('a, 'b) cmp -> bool
val get :  'a t -> 'a  -> cmp:('a, 'b) cmp -> 'a option
val getUndefined: 'a t -> 'a -> cmp:('a, 'b) cmp -> 'a Js.undefined
val getExn: 'a t -> 'a ->  cmp:('a, 'b) cmp -> 'a 


val fromArray: 'a array ->  cmp:('a, 'b) cmp -> 'a t


val addMutate : cmp:('a, 'b) cmp -> 'a t -> 'a -> 'a t
val balMutate : 'a node -> 'a node
val removeMinAuxWithRootMutate : 'a node -> 'a node -> 'a t
(* [rmeoveMinAuxMutateWithRoot root n]
   remove the minimum of n in place and store its value in the [key root]
 *)
