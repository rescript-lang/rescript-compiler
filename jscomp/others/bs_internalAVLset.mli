
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
type 'elt t0 = 'elt node Js.null
and 'elt node  = private {
  mutable left : 'elt t0;
   key : 'elt ; 
  mutable right : 'elt t0;
  h : int
} [@@bs.deriving abstract]
(* TODO: node is used in [subset] *)
external toOpt : 'a Js.null -> 'a option = "#null_to_opt"
external return : 'a -> 'a Js.null = "%identity"
external empty : 'a Js.null = "#null"



val copy : 'a t0 -> 'a t0
val create : 'a t0 -> 'a -> 'a t0 -> 'a t0
val bal : 'a t0 -> 'a -> 'a t0 -> 'a t0
val singleton0 : 'a -> 'a t0


val minOpt0 : 'a t0 -> 'a option
val minNull0 : 'a t0 -> 'a Js.null
val maxOpt0 : 'a t0 -> 'a option
val maxNull0 : 'a t0 -> 'a Js.null

val removeMinAuxWithRef : 'a node -> 'a ref -> 'a t0
(* [removeMinAuxWithRef n cell] return a new node with
   minimum removed and stored in cell *)
val empty0 : 'a t0
val isEmpty0 : 'a t0 -> bool

val stackAllLeft : 'a t0 -> 'a node list -> 'a node list

val iter0 : 'a t0 -> ('a -> 'b [@bs]) -> unit
val fold0 : 'a t0 -> 'b -> ('b -> 'a -> 'b [@bs]) -> 'b
val every0 : 'a t0 -> ('a -> bool [@bs]) -> bool
val some0 : 'a t0 -> ('a -> bool [@bs]) -> bool

val joinShared : 'a t0 -> 'a -> 'a t0 -> 'a t0
val concatShared : 'a t0 -> 'a t0 -> 'a t0
val filterShared0 : 'a t0 -> ('a -> bool [@bs]) -> 'a t0
val filterCopy : 'a t0 -> ('a -> bool  [@bs]) -> 'a t0

val partitionShared0 :
  'a t0 -> ('a -> bool [@bs]) -> 'a t0 * 'a t0
val partitionCopy: 
  'a t0 -> ('a -> bool [@bs]) -> 'a t0 * 'a t0

val lengthNode : 'a node -> int   
val length0 : 'a t0 -> int

val toList0 : 'a t0 -> 'a list
val checkInvariant : _ t0 -> bool
val fillArray: 'a node -> int -> 'a array -> int 
val toArray0 : 'a t0 -> 'a array
val ofSortedArrayAux : 'a array -> int -> int -> 'a t0
val ofSortedArrayRevAux : 'a array -> int -> int -> 'a t0
val ofSortedArrayUnsafe0 : 'a array -> 'a t0
val mem0 :  'a t0 ->  'a -> cmp:('a, 'b) Bs_Cmp.cmp -> bool
val cmp0 : 'a t0 -> 'a t0 -> cmp:('a, 'b) Bs_Cmp.cmp -> int
val eq0 :  'a t0 -> 'a t0 -> cmp:('a, 'b) Bs_Cmp.cmp -> bool
val subset0 :  'a t0 -> 'a t0 -> cmp:('a, 'b) Bs_Cmp.cmp -> bool
val findOpt0 :  'a t0 -> 'a  -> cmp:('a, 'b) Bs_Cmp.cmp -> 'a option
val findNull0 : 'a t0 -> 'a -> cmp:('a, 'b) Bs_Cmp.cmp -> 'a Js.null
val findExn0 : 'a t0 -> 'a ->  cmp:('a, 'b) Bs_Cmp.cmp -> 'a 


val ofArray0 : 'a array ->  cmp:('a, 'b) Bs_Cmp.cmp -> 'a t0


val addMutate : cmp:('a, 'b) Bs_Cmp.cmp -> 'a t0 -> 'a -> 'a t0
val balMutate : 'a node -> 'a node
val removeMinAuxWithRootMutate : 'a node -> 'a node -> 'a t0
(* [rmeoveMinAuxMutateWithRoot root n]
   remove the minimum of n in place and store its value in the [key root]
 *)
