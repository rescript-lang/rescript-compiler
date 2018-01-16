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



type ('key, 'a) t0 = ('key, 'a) node Js.null

and ('k,  'v) node  = private {
  mutable left : ('k,'v) t0;
  mutable key : 'k; 
  mutable value : 'v; 
  mutable right : ('k,'v) t0;
  h : int 
} [@@bs.deriving abstract]

external toOpt : 'a Js.null -> 'a option = "#null_to_opt"
external return : 'a -> 'a Js.null = "%identity"
external empty : 'a Js.null = "#null"

val copy : ('k, 'v) t0 -> ('k, 'v) t0
val create :
  ('a,'b) t0 -> 'a -> 'b -> ('a,'b) t0 -> ('a,'b) t0
val bal :
  ('a,'b) t0 -> 'a -> 'b -> ('a,'b) t0 -> ('a,'b) t0
val singleton0 : 'a -> 'b -> ('a,'b) t0

val updateKV : ('k, 'v) node -> 'k -> 'v -> ('k,'v) t0

val minKeyOpt0 : ('a, 'b) t0 -> 'a option 
val minKeyNull0: ('a, 'b) t0 -> 'a Js.null

val maxKeyOpt0 : ('a, 'b) t0 -> 'a option 
val maxKeyNull0 : ('a, 'b) t0 -> 'a Js.null 

val minKVOpt0 : ('a, 'b) t0 -> ('a * 'b) option
val minKVNull0 : ('a,'b) t0 -> ('a * 'b) Js.null

val maxKVOpt0 : ('a,'b) t0 -> ('a * 'b) option
val maxKVNull0 : ('a,'b) t0 -> ('a * 'b) Js.null

val removeMinAuxWithRef : ('a, 'b) node -> 'a ref -> 'b ref -> ('a,'b) t0

val empty0 : _ t0
val isEmpty0 : _ t0 -> bool

val stackAllLeft :
  ('a,'b) t0 -> ('a, 'b) node list -> ('a, 'b) node list

val iter0 :  ('a,'b) t0 -> ('a -> 'b -> unit [@bs]) -> unit
val map0 :  ('c, 'a) t0 -> ('a -> 'b [@bs]) -> ('c, 'b) t0
val mapi0 :
   ('a,'b) t0 -> ('a -> 'b -> 'c [@bs]) -> ('a, 'c) t0
val fold0 : ('a,'b) t0 -> 'c -> ( 'c -> 'a -> 'b -> 'c [@bs]) ->  'c
val forAll0 :  ('a,'b) t0 -> ('a -> 'b -> bool [@bs]) -> bool
val exists0 :  ('a,'b) t0 -> ('a -> 'b -> bool [@bs]) -> bool

val join : ('a,'b) t0 -> 'a -> 'b -> ('a,'b) t0 -> ('a, 'b) t0

val concat : ('a,'b) t0 -> ('a,'b) t0 -> ('a,'b) t0

val concatOrJoin :
  ('a,'b) t0 -> 'a -> 'b option -> ('a,'b) t0 -> ('a, 'b) t0
val filterShared0 : 
  ('a,'b) t0 ->
  ('a -> 'b -> bool [@bs]) -> 
  ('a,'b) t0

val filterMap0 :    
  ('a, 'b) t0 -> 
  ('a -> 'b -> 'c option [@bs]) -> 
  ('a, 'c) t0 
(* seems no sharing, could be shared with mutation *)
val partitionShared0 :  
  ('a,'b) t0 -> 
  ('a -> 'b -> bool [@bs]) ->
  ('a,'b) t0 * ('a,'b) t0


val lengthNode : ('a, 'b) node -> int
val length0 : ('a,'b) t0 -> int

val toList0 : ('a,'b) t0 -> ('a * 'b) list
val checkInvariant : ('a,'b) t0 -> bool

val fillArray : ('a,'b) node -> int -> ('a * 'b) array -> int  

val toArray0 : ('a, 'b) t0 -> ('a * 'b) array  
val keysToArray0 : ('a, 'b) t0 -> 'a array
val valuesToArray0 : ('a, 'b) t0 -> 'b array 
val ofSortedArrayAux : ('a * 'b) array -> int -> int -> ('a, 'b) t0
val ofSortedArrayRevAux : ('a * 'b) array -> int -> int -> ('a, 'b) t0
val ofSortedArrayUnsafe0 : ('a * 'b) array -> ('a, 'b) t0

val cmp0 : 
  ('a, 'b) t0 -> ('a, 'c) t0 -> 
  kcmp:('a,_) Bs_Cmp.cmp -> 
  vcmp :('b -> 'c -> int [@bs]) -> 
  int 

val eq0:   
  ('a, 'b) t0 -> ('a, 'c) t0 -> 
  kcmp:('a,_) Bs_Cmp.cmp -> 
  vcmp :('b -> 'c -> bool [@bs]) -> 
  bool

val findOpt0:  
  ('a, 'b) t0 -> 
  'a -> 
  cmp:('a,_) Bs_Cmp.cmp -> 
  'b option 

val findNull0:  
  ('a, 'b) t0 -> 
  'a -> 
  cmp:('a,_) Bs_Cmp.cmp -> 
  'b Js.null

val findWithDefault0:  
  ('a, 'b) t0 -> 
  'a -> 
  'b -> 
  cmp:('a,_) Bs_Cmp.cmp -> 
  'b 
val findExn0:  
  ('a, 'b) t0 -> 
  'a -> 
  cmp:('a,_) Bs_Cmp.cmp ->   
  'b 

val mem0:  
  ('a, 'b) t0 -> 
  'a -> 
  cmp:('a,_) Bs_Cmp.cmp -> 
  bool


  
val ofArray0 : cmp:('a,'id) Bs_Cmp.cmp -> ('a * 'b) array -> ('a, 'b) t0

val updateMutate :   
  ('a, 'b) t0 -> 'a -> 'b -> 
  cmp:('a,'id) Bs_Cmp.cmp -> 
  ('a, 'b) t0

val balMutate :   
  ('a, 'b) node -> ('a, 'b) node 

val removeMinAuxWithRootMutate :   
  ('a, 'b) node -> 
  ('a, 'b) node -> 
  ('a, 'b) t0 