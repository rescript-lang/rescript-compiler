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


val create: ('a,'id) Bs_Hash.t -> int -> ('a, 'id) t
val clear: ('a, 'id) t -> unit

val add: ('a, 'id) t -> 'a -> unit
val has: ('a, 'id) t -> 'a -> bool
val remove: ('a, 'id) t -> 'a -> unit

val forEach: ('a, 'id) t -> ('a  -> unit [@bs]) ->  unit
(** Order unspecified. *)


val reduce: ('a, 'id) t -> 'c -> ('c -> 'a  ->  'c [@bs]) -> 'c
(** Order unspecified. *)

val size: ('a, 'id) t -> int  



val logStats: _ t -> unit


val toArray: ('a,'id) t -> 'a array 

val ofArray:   
  'a array ->
  dict:('a,'id) Bs_Hash.t -> 
  ('a,'id) t 


val mergeArrayDone: ('a,'id) t -> 'a array -> unit
val mergeArray: ('a,'id) t -> 'a array -> ('a, 'id)t

(****************************************************************************)
type ('a, 'id) t0

val getData: ('k, 'id) t  -> ('k, 'id) t0
val getDict: ('k, 'id) t  -> ('k, 'id) Bs_Hash.t
val packDictData: dict:('k, 'id) Bs_Hash.t -> data:('k, 'id) t0 -> ('k, 'id) t

val clear0 : ('a, 'id) t0 -> unit
val create0 : int -> ('a, 'id) t0
val reset0 : ('a, 'id) t0 -> unit
val add0 :
  hash:('a,'id) Bs_Hash.hash ->
  eq:('a,'id) Bs_Hash.eq -> 
  ('a,'id) t0 -> 'a ->  unit
val mem0: 
  hash:('a,'id) Bs_Hash.hash  -> 
  eq:('a,'id) Bs_Hash.eq -> 
  ('a, 'id) t0 -> 'a -> bool
val remove0: 
  hash:('a,'id) Bs_Hash.hash  -> 
  eq:('a,'id) Bs_Hash.eq -> 
  ('a, 'id) t0 -> 'a -> unit
val iter0 : ('a, 'id) t0 -> ('a -> unit [@bs]) ->  unit
val fold0 : ('a, 'id) t0 -> 'c -> ('c -> 'a ->  'c [@bs]) ->  'c
val length0 : ('a, 'id) t0 -> int
val logStats0 : ('a, 'id) t0 -> unit
val toArray0 : ('a,'id) t0 -> 'a array
val ofArray0 : 
  hash:('a,'id) Bs_Hash.hash  -> 
  eq:('a,'id) Bs_Hash.eq -> 
  'a array -> 
  ('a, 'id) t0      

val addArray0 : 
  hash:('a,'id) Bs_Hash.hash  -> 
  eq:('a,'id) Bs_Hash.eq -> 
  ('a,'id) t0 -> 'a array -> unit     
