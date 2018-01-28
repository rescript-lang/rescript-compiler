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

type 'a t = 'a list

val head: 'a t -> 'a option

val tail: 'a t -> 'a t option
val add: 'a t -> 'a -> 'a t
val get: 'a t -> int -> 'a option
val getExn: 'a t -> int -> 'a
  
val make: int -> 'a -> 'a t
(** [make n v] return empty if [n] is negative
*)
    
val makeBy: int -> (int -> 'a [@bs]) -> 'a t
(** [makeBy n f] return empty if [n] is negative
*)    


val drop: 'a t -> int -> 'a t option 
val take: 'a t -> int -> 'a t option 

val splitAt: 'a t -> int -> ('a list * 'a list) option 

val concat: 'a t -> 'a t -> 'a t

val map: 'a t -> ('a -> 'b [@bs]) -> 'b t

val map2: 'a t -> 'b t ->  ('a -> 'b -> 'c [@bs]) -> 'c t

val mapWithIndex: 'a t ->  (int -> 'a -> 'b [@bs]) -> 'b t

val length: 'a t -> int

val toArray: 'a t -> 'a array

(* type json = Js_json.t  *)

(* val toJson : 'a t -> ('a -> json  [@bs]) -> json *)
(* val fromJson : json -> (json -> 'a [@bs]) -> 'a t  *)

val reverseConcat: 'a t -> 'a t -> 'a t
(**
   [reverseAppend a b] is semantically equivalent to [concat (reverse a) b]
*)
    
val reverse: 'a t -> 'a t

val concatMany: 'a t t -> 'a t

val mapReverse: 'a t -> ('a -> 'b [@bs]) -> 'b t
(** [mapReverse a f] is semantically equivalent to [reverse (map a f)]    
*)
val forEach: 'a t ->  ('a -> 'b [@bs]) -> unit

val forEachWithIndex: 'a t -> (int -> 'a -> 'b [@bs]) -> unit

val reduce:  'a t -> 'b ->  ('b -> 'a -> 'b [@bs]) ->'b

val reduceReverse: 'a t -> 'b -> ('a -> 'b -> 'b [@bs])  -> 'b

val mapReverse2: 'a t -> 'b t -> ('a -> 'b -> 'c [@bs]) ->  'c t

val forEach2: 'a t -> 'b t ->  ('a -> 'b -> 'c [@bs]) -> unit

val reduce2:
  'b t -> 'c t  -> 'a  -> ('a -> 'b -> 'c -> 'a [@bs]) ->  'a

val reduceReverse2:
  'a t -> 'b t -> 'c -> ('a -> 'b -> 'c -> 'c [@bs]) ->  'c

val every: 'a t -> ('a -> bool [@bs]) ->  bool

val some: 'a t -> ('a -> bool [@bs]) -> bool

val every2: 'a t -> 'b t -> ('a -> 'b -> bool [@bs]) -> bool

val cmp: 'a t -> 'a t -> ('a -> 'a -> int [@bs]) -> int

val eq: 'a t -> 'a t -> ('a -> 'a -> bool [@bs]) -> bool
  
val some2:  'a t -> 'b t -> ('a -> 'b -> bool [@bs]) -> bool

val has:  'a t -> 'b ->  ('a -> 'b -> bool [@bs]) -> bool

val hasByReference:  'a t -> 'a ->bool

val assoc: ('a * 'c) t -> 'b ->  ('a -> 'b -> bool [@bs])  -> 'c option

val assocByReference: ('a * 'b) t -> 'a ->  'b option

val hasAssoc: ('a * 'c) t -> 'b -> ('a -> 'b -> bool [@bs]) -> bool

val hasAssocByReference: ('a * 'b) t -> 'a -> bool

val removeAssoc:
  ('a * 'c) t ->
  'b -> 
  ('a -> 'b -> bool [@bs]) -> ('a * 'c) t

val removeAssocByReference:  ('a * 'b) t -> 'a -> ('a * 'b) t

val getBy: 'a t -> ('a -> bool [@bs]) ->  'a option

val keepBy: 'a t ->  ('a -> bool [@bs]) -> 'a t
val keepMap: 'a t -> ('a -> 'b option [@bs]) -> 'b t
val partition: 'a t -> ('a -> bool [@bs]) ->  'a t * 'a t

val unzip: ('a * 'b) t -> 'a t * 'b t

val zip: 'a t -> 'b t -> ('a * 'b) t


