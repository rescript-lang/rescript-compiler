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

val headOpt : 'a t -> 'a option

val tailOpt : 'a t -> 'a t option

val nthOpt : 'a t -> int -> 'a option

val nthAssert : 'a t -> int -> 'a

val dropOpt : 'a t -> int -> 'a t option 

val takeOpt : 'a t -> int -> 'a t option 

val splitAtOpt : 'a t -> int -> ('a list * 'a list) option 

val append : 'a t -> 'a t -> 'a t

val map : 'a t -> ('a -> 'b [@bs]) -> 'b t

val map2 : 'a t -> 'b t ->  ('a -> 'b -> 'c [@bs]) -> 'c t

val mapi : 'a t ->  (int -> 'a -> 'b [@bs]) -> 'b t

val init : int -> (int -> 'a [@bs]) -> 'a t

val length : 'a t -> int

val toArray : 'a t -> 'a array

type json = Js_json.t 

val toJson : 'a t -> ('a -> json  [@bs]) -> json
val fromJson : json -> (json -> 'a [@bs]) -> 'a t 

val revAppend : 'a t -> 'a t -> 'a t

val rev : 'a t -> 'a t


val flatten : 'a t t -> 'a t

val mapRev : 'a t -> ('a -> 'b [@bs]) -> 'b t

val iter : 'a t ->  ('a -> 'b [@bs]) -> unit

val iteri : 'a t -> (int -> 'a -> 'b [@bs]) -> unit

val foldLeft :  'a t -> 'b ->  ('b -> 'a -> 'b [@bs]) ->'b

val foldRight : 'a t -> 'b -> ('a -> 'b -> 'b [@bs])  -> 'b

val mapRev2 : 'a t -> 'b t -> ('a -> 'b -> 'c [@bs]) ->  'c t

val iter2 : 'a t -> 'b t ->  ('a -> 'b -> 'c [@bs]) -> unit

val foldLeft2 :
  'b t -> 'c t  -> 'a  -> ('a -> 'b -> 'c -> 'a [@bs]) ->  'a

val foldRight2 :
  'a t -> 'b t -> 'c -> ('a -> 'b -> 'c -> 'c [@bs]) ->  'c

val forAll : 'a t -> ('a -> bool [@bs]) ->  bool

val exists : 'a t -> ('a -> bool [@bs]) -> bool

val forAll2 : 'a t -> 'b t -> ('a -> 'b -> bool [@bs]) -> bool

val exists2 :  'a t -> 'b t -> ('a -> 'b -> bool [@bs]) -> bool

val mem :  'a t -> 'b ->  ('a -> 'b -> bool [@bs]) -> bool

val memq :  'a t -> 'a ->bool

val assocOpt : ('a * 'c) t -> 'b ->  ('a -> 'b -> bool [@bs])  -> 'c option

val assqOpt : ('a * 'b) t -> 'a ->  'b option

val memAssoc : ('a * 'c) t -> 'b -> ('a -> 'b -> bool [@bs]) -> bool

val memAssq : ('a * 'b) t -> 'a -> bool

val removeAssoc :
  ('a * 'c) t ->
  'b -> 
  ('a -> 'b -> bool [@bs]) -> ('a * 'c) t

val removeAssq :  ('a * 'b) t -> 'a -> ('a * 'b) t

val findOpt : 'a t -> ('a -> bool [@bs]) ->  'a option

val filter : 'a t ->  ('a -> bool [@bs]) -> 'a t

val partition : 'a t -> ('a -> bool [@bs]) ->  'a t * 'a t

val unzip : ('a * 'b) t -> 'a t * 'b t

val zip : 'a t -> 'b t -> ('a * 'b) t
