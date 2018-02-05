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

(** {!Bs.List}
    
    Utilities for List data type
*)

type 'a t = 'a list
(** [t] is compatible with built-in [list] type *)

val length: 'a t -> int
val size: 'a t -> int
(** [size l] is the same as [lenth l] *)

val head: 'a t -> 'a option

val tail: 'a t -> 'a t option

val add: 'a t -> 'a -> 'a t

val get: 'a t -> int -> 'a option
(** [get xs n]

    return the nth element in [xs]
 *)

val getExn: 'a t -> int -> 'a
(** [getExn xs n]

    {b raise} an exception if [n] is larger than the length of list
*)  

val make: int -> 'a -> 'a t
(**  [make n v] 
  
    - return empty if [n] is negative    
    - return a list of length [n] filled with [v]  
*)
    
val makeByU: int -> (int -> 'a [@bs]) -> 'a t 
val makeBy: int -> (int -> 'a) -> 'a t
(** [makeBy n f] 
    
    - return empty if [n] is negative    
    - return a list of length [n] filled with [f i] start from [0] to [n - 1]

*)    


val drop: 'a t -> int -> 'a t option 
(** [drop xs n]
    drop [n] elements from [xs]
*)
val take: 'a t -> int -> 'a t option 
(** [take xs n]
    take [n] elements from [xs]
*)

val splitAt: 'a t -> int -> ('a list * 'a list) option 
(** [splitAt ls n]

    return None when the length of [ls] is less than [n]
*)
    
val concat: 'a t -> 'a t -> 'a t

val concatMany: 'a t array -> 'a t

val reverseConcat: 'a t -> 'a t -> 'a t
(**
   [reverseConcat a b] is  equivalent to [concat (reverse a) b]
*)
    
val flatten: 'a t t -> 'a t

val mapU: 'a t -> ('a -> 'b [@bs]) -> 'b t
val map: 'a t -> ('a -> 'b) -> 'b t 

val zip: 'a t -> 'b t -> ('a * 'b) t
(** [zip xs ys]
  stops with shorter array
*)

val zipByU: 'a t -> 'b t ->  ('a -> 'b -> 'c [@bs]) -> 'c t
val zipBy: 'a t -> 'b t ->  ('a -> 'b -> 'c ) -> 'c t
(** [zipBy xs ys f]
  stops with shorter array 
*)

val mapWithIndexU: 'a t ->  (int -> 'a -> 'b [@bs]) -> 'b t
val mapWithIndex: 'a t ->  (int -> 'a -> 'b ) -> 'b t

val ofArray: 'a array -> 'a t 
   
val toArray: 'a t -> 'a array

(* type json = Js_json.t  *)

(* val toJson : 'a t -> ('a -> json  [@bs]) -> json *)
(* val fromJson : json -> (json -> 'a [@bs]) -> 'a t  *)


val reverse: 'a t -> 'a t

    
val mapReverseU: 'a t -> ('a -> 'b [@bs]) -> 'b t
val mapReverse: 'a t -> ('a -> 'b ) -> 'b t
(** [mapReverse a f] is  equivalent to [reverse (map a f)]    
*)

val forEachU: 'a t ->  ('a -> 'b [@bs]) -> unit
val forEach: 'a t ->  ('a -> 'b ) -> unit

val forEachWithIndexU: 'a t -> (int -> 'a -> 'b [@bs]) -> unit
val forEachWithIndex: 'a t -> (int -> 'a -> 'b) -> unit


val reduceU:  'a t -> 'b ->  ('b -> 'a -> 'b [@bs]) ->'b
val reduce:  'a t -> 'b ->  ('b -> 'a -> 'b) ->'b

val reduceReverseU: 'a t -> 'b -> ('a -> 'b -> 'b [@bs])  -> 'b
val reduceReverse: 'a t -> 'b -> ('a -> 'b -> 'b )  -> 'b

val mapReverse2U: 'a t -> 'b t -> ('a -> 'b -> 'c [@bs]) ->  'c t
val mapReverse2: 'a t -> 'b t -> ('a -> 'b -> 'c ) ->  'c t
(** [mapReverse2 xs ys f]

    equivalent to [reverse (map2 xs ys f)]    

    @example {[
      mapReverse2 [1;2;3] [1;2] (+) = [4;2]
    ]}
*)

val forEach2U: 'a t -> 'b t ->  ('a -> 'b -> 'c [@bs]) -> unit
val forEach2: 'a t -> 'b t ->  ('a -> 'b -> 'c ) -> unit
(** [forEach2 xs ys f] stop with the shorter list
*)  


val reduce2U:
  'b t -> 'c t  -> 'a  -> ('a -> 'b -> 'c -> 'a [@bs]) ->  'a
val reduce2:
  'b t -> 'c t  -> 'a  -> ('a -> 'b -> 'c -> 'a ) ->  'a
(** [reduce2 xs ys init f ]

    stops with the shorter list. 
*)

val reduceReverse2U:
  'a t -> 'b t -> 'c -> ('a -> 'b -> 'c -> 'c [@bs]) ->  'c
val reduceReverse2:
  'a t -> 'b t -> 'c -> ('a -> 'b -> 'c -> 'c ) ->  'c
(**
   [reduceReverse2 xs ys init f ]

   Stops with the shorter list

   @example {[
     reduceReverse2 [1;2;3] [1;2] 0 (fun acc x y -> acc + x + y ) = 6
   ]}
*)

val everyU: 'a t -> ('a -> bool [@bs]) ->  bool
val every: 'a t -> ('a -> bool ) ->  bool

val someU: 'a t -> ('a -> bool [@bs]) -> bool
val some: 'a t -> ('a -> bool ) -> bool

val every2U: 'a t -> 'b t -> ('a -> 'b -> bool [@bs]) -> bool
val every2: 'a t -> 'b t -> ('a -> 'b -> bool ) -> bool


val cmpU: 'a t -> 'a t -> ('a -> 'a -> int [@bs]) -> int
val cmp: 'a t -> 'a t -> ('a -> 'a -> int) -> int
(** @example {[
  cmp 
]}
*)
val eqU: 'a t -> 'a t -> ('a -> 'a -> bool [@bs]) -> bool
val eq: 'a t -> 'a t -> ('a -> 'a -> bool) -> bool
(**
    @example {[
      eq [1;2;3] [1;2] (=) = false ;;
      eq [1;2] [1;2] (=) = true
    ]}
*)  

val some2U:  'a t -> 'b t -> ('a -> 'b -> bool [@bs]) -> bool
val some2:  'a t -> 'b t -> ('a -> 'b -> bool) -> bool

val hasU:  'a t -> 'b ->  ('a -> 'b -> bool [@bs]) -> bool
val has:  'a t -> 'b ->  ('a -> 'b -> bool) -> bool


val getByU: 'a t -> ('a -> bool [@bs]) ->  'a option
val getBy: 'a t -> ('a -> bool) ->  'a option

val keepU: 'a t ->  ('a -> bool [@bs]) -> 'a t
val keep: 'a t ->  ('a -> bool ) -> 'a t

val keepMapU: 'a t -> ('a -> 'b option [@bs]) -> 'b t
val keepMap: 'a t -> ('a -> 'b option) -> 'b t

val partitionU: 'a t -> ('a -> bool [@bs]) ->  'a t * 'a t
val partition: 'a t -> ('a -> bool) ->  'a t * 'a t

val unzip: ('a * 'b) t -> 'a t * 'b t

val assocU: ('a * 'c) t -> 'b ->  ('a -> 'b -> bool [@bs])  -> 'c option
val assoc: ('a * 'c) t -> 'b ->  ('a -> 'b -> bool)  -> 'c option



val hasAssocU: ('a * 'c) t -> 'b -> ('a -> 'b -> bool [@bs]) -> bool
val hasAssoc: ('a * 'c) t -> 'b -> ('a -> 'b -> bool ) -> bool


val removeAssocU:
  ('a * 'c) t ->
  'b -> 
  ('a -> 'b -> bool [@bs]) -> ('a * 'c) t
val removeAssoc:
  ('a * 'c) t ->
  'b -> 
  ('a -> 'b -> bool) -> ('a * 'c) t




