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
(** ['a t] is compatible with built-in [list] type *)

val length: 'a t -> int
(** [length l]
    return the length of the list [l]
*)

val size: 'a t -> int
(** [size l] is the same as [length l] *)

val head: 'a t -> 'a option

val headExn: 'a t -> 'a  
(** [headExn h]

    {b raise} an exception if [h] is emmpty
*)

val tail: 'a t -> 'a t option

val tailExn: 'a t -> 'a t 
(** [tailExn h]

    {b raise} an exception if [h] is empty
*)

val add: 'a t -> 'a -> 'a t

val get: 'a t -> int -> 'a option
(** [get xs n]

    return the nth element in [xs],
    or [None] if [n] is larger than the length
 *)

val getExn: 'a t -> int -> 'a
(** [getExn xs n]

    return the nth element in [xs],
    or {b raise} an exception if [n] is larger than the length
*)  

val make: int -> 'a -> 'a t
(**  [make n v] 
  
    - return a list of length [n] with each element filled with [v]  
    - return the empty list if [n] is negative
*)
    
val makeByU: int -> (int -> 'a [@bs]) -> 'a t 
val makeBy: int -> (int -> 'a) -> 'a t
(** [makeBy n f] 
    
    - return a list of length [n] with element [i] initialized with [f i]
    - return the empty list if [n] is negative

    @example {[
      makeBy 5 (fun i -> i) = [0;1;2;3;4]
    ]}
*)    

val drop: 'a t -> int -> 'a t option 
(** [drop xs n]
    return the list obtained by dropping the first [n] elements,
    or [None] if [xs] has fewer than [n] elements
*)

val take: 'a t -> int -> 'a t option 
(** [take xs n]
    return a list with the first [n] elements from [xs],
    or [None] if [xs] has fewer than [n] elements
*)

val splitAt: 'a t -> int -> ('a list * 'a list) option 
(**
    [splitAt xs n]
    split the list [xs] at position [n]
    return None when the length of [xs] is less than [n]
*)
    
val concat: 'a t -> 'a t -> 'a t
(**
    [concat xs ys]
    return the list obtained by adding [ys] after [xs]
*)

val concatMany: 'a t array -> 'a t
(**
    [concatMany a]
    return the list obtained by concatenating in order all the lists in array [a]
*)

val reverseConcat: 'a t -> 'a t -> 'a t
(**
   [reverseConcat xs ys] is  equivalent to [concat (reverse xs) ys]
   @example {[
     reverseConcat [1;2] [3;4] = [2;1;3;4]
   ]}
*)
    
val flatten: 'a t t -> 'a t
(**
    [flatten ls]
    return the list obtained by concatenating in order all the lists in list [ls]
*)

val mapU: 'a t -> ('a -> 'b [@bs]) -> 'b t
val map: 'a t -> ('a -> 'b) -> 'b t
(**
    [map xs f]
    return the list obtained by applying [f] to the element of [xs]
*)

val zip: 'a t -> 'b t -> ('a * 'b) t
(** [zip xs ys]
  return a list of pairs from the two lists
  with the length of the shorter list
*)

val zipByU: 'a t -> 'b t -> ('a -> 'b -> 'c [@bs]) -> 'c t
val zipBy: 'a t -> 'b t -> ('a -> 'b -> 'c) -> 'c t
(** [zipBy xs ys f]

  equivalent to [zip xs ys |> List.map (fun (x,y) -> f x y)]
*)

val mapWithIndexU: 'a t -> (int -> 'a -> 'b [@bs]) -> 'b t
val mapWithIndex: 'a t -> (int -> 'a -> 'b) -> 'b t

val ofArray: 'a array -> 'a t 
   
val toArray: 'a t -> 'a array

(* type json = Js_json.t  *)

(* val toJson : 'a t -> ('a -> json  [@bs]) -> json *)
(* val fromJson : json -> (json -> 'a [@bs]) -> 'a t  *)


val reverse: 'a t -> 'a t

    
val mapReverseU: 'a t -> ('a -> 'b [@bs]) -> 'b t
val mapReverse: 'a t -> ('a -> 'b) -> 'b t
(** [mapReverse a f] is equivalent to [reverse (map a f)]    
*)

val forEachU: 'a t -> ('a -> 'b [@bs]) -> unit
val forEach: 'a t -> ('a -> 'b) -> unit

val forEachWithIndexU: 'a t -> (int -> 'a -> 'b [@bs]) -> unit
val forEachWithIndex: 'a t -> (int -> 'a -> 'b) -> unit


val reduceU:  'a t -> 'b -> ('b -> 'a -> 'b [@bs]) -> 'b
val reduce:  'a t -> 'b -> ('b -> 'a -> 'b) -> 'b

val reduceReverseU: 'a t -> 'b -> ('a -> 'b -> 'b [@bs]) -> 'b
val reduceReverse: 'a t -> 'b -> ('a -> 'b -> 'b) -> 'b

val mapReverse2U: 'a t -> 'b t -> ('a -> 'b -> 'c [@bs]) -> 'c t
val mapReverse2: 'a t -> 'b t -> ('a -> 'b -> 'c) -> 'c t
(** [mapReverse2 xs ys f]

    equivalent to [reverse (map2 xs ys f)]    

    @example {[
      mapReverse2 [1;2;3] [1;2] (+) = [4;2]
    ]}
*)

val forEach2U: 'a t -> 'b t -> ('a -> 'b -> 'c [@bs]) -> unit
val forEach2: 'a t -> 'b t -> ('a -> 'b -> 'c) -> unit
(** [forEach2 xs ys f] stop with the shorter list
*)  


val reduce2U:
  'b t -> 'c t -> 'a -> ('a -> 'b -> 'c -> 'a [@bs]) -> 'a
val reduce2:
  'b t -> 'c t -> 'a -> ('a -> 'b -> 'c -> 'a) -> 'a
(** [reduce2 xs ys init f ]

    stops with the shorter list. 
*)

val reduceReverse2U:
  'a t -> 'b t -> 'c -> ('a -> 'b -> 'c -> 'c [@bs]) -> 'c
val reduceReverse2:
  'a t -> 'b t -> 'c -> ('a -> 'b -> 'c -> 'c) -> 'c
(**
   [reduceReverse2 xs ys init f ]

   Stops with the shorter list

   @example {[
     reduceReverse2 [1;2;3] [1;2] 0 (fun acc x y -> acc + x + y) = 6
   ]}
*)

val everyU: 'a t -> ('a -> bool [@bs]) -> bool
val every: 'a t -> ('a -> bool) -> bool

val someU: 'a t -> ('a -> bool [@bs]) -> bool
val some: 'a t -> ('a -> bool) -> bool

val every2U: 'a t -> 'b t -> ('a -> 'b -> bool [@bs]) -> bool
val every2: 'a t -> 'b t -> ('a -> 'b -> bool) -> bool


val cmpU: 'a t -> 'a t -> ('a -> 'a -> int [@bs]) -> int
val cmp: 'a t -> 'a t -> ('a -> 'a -> int) -> int
(**
    [cmp xs ys cmpElem]
    compare lists [xs] and [ys] using [cmpElem] to compare elements
    @example {[
      cmp [1;2;3] [1;2;3] compare = 0
  ]}
*)


val eqU: 'a t -> 'a t -> ('a -> 'a -> bool [@bs]) -> bool
val eq: 'a t -> 'a t -> ('a -> 'a -> bool) -> bool
(**
   [eq xs ys eqElem]
   check equality of [xs] and [ys] using [eqElem] for equality on elements

    @example {[
      eq [1;2;3] [1;2] (=) = false ;;
      eq [1;2] [1;2] (=) = true
    ]}
*)  

val some2U:  'a t -> 'b t -> ('a -> 'b -> bool [@bs]) -> bool
val some2:  'a t -> 'b t -> ('a -> 'b -> bool) -> bool

val hasU:  'a t -> 'b -> ('a -> 'b -> bool [@bs]) -> bool
val has:  'a t -> 'b -> ('a -> 'b -> bool) -> bool


val getByU: 'a t -> ('a -> bool [@bs]) -> 'a option
val getBy: 'a t -> ('a -> bool) -> 'a option

val keepU: 'a t -> ('a -> bool [@bs]) -> 'a t
val keep: 'a t -> ('a -> bool) -> 'a t

val keepMapU: 'a t -> ('a -> 'b option [@bs]) -> 'b t
val keepMap: 'a t -> ('a -> 'b option) -> 'b t

val partitionU: 'a t -> ('a -> bool [@bs]) -> 'a t * 'a t
val partition: 'a t -> ('a -> bool) -> 'a t * 'a t

val unzip: ('a * 'b) t -> 'a t * 'b t

val getAssocU: ('a * 'c) t -> 'b ->  ('a -> 'b -> bool [@bs])  -> 'c option
val getAssoc: ('a * 'c) t -> 'b ->  ('a -> 'b -> bool)  -> 'c option
(** [getAssoc xs k eq]
    
    return the second element of a pair in [xs] where the first element equals [x],
    or [None] if not found
    @example {[
      getAssoc [ 1, "a"; 2, "b"; 3, "c"] 2 (=) = Some "b"
    ]}
*)

val hasAssocU: ('a * 'c) t -> 'b -> ('a -> 'b -> bool [@bs]) -> bool
val hasAssoc: ('a * 'c) t -> 'b -> ('a -> 'b -> bool ) -> bool
(** [hasAssoc xs x eq]
     return true if there is a pair in [xs] where the first element equals [x]
    @example {[
      hasAssoc [1, "a"; 2, "b"; 3,"c"] 1 (=) = true
    ]}
*)

val removeAssocU:('a * 'c) t -> 'b -> ('a -> 'b -> bool [@bs]) -> ('a * 'c) t
val removeAssoc: ('a * 'c) t -> 'b ->  ('a -> 'b -> bool) -> ('a * 'c) t
(** [removeAssoc xs x eq]
    Try to remove the first pair, if not found, leave it untouched.
    @example {[
      removeAssoc [1,"a"; 2, "b"; 3, "c" ] 1 (=) =
      [2, "b"; 3, "c"]
    ]}
*)

val setAssocU: ('a * 'c) t -> 'a -> 'c -> ('a -> 'a -> bool [@bs]) -> ('a * 'c) t
val setAssoc: ('a * 'c) t -> 'a -> 'c -> ('a -> 'a -> bool) -> ('a * 'c) t    
(** [setAssoc xs x eq]

    @example {[
      setAssoc [1,"a"; 2, "b"; 3, "c"] 2 "x" (=) =
      [1,"a"; 2, "x"; 3,"c"] ;; 

      setAssoc [1,"a"; 3, "c"] 2 "2" (=) = 
      [2,"2"; 1,"a"; 3, "c"] 
    ]}
*)    




