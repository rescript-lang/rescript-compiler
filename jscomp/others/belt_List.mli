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

(** {!Belt.List}
    
    Utilities for List data type.
    
    This module is compatible with original ocaml stdlib.
    In general, all functions comes with the original stdlib also
    applies to this collection, however, this module provides  faster
    and stack safer utilities 

*)

type 'a t = 'a list
(** ['a t] is compatible with built-in [list] type *)

val length: 'a t -> int
(** [length l]

    @return the length of the list [l]
*)

val size: 'a t -> int
(** {b See} {!length} *)

val head: 'a t -> 'a option
(**
   @example {[
     head [] = None ;;
     head [1;2;3] = Some 1 ;;
   ]}
*)
val headExn: 'a t -> 'a  
(** [headExn h]

    {b See} {!head}
    
    {b raise} an exception if [h] is empty

*)

val tail: 'a t -> 'a t option
(** @example{[

      tail [] = None ;;
      tail [1;2] = Some [2];;
    ]}
*)
    
val tailExn: 'a t -> 'a t 
(** [tailExn h]

    {b See} {!tail}
    
    {b raise} an exception if [h] is empty
*)

val add: 'a t -> 'a -> 'a t
(**
   @example{[
     add [1] 3 = [3;1];;
   ]}
*)
val get: 'a t -> int -> 'a option
(** [get xs n]

    return the nth element in [xs],
    or [None] if [n] is larger than the length

    @example {[
      get [0;3;32] 2 = Some 2 ;;
      get [0;3;32] 3 = None;;
    ]}
 *)

val getExn: 'a t -> int -> 'a
(** [getExn xs n]

    {b See} {!get}
    
    {b raise} an exception if [n] is larger than the length
*)  

val make: int -> 'a -> 'a t
(**  [make n v] 
  
    - return a list of length [n] with each element filled with [v]  
    - return the empty list if [n] is negative

     @example {[
       make 3 1 =  [1;1;1]
     ]}
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

val shuffle: 'a t -> 'a t 
(** [shuffle xs]
   @return a new list in random order
*)


val drop: 'a t -> int -> 'a t option 
(** [drop xs n]

    return the list obtained by dropping the first [n] elements,
    or [None] if [xs] has fewer than [n] elements

    @example {[
      drop [1;2;3] 2 = Some [3];;
      drop [1;2;3] 3 = Some [];;
      drop [1;2;3] 4 = None;;
    ]}
*)

val take: 'a t -> int -> 'a t option 
(** [take xs n]

    return a list with the first [n] elements from [xs],
    or [None] if [xs] has fewer than [n] elements

    @example {[
      take [1;2;3] 1 = Some [1];;
      take [1;2;3] 2 = Some [1;2];;
      take [1;2;3] 4 = None;;
    ]}
*)

val splitAt: 'a t -> int -> ('a list * 'a list) option 
(**
    [splitAt xs n]
    split the list [xs] at position [n]
    return None when the length of [xs] is less than [n]

   @example{[
     splitAt [0;1;2;3;4] 2 = Some ([0;1], [2;3;4])
   ]} 
*)
    
val concat: 'a t -> 'a t -> 'a t
(**
    [concat xs ys]

    @return the list obtained by adding [ys] after [xs]

   @example {[
     concat [1;2;3] [4;5] = [1;2;3;4;5]
   ]}
*)

val concatMany: 'a t array -> 'a t
(**
    [concatMany a]
    return the list obtained by concatenating in order all the lists in array [a]

   @example {[
     concatMany [| [1;2;3] ; []; [3]; [4] |] = [1;2;3;3;4]
   ]}
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

   @example {[
     flatten [ [1;2;3] ; []; [3]; [4] ] = [1;2;3;3;4]
   ]}
*)

val mapU: 'a t -> ('a -> 'b [@bs]) -> 'b t
val map: 'a t -> ('a -> 'b) -> 'b t
(**
    [map xs f]

    return the list obtained by applying [f] to the element of [xs]

   @example {[
     map [1;2] (fun x-> x + 1) = [3;4]
   ]}
*)

val zip: 'a t -> 'b t -> ('a * 'b) t
(** [zip xs ys]

    @return a list of pairs from the two lists
    with the length of the shorter list

    @example {[
      zip [1;2] [1;2;3] = [(1,1); (2,2)]
    ]}
*)

val zipByU: 'a t -> 'b t -> ('a -> 'b -> 'c [@bs]) -> 'c t
val zipBy: 'a t -> 'b t -> ('a -> 'b -> 'c) -> 'c t
(** [zipBy xs ys f]

    {b See} {!zip}
    
    Equivalent to [zip xs ys |> List.map (fun (x,y) -> f x y)]
*)

val mapWithIndexU: 'a t -> (int -> 'a -> 'b [@bs]) -> 'b t
val mapWithIndex: 'a t -> (int -> 'a -> 'b) -> 'b t
(** @example {[
      mapWithIndex [1;2;3] (fun i x -> i + x) =
      [0 + 1; 1 + 2; 2 + 3 ]
    ]}
*)

val ofArray: 'a array -> 'a t 
(** @example {[
      ofArray [|1;2;3|]  = [1;2;3]
    ]}
*)

val toArray: 'a t -> 'a array
(** @example {[
      toArray [1;2;3] = [|1;2;3|]
    ]}
*)
(* type json = Js_json.t  *)

(* val toJson : 'a t -> ('a -> json  [@bs]) -> json *)
(* val fromJson : json -> (json -> 'a [@bs]) -> 'a t  *)


val reverse: 'a t -> 'a t
(** @example {[
      reverse [1;2;3] = [3;2;1]
    ]}
*)
    
val mapReverseU: 'a t -> ('a -> 'b [@bs]) -> 'b t
val mapReverse: 'a t -> ('a -> 'b) -> 'b t
(** [mapReverse a f]

    Equivalent to [reverse (map a f)]    
*)

val forEachU: 'a t -> ('a -> 'b [@bs]) -> unit
val forEach: 'a t -> ('a -> 'b) -> unit
(** [forEach xs f ]
    @example {[
      let us = ref 0;;
      forEach [1;2;3;4] (fun x -> us := !us + x);;
      !us  = 1 + 2 + 3 + 4;;
    ]}
*)
  
val forEachWithIndexU: 'a t -> (int -> 'a -> 'b [@bs]) -> unit
val forEachWithIndex: 'a t -> (int -> 'a -> 'b) -> unit
(** [forEachWithIndex xs f]

    @example {[
      let us = ref 0 ;;
      forEachWithIndex [1;1;1;1] (fun i x -> us := !us + x + i);;
      !us  = 0 + 1 + 1 +  1 + 2 + 1 + 3 + 1;;
    ]}
*)

val reduceU:  'a t -> 'b -> ('b -> 'a -> 'b [@bs]) -> 'b
val reduce:  'a t -> 'b -> ('b -> 'a -> 'b) -> 'b
(** [reduce xs f]

    @example {[
      reduce [1;2;3;4] 0 (+) = 10;;
      reduce [1;2;3;4] 10 (-) = 0;;
      reduce [1;2;3;4] [] add = [4;3;2;1];
    ]}
*)
  
val reduceReverseU: 'a t -> 'b -> ('b -> 'a ->  'b [@bs]) -> 'b
val reduceReverse: 'a t -> 'b -> ('b -> 'a ->  'b) -> 'b
(** [reduceReverse xs f]

    @example {[
      reduceReverse [1;2;3;4] 0 (+) = 10;;
      reduceReverse [1;2;3;4] 10 (-) = 0;;
      reduceReverse [1;2;3;4] [] add = [1;2;3;4];;
    ]}
*)
  
val mapReverse2U: 'a t -> 'b t -> ('a -> 'b -> 'c [@bs]) -> 'c t
val mapReverse2: 'a t -> 'b t -> ('a -> 'b -> 'c) -> 'c t
(** [mapReverse2 xs ys f]

    equivalent to [reverse (zipBy xs ys f)]    

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
  'a t -> 'b t -> 'c -> ('c -> 'a -> 'b ->  'c [@bs]) -> 'c
val reduceReverse2:
  'a t -> 'b t -> 'c -> ('c -> 'a -> 'b ->  'c) -> 'c
(**
   [reduceReverse2 xs ys init f ]

   Stops with the shorter list

   @example {[
     reduceReverse2 [1;2;3] [1;2] 0 (fun acc x y -> acc + x + y) = 6
   ]}
*)

val everyU: 'a t -> ('a -> bool [@bs]) ->  bool
val every: 'a t -> ('a -> bool ) ->  bool
(** [every ls p]

    @example {[
      every [] (fun x -> x mod 2 = 0) = true;;
      every [2;4] (fun x -> x mod 2 = 0 ) true;;
    ]}
*)
val someU: 'a t -> ('a -> bool [@bs]) -> bool
val some: 'a t -> ('a -> bool ) -> bool
(** [some ls p]

    @example {[
      some [] (fun x -> x mod 2 = 0) = false ;;
      some [1;2] (fun x -> x mod 2 = 0) = true;;)
    ]}
*)

val every2U: 'a t -> 'b t -> ('a -> 'b -> bool [@bs]) -> bool
val every2: 'a t -> 'b t -> ('a -> 'b -> bool ) -> bool
(** [every2 xs ys p] stop with the shorter list
    @example {[
      (every2 [] [1] (fun   x y -> x > y)) = true;;
      (every2 [2;3] [1] (fun   x y -> x > y)) = true;;    
    ]}
*)

val some2U:  'a t -> 'b t -> ('a -> 'b -> bool [@bs]) -> bool
val some2:  'a t -> 'b t -> ('a -> 'b -> bool) -> bool
(** [some2 xs ys p]
    @example {[
      (some2 [] [1] (fun   x y -> x > y)) =  false;;
      (some2 [2;3] [1;4] (fun   x y -> x > y)) = true;;
    ]}
*)

val cmpByLength: 'a t -> 'a t -> int
(** [cmpByLength l1 l2]

    Compare two lists solely by length
*)
  
val cmpU: 'a t -> 'a t -> ('a -> 'a -> int [@bs]) -> int
val cmp: 'a t -> 'a t -> ('a -> 'a -> int) -> int
(**
    [cmp xs ys cmpElem]
    compare lists [xs] and [ys] using [cmpElem] to compare elements
    @example {[
      cmp [1;2;3] [1;2;3] compare = 0;;
      cmp [1;2;3] [0;1;2;3] compare = 1 ;;]
  ]}

   {b Attention}: The total ordering of List is different from Array,
   for Array, we compare the length first and one by one later, while
   for lists, we just compare one by one 
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


val hasU:  'a t -> 'b -> ('a -> 'b -> bool [@bs]) -> bool
val has:  'a t -> 'b -> ('a -> 'b -> bool) -> bool
(** @example {[
      has [1;2;3] 2 (=) = true;;
      has [1;2;3] 4 (=) = false;;
    ]}
*)

val getByU: 'a t -> ('a -> bool [@bs]) -> 'a option
val getBy: 'a t -> ('a -> bool) -> 'a option
(** @example {[
      getBy [1;4;3;2] (fun x -> x mod 2 = 0) = Some 4
    ]}
*)
    
val keepU: 'a t ->  ('a -> bool [@bs]) -> 'a t
val keep: 'a t ->  ('a -> bool) -> 'a t
(** [keep  xs p]

    @example {[
      keep [1;2;3;4] (fun x -> x mod 2 = 0) =
      [2;4]
    ]}
*)
val keepMapU: 'a t -> ('a -> 'b option [@bs]) -> 'b t
val keepMap: 'a t -> ('a -> 'b option) -> 'b t
(** [keepMap xs f]

    @example {[
      keepMap [1;2;3;4] (fun x -> if x mod 2 = 0 then Some (-x ) else None)
      =
      [-2;-4]
    ]}
*)
val partitionU: 'a t -> ('a -> bool [@bs]) ->  'a t * 'a t
val partition: 'a t -> ('a -> bool) ->  'a t * 'a t
(** [partition xs p]

    @example {[
      partition [1;2;3;4] (fun x -> x mod 2 = 0) =
      ([2;4], [1;3])
    ]}
*)
val unzip: ('a * 'b) t -> 'a t * 'b t
(** [unzip xs]

    @example {[
      unzip [(1,2) ; (3,4)] = ([1;3], [2;4])
    ]}
*)
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
(** [setAssoc xs k v eq]
    if [k] exists in [xs], replace it with the new [v], otherwise, add
    it to the head
    @example {[
      setAssoc [1,"a"; 2, "b"; 3, "c"] 2 "x" (=) =
      [1,"a"; 2, "x"; 3,"c"] ;; 

      setAssoc [1,"a"; 3, "c"] 2 "2" (=) = 
      [2,"2"; 1,"a"; 3, "c"] 
    ]}
*)    




