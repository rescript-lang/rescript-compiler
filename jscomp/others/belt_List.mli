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
(** [length xs]

    @return the length of the list [xs]
*)

val size: 'a t -> int
(** {b See} {!length} *)

val head: 'a t -> 'a option
(**
  [head xs] returns [None] if [xs] is the empty list, otherwise it returns [Some value] where [value] is the first
  element in the list.
   @example {[
     head [] = None ;;
     head [1;2;3] = Some 1 ;;
   ]}
*)
val headExn: 'a t -> 'a  
(** [headExn xs]

    {b See} {!head}
    
    {b raise} an exception if [xs] is empty

*)

val tail: 'a t -> 'a t option
(**  [tail xs] returns [None] if [xs] is empty;
  otherwise it returns [Some xs2] where [xs2] is everything except the first element of [xs];

  @example{[

      tail [] = None;;
      tail [1;2;3;4] = Some [2;3;4];;
    ]}
*)
    
val tailExn: 'a t -> 'a t 
(** [tailExn xs]

    {b See} {!tail}
    
    {b raise} an exception if [xs] is empty
*)

val add: 'a t -> 'a -> 'a t
(**
  [add xs y] adds [y] to the beginning of list [xs]
  
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
  
    - return a list of length [n] with each element filled with value [v]  
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
      makeBy 5 (fun i -> i) = [0;1;2;3;4];;
      makeBy 5 (fun i -> i * i) = [0;1;4;9;16];;
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

    return the list obtained by applying [f] to each element of [xs]

   @example {[
     map [1;2] (fun x-> x + 1) = [3;4]
   ]}
*)

val zip: 'a t -> 'b t -> ('a * 'b) t
(** [zip xs ys]

    @return a list of pairs from the two lists
    with the length of the shorter list

    @example {[
      zip [1;2] [3;4;5] = [(1,3); (2,4)]
    ]}
*)

val zipByU: 'a t -> 'b t -> ('a -> 'b -> 'c [@bs]) -> 'c t
val zipBy: 'a t -> 'b t -> ('a -> 'b -> 'c) -> 'c t
(** [zipBy xs ys f]

    {b See} {!zip}
    
    Equivalent to [zip xs ys |> List.map (fun (x,y) -> f x y)]
    
    @example {[
      zipBy [1;2;3] [4;5] (fun a b -> 2 * a + b) = [6;9];;
    ]}

*)

val mapWithIndexU: 'a t -> (int -> 'a -> 'b [@bs]) -> 'b t
val mapWithIndex: 'a t -> (int -> 'a -> 'b) -> 'b t
(** [mapWithIndex xs f] applies [f] to each element of [xs]. Function [f] takes two arguments:
  the index starting from 0 and the element from [xs].

    @example {[
      mapWithIndex [1;2;3] (fun i x -> i + x) =
      [0 + 1; 1 + 2; 2 + 3 ]
    ]}
*)



val fromArray: 'a array -> 'a t 
(** [fromArray arr] converts the given array to a list

  @example {[
      fromArray [|1;2;3|]  = [1;2;3]
    ]}
*)

val toArray: 'a t -> 'a array
(** [toArray xs] converts the given list to an array
  @example {[
      toArray [1;2;3] = [|1;2;3|]
    ]}
*)
(* type json = Js_json.t  *)

(* val toJson : 'a t -> ('a -> json  [@bs]) -> json *)
(* val fromJson : json -> (json -> 'a [@bs]) -> 'a t  *)


val reverse: 'a t -> 'a t
(** [reverse xs] returns a new list whose elements are those of [xs] in reverse order. 
  @example {[
      reverse [1;2;3] = [3;2;1]
    ]}
*)
    
val mapReverseU: 'a t -> ('a -> 'b [@bs]) -> 'b t
val mapReverse: 'a t -> ('a -> 'b) -> 'b t
(** [mapReverse xs f]

    Equivalent to [reverse (map xs f)]
    
    @example {[
      mapReverse [3;4;5] (fun x -> x * x) = [25;16;9];;
    ]}
*)

val forEachU: 'a t -> ('a -> 'b [@bs]) -> unit
val forEach: 'a t -> ('a -> 'b) -> unit
(** [forEach xs f ]
    Call [f] on each element of [xs] from the beginning to end. [f] returns [unit], so no
    new array is created. Use [foreach] when you are primarily concerned with repetitively
    creating side effects.
    
    @example {[
      forEach ["a";"b";"c"] (fun x -> Js.log("Item: " ^ x));;
      (*  prints:
        Item: a
        Item: b
        Item: c
      *)

      let us = ref 0;;
      forEach [1;2;3;4] (fun x -> us := !us + x);;
      !us  = 1 + 2 + 3 + 4;;
    ]}
*)
  
val forEachWithIndexU: 'a t -> (int -> 'a -> 'b [@bs]) -> unit
val forEachWithIndex: 'a t -> (int -> 'a -> 'b) -> unit
(** [forEachWithIndex xs f]

    @example {[
    
      forEach ["a";"b";"c"] (fun i x -> Js.log("Item " ^ (string_of_int i) ^ " is " ^ x));;
      (*  prints:
        Item 0 is a
        Item 1 is b
        Item 2 is cc
      *)

      let total = ref 0 ;;
      forEachWithIndex [10;11;12;13] (fun i x -> total := !total + x + i);;
      !total  = 0 + 10 + 1 +  11 + 2 + 12 + 3 + 13;;
    ]}
*)

val reduceU:  'a t -> 'b -> ('b -> 'a -> 'b [@bs]) -> 'b
val reduce:  'a t -> 'b -> ('b -> 'a -> 'b) -> 'b
(** [reduce xs f]

    Applies [f] to each element of [xs] from beginning to end.  Function [f] has two parameters: the item
    from the list and an “accumulator”, which starts with a value of [init]. [reduce]
    returns the final value of the accumulator.
    
    @example {[
      reduce [1;2;3;4] 0 (+) = 10;;
      reduce [1;2;3;4] 10 (-) = 0;;
      reduce [1;2;3;4] [] add = [4;3;2;1];
    ]}
*)
  
val reduceReverseU: 'a t -> 'b -> ('b -> 'a ->  'b [@bs]) -> 'b
val reduceReverse: 'a t -> 'b -> ('b -> 'a ->  'b) -> 'b
(** [reduceReverse xs f]

    Works like {!reduce}, except that function [f] is applied to each item of [xs] from the last
    back to the first.

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

    Applies [f] to each element of [xs] and [ys] from beginning to end. Stops with the shorter list.
    Function [f] has three parameters: an “accumulator” which starts with a value of [init],
    an item from [xs], and an item from [ys]. [reduce2] returns the final value of the accumulator.
    
    @example {[
      reduce2 [1;2;3] [4;5] 0 (fun acc x y -> acc + x * x + y) =  0 + (1 * 1 + 4) + (2 * 2 + 5);;
      reduce2 [1;2;3] [4;5] [] (fun acc x y -> add acc (x + y) = [2 +5;1 + 4 ];; (*add appends at end *)
    ]}
*)

val reduceReverse2U:
  'a t -> 'b t -> 'c -> ('c -> 'a -> 'b ->  'c [@bs]) -> 'c
val reduceReverse2:
  'a t -> 'b t -> 'c -> ('c -> 'a -> 'b ->  'c) -> 'c
(**
   [reduceReverse2 xs ys init f ]

    Applies [f] to each element of [xs] and [ys] from end to beginning. Stops with the shorter list.
    Function [f] has three parameters: an “accumulator” which starts with a value of [init],
    an item from [xs], and an item from [ys]. [reduce2] returns the final value of the accumulator.
    
    @example {[
      reduceReverse2 [1;2;3] [4;5] 0 (fun acc x y -> acc + x * x + y) =  0 + (1 * 1 + 4) + (2 * 2 + 5);;
      reduceReverse2 [1;2;3] [4;5] [] (fun acc x y -> add acc (x + y) = [1 + 4;2 + 5];; (*add appends at end *)
    ]}*)

val everyU: 'a t -> ('a -> bool [@bs]) ->  bool
val every: 'a t -> ('a -> bool ) ->  bool
(** [every xs p]

    @return true if all elements satisfy [p], where [p] is a {i predicate}: a function taking
    an element and returning a [bool].

    @example {[
      every [] (fun x -> x mod 2 = 0) = true;;
      every [2;4;6] (fun x -> x mod 2 = 0 ) = true;;
      every [1;-3;5] (fun x -> x > 0) = false;;
    ]}
*)

val someU: 'a t -> ('a -> bool [@bs]) -> bool
val some: 'a t -> ('a -> bool ) -> bool
(** [some xs p]
    @return true if at least one of the elements in [xs] satifies [p], where [p] is a {i predicate}: a function taking
    an element and returning a [bool].

    @example {[
      some [] (fun x -> x mod 2 = 0) = false ;;
      some [1;2;4] (fun x -> x mod 2 = 0) = true;;
      some [-1;-3;-5] (fun x -> x > 0) = false;;
    ]}
*)

val every2U: 'a t -> 'b t -> ('a -> 'b -> bool [@bs]) -> bool
val every2: 'a t -> 'b t -> ('a -> 'b -> bool ) -> bool
(** [every2 xs ys p] returns true if predicate [p xi yi] is true for all pairs of elements
  up to the shorter length (i.e. [min (length xs) (length ys)])
    @example {[
      every2 [1;2;3] [0;1] (>) = true;;
      every2 [] [1] (fun  x y -> x > y) = true;;
      every2 [2;3] [1] (fun  x y -> x > y) = true;;
      every2 [0;1] [5;0] (fun x y -> x > y) = false;
    ]}
*)

val some2U:  'a t -> 'b t -> ('a -> 'b -> bool [@bs]) -> bool
val some2:  'a t -> 'b t -> ('a -> 'b -> bool) -> bool
(** [some2 xs ys p] returns true if [p xi yi] is true for any pair of elements
  up to the shorter length (i.e. [min (length xs) (length ys)])

    @example {[
      some2 [0;2] [1;0;3] (>) = true ;;
      some2 [] [1] (fun  x y -> x > y) =  false;;
      some2 [2;3] [1;4] (fun  x y -> x > y) = true;;
    ]}
*)

val cmpByLength: 'a t -> 'a t -> int
(** [cmpByLength l1 l2]

    Compare two lists solely by length. Returns -1 if [length l1] is less than [length l2],
    0 if [length l1] equals [length l2], and 1 if [length l1] is greater than [length l2].
    
    @example {[
    cmpByLength [1;2] [3;4;5;6] = -1;;
    cmpByLength [1;2;3] [4;5;6] = 0;;
    cmpByLength [1;2;3;4] [5;6] = 1;;
    ]}

*)
  
val cmpU: 'a t -> 'a t -> ('a -> 'a -> int [@bs]) -> int
val cmp: 'a t -> 'a t -> ('a -> 'a -> int) -> int
(**
    Compare elements one by one [f x y]. [f] returns
      - a negative number if [x] is “less than” [y]
      - zero if [x] is “equal to” [y]
      - a positive number if [x] is “greater than” [y]
    The comparison returns the first non-zero result of [f], or zero if [f] returns zero for all [x] and [y].
    If all items have compared equal, but [xs] is exhausted first, return -1. ([xs] is shorter)
    If all items have compared equal, but [ys] is exhausted first, return 1 ([xs] is longer)
    
    
    @example {[
      cmp [3] [3;7] (fun a b -> compare a b) = -1
      cmp [5;3] [5] (fun a b -> compare a b)  = 1
      cmp [|1; 3; 5|] [|1; 4; 2|] (fun a b -> compare a b) = -1;;
      cmp [|1; 3; 5|] [|1; 2; 3|] (fun a b -> compare a b) = 1;;
      cmp [|1; 3; 5|] [|1; 3; 5|] (fun a b -> compare a b) = 0;;
    ]}

   {b Attention}: The total ordering of List is different from Array,
   for Array, we compare the length first and, only if the lengths are equal, elements one by one.
   For lists, we just compare elements one by one 
*)


val eqU: 'a t -> 'a t -> ('a -> 'a -> bool [@bs]) -> bool
val eq: 'a t -> 'a t -> ('a -> 'a -> bool) -> bool
(**
   [eq xs ys eqElem]
   check equality of [xs] and [ys] using [eqElem] for equality on elements, where [eqElem] is a function
   that returns true if items [x] and [y] meet some criterion for equality, false otherwise.
   [eq] false if length of [xs] and [ys] are not the same.

    @example {[
      eq [1;2;3] [1;2] (=) = false ;;
      eq [1;2] [1;2] (=) = true;;
      eq [1; 2; 3] [-1; -2; -3] (fun a b -> abs a = abs b) = true;;
    ]}
*)  


val hasU:  'a t -> 'b -> ('a -> 'b -> bool [@bs]) -> bool
val has:  'a t -> 'b -> ('a -> 'b -> bool) -> bool
(** 
    [has xs eqFcn] returns true if the list contains at least one element for which [eqFcn x] returns
    true
    @example {[
      has [1;2;3] 2 (=) = true;;
      has [1;2;3] 4 (=) = false;;
      has [-1;-2;-3] 2 (fun a b -> abs a = abs b) = true;;
    ]}
*)

val getByU: 'a t -> ('a -> bool [@bs]) -> 'a option
val getBy: 'a t -> ('a -> bool) -> 'a option
(** [getBy xs p] returns [Some value] for the first value in [xs] that satisifies the predicate function [p]; returns [None] if no element satisifies the function.

  @example {[
      getBy [1;4;3;2] (fun x -> x mod 2 = 0) = Some 4
      getBy [15;13;11] (fun x -> x mod 2 = 0) = None
    ]}
*)
    
val keepU: 'a t ->  ('a -> bool [@bs]) -> 'a t
val keep: 'a t ->  ('a -> bool) -> 'a t
(** [keep  xs p] returns a list of all elements in [xs] which satisfy the predicate function [p]

    @example {[
      keep [1;2;3;4] (fun x -> x mod 2 = 0) =
      [2;4]
    ]}
*)
val keepMapU: 'a t -> ('a -> 'b option [@bs]) -> 'b t
val keepMap: 'a t -> ('a -> 'b option) -> 'b t
(** [keepMap xs f] applies [f] to each element of [xs]. If [f xi] returns [Some value], then [value] is kept in the resulting list; if [f xi] returns [None], the element is not retained in the result.

    @example {[
      keepMap [1;2;3;4] (fun x -> if x mod 2 = 0 then Some (-x ) else None)
      =
      [-2;-4]
    ]}
*)
val partitionU: 'a t -> ('a -> bool [@bs]) ->  'a t * 'a t
val partition: 'a t -> ('a -> bool) ->  'a t * 'a t
(** [partition xs p] creates a pair of lists; the first list consists of all elements of [xs] that satisfy the predicate function [p]; the second list consists of all elements of [xs] that do not satisfy [p]

    @example {[
      partition [1;2;3;4] (fun x -> x mod 2 = 0) =
      ([2;4], [1;3])
    ]}
*)
val unzip: ('a * 'b) t -> 'a t * 'b t
(** [unzip xs] takes a list of pairs and creates a pair of lists. The first list contains all the first items of the pairs; the second list contains all the second items.

    @example {[
      unzip [(1,2) ; (3,4)] = ([1;3], [2;4]);;
      unzip [(1,2) ; (3,4) ; (5,6) ; (7,8)] = ([1;3;5;7], [2;4;6;8]);;
    ]}
*)
val getAssocU: ('a * 'c) t -> 'b ->  ('a -> 'b -> bool [@bs])  -> 'c option
val getAssoc: ('a * 'c) t -> 'b ->  ('a -> 'b -> bool)  -> 'c option
(** [getAssoc xs k eq]
    
    return the second element of a pair in [xs] where the first element equals [x] as per the predicate
    function [eq], or [None] if not found
    @example {[
      getAssoc [ 1, "a"; 2, "b"; 3, "c"] 2 (=) = Some "b"
      getAssoc [9, "morning"; 15, "afternoon"; 22, "night"] 3 (fun a b -> a mod 12 = b mod 12) = Some "afternoon"
    ]}
*)

val hasAssocU: ('a * 'c) t -> 'b -> ('a -> 'b -> bool [@bs]) -> bool
val hasAssoc: ('a * 'c) t -> 'b -> ('a -> 'b -> bool ) -> bool
(** [hasAssoc xs k eq]
     return true if there is a pair in [xs] where the first element equals [k] as per the predicate
     funtion [eq]
    @example {[
      hasAssoc [1, "a"; 2, "b"; 3,"c"] 1 (=) = true;;
      hasAssoc [9, "morning"; 15, "afternoon"; 22, "night"] 3 (fun a b -> a mod 12 = b mod 12) = true;;
    ]}
*)

val removeAssocU:('a * 'c) t -> 'b -> ('a -> 'b -> bool [@bs]) -> ('a * 'c) t
val removeAssoc: ('a * 'c) t -> 'b ->  ('a -> 'b -> bool) -> ('a * 'c) t
(** [removeAssoc xs k eq]
    Return a list after removing the first pair whose first value is [k] per the equality predicate [eq]; if not found, return a new list identical to [xs].
    @example {[
      removeAssoc [1,"a"; 2, "b"; 3, "c" ] 1 (=) =
        [2, "b"; 3, "c"]
      removeAssoc [1,"a"; 2, "b"; 3, "c" ] 99 (=) =
        [1, "a"; 2, "b"; 3, "c"]
    ]}
*)

val setAssocU: ('a * 'c) t -> 'a -> 'c -> ('a -> 'a -> bool [@bs]) -> ('a * 'c) t
val setAssoc: ('a * 'c) t -> 'a -> 'c -> ('a -> 'a -> bool) -> ('a * 'c) t    
(** [setAssoc xs k v eq]
    if [k] exists in [xs] by satisfying the [eq] predicate, return a new list
    with the key and value replaced by the new [k] and [v]; otherwise, return a new
    list with the pair [k, v] added to the head of [xs].
    @example {[
      setAssoc [1,"a"; 2, "b"; 3, "c"] 2 "x" (=) =
      [1,"a"; 2, "x"; 3,"c"] ;; 

      setAssoc [1,"a"; 3, "c"] 2 "b" (=) = 
      [2,"b"; 1,"a"; 3, "c"]
      
      setAssoc [9, "morning"; 3, "morning?!"; 22, "night"] 15 "afternoon"
        (fun a b -> a mod 12 = b mod 12) = [9, "morning"; 15, "afternoon"; 22, "night"]
    ]}
    
    Note carefully the last example! Since [15 mod 12] equals [3 mod 12], {i both} the key and value are
    replaced in the list.
*)    




val sortU: 'a t -> ('a -> 'a -> int [@bs]) -> 'a t
val sort: 'a t -> ('a -> 'a -> int) -> 'a t
(** [sort xs]
    Returns a sorted list.
    @example {[
      sort [5; 4; 9; 3; 7] (fun a b -> a - b) = [3; 4; 5; 7; 9]
    ]}
*)
