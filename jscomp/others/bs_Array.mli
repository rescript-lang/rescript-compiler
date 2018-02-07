(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)
(* Adapted significantly by Authors of BuckleScript *)


(** {!Bs.Array}    
    Utililites for Array functions 
*)


external length: 'a array -> int = "%array_length" 
external size: 'a array -> int = "%array_length"
(** [size arr] is the same as [length arr] *)

val get: 'a array -> int -> 'a option

val getExn: 'a array -> int -> 'a  
(** [getExn arr i]

    {b raise} an exception if [i] is out of range
*)

external getUnsafe: 'a array -> int -> 'a = "%array_unsafe_get"
(** [getUnasfe arr i]

    {b Unsafe}

    no  bounds checking, this would cause type error
    if [i] does not stay within range
*)
external getUndefined: 'a array -> int -> 'a Js.undefined = "%array_unsafe_get"
(** [getUndefined arr i]

    It does the samething in the runtime as {!getUnsafe}, 
    it is {i type safe} since the return type still track whether it is 
    in range or not
*)

val set: 'a array -> int -> 'a -> bool
(** [set arr n x] modifies [arr] in place, 
    it replaces the nth element of [arr] with [x] 
    @return false means not updated due to out of range
*)

val setExn: 'a array -> int -> 'a -> unit 
(** [setExn arr i x]
    {b raise} an exception if [i] is out of range
*)

external setUnsafe: 'a array -> int -> 'a -> unit = "%array_unsafe_set"

val shuffleInPlace: 'a array -> unit

val shuffle: 'a array -> 'a array  
(** [shuffle xs]
    @return a fresh array *)

val reverseInPlace: 'a array -> unit

val reverse: 'a array -> 'a array
(** [reverse x]
    @return a fresh array *)

external makeUninitialized: int -> 'a Js.undefined array = "Array" [@@bs.new]

external makeUninitializedUnsafe: int -> 'a array = "Array" [@@bs.new]
(** [makeUninitializedUnsafe n]

    {b Unsafe}   
*)


val make: int -> 'a  -> 'a array
(** [make n e] 
    return an array of size [n] filled  with value [e]    
    @return an empty array when [n] is negative.
*)

val range: int -> int -> int array
(** [range start finish] create an inclusive array
    @example {[
      range 0 3 =  [|0;1;2;3|];;
      range 3 0 =  [||] ;;
      range 3 3 = [|3|];;
    ]}
*)
val rangeBy: int -> int -> step:int -> int array
(** [rangeBy start finish ~step]

    @return empty array when step is 0 or negative
    it also return empty array when [start > finish]
    
    @example {[
     rangeBy 0 10 ~step:3 = [|0;3;6;9|];;
     rangeBy 0 12 ~step:3 = [|0;3;6;9;12|];;
     rangeBy 33 0 ~step:1 =  [||];;
     rangeBy 33 0 ~step:(-1) = [||];;
     rangeBy 3 12 ~step:(-1) = [||];;
     rangeBy 3 3 ~step:0 = [||] ;;     
     rangeBy 3 3 ~step:(1) = [|3|] ;;
   ]}
*)    

val makeByU: int -> (int -> 'a [@bs]) -> 'a array
val makeBy: int -> (int -> 'a ) -> 'a array
(** [makeBy n f] 
    
    return an empty array when [n] is negative 
    return an array of size [n] populated by [f i] start from [0] to [n - 1]
*)

val makeByAndShuffleU: int -> (int -> 'a [@bs]) -> 'a array
val makeByAndShuffle: int -> (int -> 'a ) -> 'a array    
(** [makeByAndShuffle n f] is semantically equivalent to [makeBy n f]
    and return the shuffled array  *)    


val zip: 'a array -> 'b array -> ('a * 'b) array
(** [zip a b] 
    
    stop with the shorter array

    @example {[
      zip [|1;2] [|1;2;3|] = [| (1,2); (2;2)|]
    ]}
 *)


 val zipByU: 'a array -> 'b array -> ('a -> 'b -> 'c [@bs]) -> 'c array
 val zipBy: 'a array -> 'b array -> ('a -> 'b -> 'c ) -> 'c array         
 (**
    [zipBy a b f]
   
    stops with shorter array
 *)

val concat: 'a array -> 'a array -> 'a array
(** [concat xs ys]

    @return a fresh array containing the
    concatenation of the arrays [v1] and [v2], so even if [v1] or [v2]
    is empty, it can not be shared 
*)

val concatMany: 'a array array -> 'a array
(**
    [concatMany xss]

    @return a fresh array as the concatenation of [xss]
*)

val slice: 'a array -> offset:int -> len:int -> 'a array
(** [slice arr offset len]
    
    [offset] can be negative,
    [slice arr -1 1] means get the last element as a singleton array

    [slice arr -(very_large_index) len] will do a copy of the array
    
    if the array does not have enough data, [slice] extracts through
    the end of sequence
*)


val copy: 'a array -> 'a array
(** [copy a] 

    @return a copy of [a], that is, a fresh array
   containing the same elements as [a]. *)

val fill: 'a array -> offset:int -> len:int -> 'a -> unit
(** [fill arr ~offset ~len x] 

    modifies [arr] in place,
    storing [x] in elements number [offset] to [offset + len - 1].

    [offset] can be negative

    [fill arr offset:(-1) len:1 ] means fill the last element,
    if the array does not have enough data, [fill] will ignore it
 *)

val blit: 
    src:'a array -> srcOffset:int -> dst:'a array -> dstOffset:int -> len:int -> unit
(** [blit ~src:v1 ~srcOffset:o1 ~dst:v2 ~dstOffset:o2 ~len] 

    copies [len] elements
   from array [v1], starting at element number [o1], to array [v2],
   starting at element number [o2]. 
   
    It works correctly even if
    [v1] and [v2] are the same array, and the source and
    destination chunks overlap.

    [offset] can be negative, [-1] means [len - 1], if [len + offset]  is still 
    negative, it will be set as 0   
*)

val blitUnsafe:
  src:'a array -> srcOffset:int -> dst:'a array -> dstOffset:int -> len:int -> unit 


val forEachU: 'a array ->  ('a -> unit [@bs]) -> unit
val forEach: 'a array ->  ('a -> unit ) -> unit

val mapU: 'a array ->  ('a -> 'b [@bs]) -> 'b array
val map: 'a array ->  ('a -> 'b ) -> 'b array

val keepU: 'a array -> ('a -> bool [@bs]) -> 'a array
val keep: 'a array -> ('a -> bool ) -> 'a array
(** [keep xs p ]
    @return a new array that keep all elements satisfy [p]
*)

val keepMapU: 'a array -> ('a -> 'b option [@bs]) -> 'b array
val keepMap: 'a array -> ('a -> 'b option) -> 'b array 
(** [keepMap xs p]
    @return a new array that keep all elements that return a non-None applied [p]

    @example {[
      keepMap [|1;2;3|] (fun x -> if x mod 2 then Some x else None)
      = [| 2 |]
    ]}
*)

val forEachWithIndexU: 'a array ->  (int -> 'a -> unit [@bs]) -> unit
val forEachWithIndex: 'a array ->  (int -> 'a -> unit ) -> unit
(** [forEachWithIndex xs f]

    The same with {!forEach}, except that [f] is supplied with one
    more argument: the index starting from 0
*)

val mapWithIndexU: 'a array ->  (int -> 'a -> 'b [@bs]) -> 'b array
val mapWithIndex: 'a array ->  (int -> 'a -> 'b ) -> 'b array    
(** [mapWithIndex xs f ]

    The same with {!map} except that [f] is supplied with one
    more argument: the index starting from 0
*)
    
val reduceU:  'b array -> 'a -> ('a -> 'b -> 'a [@bs]) ->'a
val reduce:  'b array -> 'a -> ('a -> 'b -> 'a ) ->'a
(** [reduce xs init f]

    @example {[
      reduce [|2;3;4|] 1 (+) = 10
    ]}
   
*)

val reduceReverseU: 'b array -> 'a -> ('a -> 'b ->  'a [@bs]) ->  'a
val reduceReverse: 'b array -> 'a -> ('a -> 'b ->  'a ) ->  'a
(** [reduceReverse xs init f]
    @example {[
      reduceReverse [|1;2;3;4|] 100 (-) = 90 
    ]}
*)

val someU: 'a array -> ('a -> bool [@bs]) -> bool
val some: 'a array -> ('a -> bool) -> bool
(** [some xs p]
    @return true if one of element satifies [p]
*)
  
val everyU: 'a array -> ('a -> bool [@bs]) -> bool
val every: 'a array -> ('a -> bool ) -> bool
(** [every xs p]
    @return true if all elements satisfy [p]
*)
  
val every2U: 'a array -> 'b array -> ('a -> 'b -> bool [@bs]) -> bool
val every2: 'a array -> 'b array -> ('a -> 'b -> bool ) -> bool
(** [every2 xs ys p] only tests the length of shorter

    @example {[
      every2 [|1;2;3|] [|0;1|] (>) = true;;
      (every2 [||] [|1|] (fun   x y -> x > y)) = true;;
      (every2 [|2;3|] [|1|] (fun   x y -> x > y)) = true;; 
    ]}
*)

val some2U: 'a array -> 'b array -> ('a -> 'b -> bool [@bs]) -> bool
val some2: 'a array -> 'b array -> ('a -> 'b -> bool ) -> bool
(** [some2 xs ys p] only tests the length of shorter

    @example {[
      some2 [|0;2|] [|1;0;3|] (>) = true ;;
      (some2 [||] [|1|] (fun   x y -> x > y)) =  false;;
      (some2 [|2;3|] [|1;4|] (fun   x y -> x > y)) = true;;
    ]}
*)
  
val cmpU: 'a array -> 'a array -> ('a -> 'a -> int [@bs]) -> int
val cmp: 'a array -> 'a array -> ('a -> 'a -> int ) -> int
(** [cmp a b]
    
    - compared by length if [length a <> length b] 
    - otherwise compare one by one [f ai bi]
*)

val eqU:  'a array -> 'a array -> ('a -> 'a -> bool [@bs]) -> bool
val eq:  'a array -> 'a array -> ('a -> 'a -> bool ) -> bool
(** [eq a b]
    
    - return false if length is not the same
    - equal one by one using [f ai bi]
*)

external truncateToLengthUnsafe: 'a array -> int ->  unit = "length" [@@bs.set]
(** {b Unsafe} function *)
