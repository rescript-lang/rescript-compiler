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

val get : 'a array -> int -> 'a option

val getExn: 'a array -> int -> 'a  
(** [getExn arr i]
    raise an exception if [i] is out of range
*)

external getUnsafe: 'a array -> int -> 'a = "%array_unsafe_get"
(** [getUnasfe arr i]
    It does not do bounds checking, this would cause type error
    if [i] does not stay within range
*)
external getUndefined: 'a array -> int -> 'a Js.undefined = "%array_unsafe_get"
(** [getUndefined arr i] does the samething in the runtime as [getUnsafe], 
    it is type safe since the return type still track whether it is 
    in range or not
*)

val set : 'a array -> int -> 'a -> unit 
(** [set arr n x] modifies [arr] in place, 
    it replaces the nth element of [arr] with [x] 
    Nothing happens if [n] is out of range
*)

val setExn: 'a array -> int -> 'a -> unit 
(** [setExn arr i x]
    raise an exception if [i] is out of range
*)

external setUnsafe: 'a array -> int -> 'a -> unit = "%array_unsafe_set"

val shuffleInPlace: 'a array -> unit

val shuffle: 'a array -> 'a array  
(** [shuffle xs] returns a new array *)

val reverseInPlace: 'a array -> unit

val reverse: 'a array -> 'a array
(** [reverse x] returns a new array *)

external makeUninitialized: int -> 'a Js.undefined array = "Array" [@@bs.new]

external makeUninitializedUnsafe: int -> 'a array = "Array" [@@bs.new]

val make: int -> 'a  -> 'a array
(** [make n e] 
    
    return an empty array when [n] is negative.
    return an array of size [n] with value [e]
 *)

val makeBy: int -> (int -> 'a [@bs]) -> 'a array
(** [makeBy n f] 
    
    return an empty array when [n] is negative 
    return an array of size [n] populated by [f i] start from [0] to [n - 1]
*)

val makeByAndShuffle: int -> (int -> 'a [@bs]) -> 'a array
(** [makeByAndShuffle n f] is semantically equivalent to [makeBy n f]
    and return the shuffled array  *)    


val zip: 'a array -> 'b array -> ('a * 'b) array
(** [zip a b] 
    
    stop with the shorter array    
 *)


 val zipBy: 'a array -> 'b array -> ('a -> 'b -> 'c [@bs]) -> 'c array    
 (**
    [zipBy a b f]
   
    stops with shorter array    
 *)

val concat: 'a array -> 'a array -> 'a array
(** [concat xs ys]

    return a fresh array containing the
    concatenation of the arrays [v1] and [v2], so even if [v1] or [v2]
    is empty, it can not be shared 
*)

val concatMany: 'a array array -> 'a array
(**
    [concatMany xss]

    return a fresh array as the concatenation of [xss]
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

    returns a copy of [a], that is, a fresh array
   containing the same elements as [a]. *)

val fill: 'a array -> offset:int -> len:int -> 'a -> unit
(** [fill arr ofs len x] 

    modifies [arr] in place,
    storing [x] in elements number [ofs] to [ofs + len - 1].

    [offset] can be negative

    [fill arr offset:(-1) len:1 ] means fill the last element,
    if the array does not have enough data, [fill] will ignore it
 *)

val blit: 
    src:'a array -> srcOffset:int -> dst:'a array -> dstOffset:int -> len:int -> unit
(** [blit v1 o1 v2 o2 len] 

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


val forEach: 'a array ->  ('a -> unit [@bs]) -> unit

val map: 'a array ->  ('a -> 'b [@bs]) -> 'b array

val keep: 'a array -> ('a -> bool [@bs]) -> 'a array

val keepMap: 'a array -> ('a -> 'b option [@bs]) -> 'b array 
    
val forEachWithIndex: 'a array ->  (int -> 'a -> unit [@bs]) -> unit

val mapWithIndex: 'a array ->  (int -> 'a -> 'b [@bs]) -> 'b array

val reduce:  'b array -> 'a -> ('a -> 'b -> 'a [@bs]) ->'a

val reduceReverse: 'b array -> 'a -> ('a -> 'b ->  'a [@bs]) ->  'a

val every: 'a array -> ('a -> bool [@bs]) -> bool


val every2: 'a array -> 'b array -> ('a -> 'b -> bool [@bs]) -> bool
(** [every2 a b p] 
    - return false when [length a <> length b] 
    - return true when every pair is true [f ai bi]
*)

val cmp: 'a array -> 'a array -> ('a -> 'a -> int [@bs]) -> int
(** [cmp a b]
    
    if [length a <> length b] compared by length
    otherwise compare one by one [f ai bi]
*)

val eq:  'a array -> 'a array -> ('a -> 'a -> bool [@bs]) -> bool
(** [eq a b]
    
    - return false if length is not the same
    - equal one by one using [f ai bi]
*)

external truncateToLengthUnsafe: 'a array -> int ->  unit = "length" [@@bs.set]
