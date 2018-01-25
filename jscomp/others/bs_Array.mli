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


external length : 'a array -> int = "%array_length" 

val get : 'a array -> int -> 'a option

val set : 'a array -> int -> 'a -> unit 
(** [set a n x] modifies array [a] in place, replacing
    element number [n] with [x].
    Nothing happens if [n] is out of range
*)

external getUnsafe: 'a array -> int -> 'a = "%array_unsafe_get"
external getUndefined: 'a array -> int -> 'a Js.undefined = "%array_unsafe_get"
val getExn: 'a array -> int -> 'a  
external setUnsafe: 'a array -> int -> 'a -> unit = "%array_unsafe_set"

val shuffle: 'a array -> unit
val reverse: 'a array -> unit
val reverseCopy: 'a array -> 'a array
external makeUninitialized: int -> 'a Js.undefined array = "Array" [@@bs.new]
external makeUninitializedUnsafe: int -> 'a array = "Array" [@@bs.new]

val make: int -> 'a  -> 'a array
(** [make n e] return an empty array when [n] is negative *)
val makeBy: int -> (int -> 'a [@bs]) -> 'a array
(** [makeBy n f] return an empty array when [n] is negative *)
val makeByAndShuffle: int -> (int -> 'a [@bs]) -> 'a array
(** [makeByAndShuffle n f] is semantically equivalent to [makeBy n f]
    and return the shuffled array  *)    


val zip: 'a array -> 'b array -> ('a * 'b) array
(** [zip a b] stop with the shorter array *)



val concat: 'a array -> 'a array -> 'a array
(** Note it returns a fresh array containing the
    concatenation of the arrays [v1] and [v2], so even if [v1] or [v2]
    is empty, it can not be shared 
*)

val concatMany: 'a array array -> 'a array


val slice: 'a array -> offset:int -> len:int -> 'a array
(** [slice arr offset len]
    [offset] can be negative,
    [slice arr -1 1] means get the last element as a singleton array,
    [slice arr -(very_large_index) len] will do a copy of the array
    if the array does not have enough data, [slice] extracts through
    the end of sequence
*)


val copy: 'a array -> 'a array
(** [copy a] returns a copy of [a], that is, a fresh array
   containing the same elements as [a]. *)

val fill: 'a array -> offset:int -> len:int -> 'a -> unit
(** [fill arr ofs len x] modifies the array [arr] in place,
    storing [x] in elements number [ofs] to [ofs + len - 1].

    [offset] can be negative,
    [fill arr offset:(-1) len:1 ] means fill the last element,
    if the arry does not have enogh data, [fill] will ignore it
 *)

val blit: 
    src:'a array -> srcOffset:int -> dst:'a array -> dstOffset:int -> len:int -> unit
(** [blit v1 o1 v2 o2 len] copies [len] elements
   from array [v1], starting at element number [o1], to array [v2],
   starting at element number [o2]. It works correctly even if
   [v1] and [v2] are the same array, and the source and
   destination chunks overlap.

    return false means the input is invalid, the array is unchnaged
*)
val blitUnsafe:
  src:'a array -> srcOffset:int -> dst:'a array -> dstOffset:int -> len:int -> unit 

val toList: 'a array -> 'a list


val ofList: 'a list -> 'a array

val forEach: 'a array ->  ('a -> unit [@bs]) -> unit

val map: 'a array ->  ('a -> 'b [@bs]) -> 'b array

val map2: 'a array -> 'b array -> ('a -> 'b -> 'c [@bs]) -> 'c array    

val keepBy: 'a array -> ('a -> bool [@bs]) -> 'a array

val keepMap: 'a array -> ('a -> 'b option [@bs]) -> 'b array 
    
val forEachWithIndex: 'a array ->  (int -> 'a -> unit [@bs]) -> unit

val mapWithIndex: 'a array ->  (int -> 'a -> 'b [@bs]) -> 'b array

val reduce:  'b array -> 'a -> ('a -> 'b -> 'a [@bs]) ->'a

val reduceReverse: 'b array -> 'a -> ('a -> 'b ->  'a [@bs]) ->  'a

val every: 'a array -> ('a -> bool [@bs]) -> bool

(** [every2 a b] return false when [length a <> length b] *)
val every2: 'a array -> 'b array -> ('a -> 'b -> bool [@bs]) -> bool

val cmp: 'a array -> 'a array -> ('a -> 'a -> int [@bs]) -> int
val eq:  'a array -> 'a array -> ('a -> 'a -> bool [@bs]) -> bool
  
external truncateToLengthUnsafe: 'a array -> int ->  unit = "length" [@@bs.set]
