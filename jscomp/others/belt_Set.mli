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

(** A {i immutable} sorted set module which allows customize {i compare} behavior.

   The implementation uses balanced binary trees, and therefore searching
   and insertion take time logarithmic in the size of the map.

   All data are parameterized by not its only type but also a unique identity in
   the time of initialization, so that two {i Sets of ints} initialized with different
   {i compare} functions will have different type.

   For example:
   {[
     type t = int * int 
      module I0 =
        (val Belt.Id.comparableU ~cmp:(fun[\@bs] ((a0,a1) : t) ((b0,b1) : t) ->
             match Pervasives.compare a0 b0 with
             | 0 -> Pervasives.compare a1 b1
             | c -> c 
           ))
    let s0 = make ~id:(module I0)
    module I1 =
      (val Belt.Id.comparableU ~cmp:(fun[\@bs] ((a0,a1) : t) ((b0,b1) : t) ->
           match compare a1 b1 with
           | 0 -> compare a0 b0
           | c -> c 
         ))
    let s1 = make ~id:(module I1)
   ]}


   Here the compiler would infer [s0] and [s1] having different type so that
    it would not mix.

   {[
     val s0 :  ((int * int), I0.identity) t
     val s1 :  ((int * int), I1.identity) t
   ]}

   We can add elements to the collection:

   {[

     let s2 = add s1 (0,0)
     let s3 = add s2 (1,1)
   ]}

   Since this is an immutable data strucure, [s1] will be an empty set
   while [s2] will contain one element, [s3] will contain two.

   The [union s0 s3] will result in a type error, since their identity mismatch
*)

(** Specalized when key type is [int], more efficient
    than the gerneic type, its compare behavior is fixed using the built-in comparison
*)
module Int = Belt_SetInt

(** Specalized when key type is [string], more efficient
    than the gerneic type, its compare behavior is fixed using the built-in comparison
*)  
module String = Belt_SetString


(** This module seprate identity from data, it is a bit more verbsoe but slightly
    more efficient due to the fact that there is no need to pack identity and data back
    after each operation
*)  
module Dict = Belt_SetDict


type ('key,'identity)  t
(** [('key, 'identity) t]

    ['key] is the element type

    ['identity] the identity of the collection
*)


type ('key, 'id) id = ('key, 'id) Belt_Id.comparable
(** The identity needed for making a set from scratch
*)

val make: id:('elt, 'id) id -> ('elt, 'id) t
(** [make ~id]
   
     @example {[
     module IntCmp = (val IntCmp.comparable ~cmp:(fun (x:int) y -> Pervasives.comapre x y))
     let s = make ~id:(module IntCmp)
    ]}

*)


val ofArray:  'k array -> id:('k, 'id) id ->  ('k, 'id) t
(** [ofArray xs ~id]

    @example{[
     module IntCmp = (val IntCmp.comparableU
                         ~cmp:(fun[\@bs]
                                (x:int) y -> Pervasives.comapre x y));;
     toArray (ofArray [1;3;2;4] (module IntCmp)) = [1;2;3;4]
      
    ]}
*)


val ofSortedArrayUnsafe: 'elt array -> id:('elt, 'id) id -> ('elt,'id) t
(** [ofSortedArrayUnsafe xs ~id]

    The same as {!ofArray} except it is after assuming the input array [x] is already sorted

    {b Unsafe} 
*)


val isEmpty: _ t -> bool
(**
   @example {[
     module IntCmp =
       (val IntCmp.comparableU
           ~cmp:(fun[\@bs]
                  (x:int) y -> Pervasives.comapre x y));;     
     isEmpty (ofArray [||] ~id:(module IntCmp)) = true;;
     isEmpty (ofArray [|1|] ~id:(module IntCmp)) = true;;  
   ]}
*)
val has: ('elt, 'id) t -> 'elt ->  bool
(**
   @example {[
     module IntCmp =
       (val IntCmp.comparableU
           ~cmp:(fun[\@bs]
                  (x:int) y -> Pervasives.comapre x y));;
     let v = ofArray [|1;4;2;5|] ~id:(module IntCmp);;
     has v 3 = false;;
     has v 1 = true;;
   ]}
*)

val add:   
  ('elt, 'id) t -> 'elt -> ('elt, 'id) t
(** [add s x] If [x] was already in [s], [s] is returned unchanged.

    @example {[
     module IntCmp =
       (val IntCmp.comparableU
           ~cmp:(fun[\@bs]
                  (x:int) y -> Pervasives.comapre x y));;
     let s0 = make ~id:(module IntCmp);;
     let s1 = add s0 1 ;;
     let s2 = add s1 2;;
     let s3 = add s2 2;;
     toArray s0 = [||];;
     toArray s1 = [|1|];;
     toArray s2 = [|1;2|];;
     toArray s3 = [|1;2|];;
     s2 == s3;;
    ]}
*)
    
val mergeMany: ('elt, 'id) t -> 'elt array -> ('elt, 'id) t 
(** [mergeMany s xs]

    Adding each of [xs] to [s], note unlike {!add},
    the reference of return value might be changed even if all values in [xs]
    exist [s]
   
*)
val remove: ('elt, 'id) t -> 'elt -> ('elt, 'id) t
(** [remove m x] If [x] was not in [m], [m] is returned reference unchanged.

    @example {[
      module IntCmp =
       (val IntCmp.comparableU
           ~cmp:(fun[\@bs]
                  (x:int) y -> Pervasives.comapre x y));;
      let s0 = ofArray ~id:(module IntCmp) [|2;3;1;4;5|];;
      let s1 = remove s0 1 ;;
      let s2 = remove s1 3 ;;
      let s3 = remove s2 3 ;;

      toArray s1 = [|2;3;4;5|];;
      toArray s2 = [|2;4;5|];;
      s2 == s3;; 
    ]}
*)

val removeMany:
  ('elt, 'id) t -> 'elt array -> ('elt, 'id) t
(** [removeMany s xs]

    Removing each of [xs] to [s], note unlike {!remove},
    the reference of return value might be changed even if none in [xs]
    exists [s]
*)    

val union: ('elt, 'id) t -> ('elt, 'id) t -> ('elt, 'id) t
(**
   [union s0 s1]

   @example {[
     module IntCmp =
       (val IntCmp.comparableU
           ~cmp:(fun[\@bs]
                  (x:int) y -> Pervasives.comapre x y));;
     let s0 = ofArray ~id:(module IntCmp) [|5;2;3;5;6|]];;
     let s1 = ofArray ~id:(module IntCmp) [|5;2;3;1;5;4;|];;
     toArray (union s0 s1) =  [|1;2;3;4;5;6|]
   ]}
*)

val intersect: ('elt, 'id) t -> ('elt, 'id) t -> ('elt, 'id) t
(** [intersect s0 s1]
   @example {[
     module IntCmp =
       (val IntCmp.comparableU
           ~cmp:(fun[\@bs]
                  (x:int) y -> Pervasives.comapre x y));;
     let s0 = ofArray ~id:(module IntCmp) [|5;2;3;5;6|]];;
     let s1 = ofArray ~id:(module IntCmp) [|5;2;3;1;5;4;|];;
     toArray (intersect s0 s1) =  [|2;3;5|]
   ]}

*)    
val diff: ('elt, 'id) t -> ('elt, 'id) t -> ('elt, 'id) t
(** [diff s0 s1]
    @example {[
      module IntCmp =
        (val IntCmp.comparableU
            ~cmp:(fun[\@bs]
                   (x:int) y -> Pervasives.comapre x y));;
      let s0 = ofArray ~id:(module IntCmp) [|5;2;3;5;6|]];;
      let s1 = ofArray ~id:(module IntCmp) [|5;2;3;1;5;4;|];;
      toArray (diff s0 s1) = [|6|];;
      toArray (diff s1 s0) = [|1;4|];;
    ]}
*)

val subset: ('elt, 'id) t -> ('elt, 'id) t -> bool     
(** [subset s0 s1]

    @example {[
      module IntCmp =
        (val IntCmp.comparableU
            ~cmp:(fun[\@bs]
                   (x:int) y -> Pervasives.comapre x y));;
      let s0 = ofArray ~id:(module IntCmp) [|5;2;3;5;6|]];;
      let s1 = ofArray ~id:(module IntCmp) [|5;2;3;1;5;4;|];;
      let s2 = intersect s0 s1;;
      subset s2 s0 = true;;
      subset s2 s1 = true;;
      subset s1 s0 = false;;
    ]}
*)
  
val cmp: ('elt, 'id) t -> ('elt, 'id) t -> int
(** Total ordering between sets. Can be used as the ordering function
    for doing sets of sets.
    It compare [size] first and then iterate over
    each element following the order of elements
*)
  
val eq: ('elt, 'id) t -> ('elt, 'id) t -> bool
(** [eq s0 s1]

    @return true if [toArray s0 = toArray s1] 
*)
  
val forEachU: ('elt, 'id) t -> ('elt -> unit [@bs]) ->  unit
val forEach: ('elt, 'id) t -> ('elt -> unit ) ->  unit  
(** [forEach s f] applies [f] in turn to all elements of [s].
    In increasing order

    @example {[
      module IntCmp =
        (val IntCmp.comparableU
            ~cmp:(fun[\@bs]
                   (x:int) y -> Pervasives.comapre x y));;
      let s0 = ofArray ~id:(module IntCmp) [|5;2;3;5;6|]];;
      let acc = ref [] ;;
      forEach s0 (fun x -> acc := x !acc);;
      !acc = [6;5;3;2];;
    ]}
*)

val reduceU: ('elt, 'id) t -> 'a  -> ('a -> 'elt -> 'a [@bs]) ->  'a
val reduce: ('elt, 'id) t -> 'a  -> ('a -> 'elt -> 'a ) ->  'a  
(** In increasing order.

    @example {[
      module IntCmp =
        (val IntCmp.comparableU
            ~cmp:(fun[\@bs]
                   (x:int) y -> Pervasives.comapre x y));;
      let s0 = ofArray ~id:(module IntCmp) [|5;2;3;5;6|]];;
      reduce s0 [] Bs.List.add = [6;5;3;2];;
    ]}
*)

val everyU: ('elt, 'id) t -> ('elt -> bool [@bs]) -> bool
val every: ('elt, 'id) t -> ('elt -> bool ) -> bool  
(** [every p s] checks if all elements of the set
    satisfy the predicate [p]. Order unspecified.
*)

val someU: ('elt, 'id) t ->  ('elt -> bool [@bs]) -> bool
val some: ('elt, 'id) t ->  ('elt -> bool ) -> bool  
(** [some p s] checks if at least one element of
    the set satisfies the predicate [p]. *)

val keepU: ('elt, 'id) t ->  ('elt -> bool [@bs]) -> ('elt, 'id) t
val keep: ('elt, 'id) t ->  ('elt -> bool ) -> ('elt, 'id) t    
(** [keep m p] returns the set of all elements in [s]
    that satisfy predicate [p]. *)
    
val partitionU: ('elt, 'id) t -> ('elt -> bool [@bs]) ->  ('elt, 'id) t * ('elt, 'id) t
val partition: ('elt, 'id) t -> ('elt -> bool) ->  ('elt, 'id) t * ('elt, 'id) t
(** [partition m p] returns a pair of sets [(s1, s2)], where
    [s1] is the set of all the elements of [s] that satisfy the
    predicate [p], and [s2] is the set of all the elements of
    [s] that do not satisfy [p]. *)

val size:  ('elt, 'id) t -> int
(** [size s]

    @example {[
      module IntCmp =
        (val IntCmp.comparableU
            ~cmp:(fun[\@bs]
                   (x:int) y -> Pervasives.comapre x y));;
      let s0 = ofArray ~id:(module IntCmp) [|5;2;3;5;6|]];;
      size s0 = 4;;
    ]}
*)
  
val toArray: ('elt, 'id) t -> 'elt array
(** [toArray s0]
   @example {[
      module IntCmp =
        (val IntCmp.comparableU
            ~cmp:(fun[\@bs]
                   (x:int) y -> Pervasives.comapre x y));;
      let s0 = ofArray ~id:(module IntCmp) [|5;2;3;5;6|]];;
      toArray s0 = [|2;3;5;6|];;
    ]}*)
    
val toList: ('elt, 'id) t -> 'elt list
(** In increasing order

    {b See} {!toArray}
*)

val minimum: ('elt, 'id) t -> 'elt option
(** [minimum s0]

    @return the minimum element of the collection, [None] if it is empty
*)    

val minUndefined: ('elt, 'id) t -> 'elt Js.undefined
(** [minUndefined s0]

   @return the minimum element of the collection, [undefined] if it is empty
*)    

val maximum: ('elt, 'id) t -> 'elt option
(** [maximum s0]

    @return the maximum element of the collection, [None] if it is empty
*)    
val maxUndefined: ('elt, 'id) t -> 'elt Js.undefined
(** [maxUndefined s0]

    @return the maximum element of the collection, [undefined] if it is empty
*)    

val get: ('elt, 'id) t -> 'elt -> 'elt option
(** [get s0 k]

    @return the reference of the value [k'] which is equivalent to [k]
    using  the comparator specifiecd by this collection, [None]
    if it does not exist
*)

val getUndefined: ('elt, 'id) t -> 'elt -> 'elt Js.undefined
(** {b See} {!get}
    *)    

val getExn: ('elt, 'id) t -> 'elt -> 'elt
(** {b See} {!get}

    {b raise} if not exist
*)  

val split: ('elt, 'id) t -> 'elt -> (('elt, 'id) t  * ('elt, 'id) t) * bool
(** [split set ele]

    @return  a tuple [((smaller, larger), present)],
    [present] is true when [ele] exist in [set]
*)

(**/**)
val checkInvariantInternal: _ t -> unit
(**
   {b raise} when invariant is not held
*)  
(**/**)

(****************************************************************************)
(** Below are operations only when better performance needed,
    it is still safe API but more verbose.
    More API will be exposed by needs
*)

val getData: ('k,'id) t  -> ('k,'id) Belt_SetDict.t
(** [getData s0]

    {b Advanced usage only}
    
    @return the raw data (detached from comparator),
    but its type is still manifested, so that user can pass identity directly
    without boxing
*)
    
val getId: ('k,'id) t  -> ('k,'id) id
(** [getId s0]

    {b Advanced usage only}
    
    @return the identity of [s0]
*)

val packIdData: id:('k, 'id) id -> data:('k, 'id) Belt_SetDict.t -> ('k, 'id) t
(** [packIdData ~id ~data]

    {b Advanced usage only}
    
    @return the packed collection
*)    
    
