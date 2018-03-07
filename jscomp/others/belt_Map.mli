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
(*  Adapted by authors of BuckleScript without using functors          *)
(***********************************************************************)

(** A {i immutable} sorted map module which allows customize {i compare} behavior.

   The implementation uses balanced binary trees, and therefore searching
   and insertion take time logarithmic in the size of the map.

   All data are parameterized by not its only type but also a unique identity in
   the time of initialization, so that two {i Map of int keys} initialized with different
   {i compare} functions will have different type.

   For example:
   {[
     type t = int * int 
      module I0 =
        (val Belt.Id.comparableU (fun[\@bs] ((a0,a1) : t) ((b0,b1) : t) ->
             match Pervasives.compare a0 b0 with
             | 0 -> Pervasives.compare a1 b1
             | c -> c 
           ))
    let s0 : (_, string, _) t = make ~id:(module I0) (* value is of type string *)
    module I1 =
      (val Belt.Id.comparableU (fun[\@bs] ((a0,a1) : t) ((b0,b1) : t) ->
           match compare a1 b1 with
           | 0 -> compare a0 b0
           | c -> c 
         ))
    let s1 : (_, string, _) t = make ~id:(module I1)
   ]}


   Here the compiler would infer [s0] and [s1] having different type so that
    it would not mix.

   {[
     val s0 : ((int * int), string,  I0.identity) t 
     val s1 : ((int * int), string,  I1.identity) t
   ]}

   We can add elements to the collection:

   {[

     let s2 = add s1 (0,0) "a"
     let s3 = add s2 (1,1) "b"
   ]}

   Since this is an immutable data strucure, [s1] will be an empty map
   while [s2] will contain one pair, [s3] will contain two.

   The [merge s0 s3 callback] will result in a type error, since their identity mismatch
*)


(** Specalized when key type is [int], more efficient
    than the gerneic type, its compare behavior is fixed using the built-in comparison
*)
module Int = Belt_MapInt

(** specalized when key type is [string], more efficient
    than the gerneic type, its compare behavior is fixed using the built-in comparison *)  
module String = Belt_MapString

(** This module seprate identity from data, it is a bit more verbsoe but slightly
    more efficient due to the fact that there is no need to pack identity and data back
    after each operation

    {b Advanced usage only}
*)  
module Dict = Belt_MapDict


type ('key, 'value, 'identity) t
(** [('key, 'identity) t]

    ['key] is the field type

    ['value] is the element type

    ['identity] the identity of the collection
*)


type ('key, 'id) id = ('key, 'id) Belt_Id.comparable
(** The identity needed for making an empty map*)


(*
    How we retain soundness:
    The only way to create a value of type [_ t] from scratch 
    is through [empty] which requires [_ Belt_Id.t]
    The only way to create [_ Belt_Id.t] is using [Belt_Id.Make] which
    will create a fresh type [id] per module

    Generic operations over tree without [cmp] are still exported 
    (for efficient reasons) so that [data] does not need be boxed and unboxed.

    The soundness is guaranteed in two aspects:
    When create a value of [_ t] it needs both [_ Belt_Id.t] and [_ t0].
    [_ Belt_Id.t] is an abstract type. Note [add0] requires [_ Belt_Id.cmp] which 
    is also an abstract type which can only come from [_ Belt_Id.t]

    When destructing a value of [_ t], the ['id] parameter is threaded.

*)

(* should not export [Belt_Id.compare]. 
   should only export [Belt_Id.t] or [Belt_Id.cmp] instead *)


val make: id:('k, 'id) id -> ('k, 'v, 'id) t
(** [make ~id]
   
     @example {[
     module IntCmp = (val Belt.Id.comparable (fun (x:int) y -> Pervasives.compare x y));;
     let s = make ~id:(module IntCmp)
    ]}

*)


val isEmpty: _ t -> bool
(** [isEmpty s0]
    @example {[
      module IntCmp = (val Belt.Id.comparable (fun (x:int) y -> Pervasives.compare x y));;
      isEmpty (fromArray [|1,"1"|] ~id:(module IntCmp)) = false;;
    ]}
*)

val has: ('k, 'v, 'id) t -> 'k  -> bool
(** [has s k]

    @example {[
      module IntCmp = (val Belt.Id.comparable (fun (x:int) y -> Pervasives.compare x y));;
      has (fromArray [|1,"1"|] ~id:(module IntCmp)) 1 = true;;
    ]}
*)  

val cmpU:
    ('k, 'v, 'id) t ->
    ('k, 'v, 'id) t ->
    ('v -> 'v -> int [@bs]) ->
     int
val cmp:
    ('k, 'v, 'id) t ->
    ('k, 'v, 'id) t ->
    ('v -> 'v -> int ) ->
     int
(** [cmp s0 s1 vcmp]

    Totoal ordering of map given total ordering of value function.

    It will compare size first and each element following the order one by one.
*)

val eqU:  
    ('k, 'v, 'id) t -> 
    ('k, 'v, 'id) t -> 
    ('v -> 'v -> bool [@bs]) -> 
    bool
val eq:  
    ('k, 'v, 'id) t -> 
    ('k, 'v, 'id) t -> 
    ('v -> 'v -> bool) -> 
    bool
(** [eq m1 m2 veq] tests whether the maps [m1] and [m2] are
    equal, that is, contain equal keys and associate them with
    equal data.  [veq] is the equality predicate used to compare
    the data associated with the keys. *)

val forEachU:  ('k, 'v, 'id) t -> ('k -> 'v -> unit [@bs]) -> unit
val forEach:  ('k, 'v, 'id) t -> ('k -> 'v -> unit) -> unit
(** [forEach m f] applies [f] to all bindings in map [m].
    [f] receives the 'k as first argument, and the associated value
    as second argument.  The bindings are passed to [f] in increasing
    order with respect to the ordering over the type of the keys.

    @example {[
      module IntCmp =
        (val Belt.Id.comparableU (fun[\@bs] (x:int) y -> Pervasives.compare x y));;

      let s0 = fromArray ~id:(module IntCmp) [|4,"4";1,"1";2,"2,"3""|];;
      let acc = ref [] ;;
      forEach s0 (fun k v -> acc := (k,v) :: !acc);;

      !acc = [4,"4"; 3,"3"; 2,"2"; 1,"1"]
    ]}
*)

val reduceU: ('k, 'v, 'id) t -> 'acc -> ('acc -> 'k -> 'v -> 'acc [@bs]) -> 'acc
val reduce: ('k, 'v, 'id) t -> 'acc -> ('acc -> 'k -> 'v -> 'acc) -> 'acc
(** [reduce m a f] computes [(f kN dN ... (f k1 d1 a)...)],
    where [k1 ... kN] are the keys of all bindings in [m]
    (in increasing order), and [d1 ... dN] are the associated data.

    @example {[
      module IntCmp =
        (val Belt.Id.comparableU (fun[\@bs] (x:int) y -> Pervasives.compare x y));;

      let s0 = fromArray ~id:(module IntCmp) [|4,"4";1,"1";2,"2,"3""|];;
      reduce s0 [] (fun acc k v -> (k,v) acc ) = [4,"4";3,"3";2,"2";1,"1"];;
    ]}
*)

val everyU: ('k, 'v, 'id) t -> ('k -> 'v -> bool [@bs]) ->  bool
val every: ('k, 'v, 'id) t -> ('k -> 'v -> bool) ->  bool
(** [every m p] checks if all the bindings of the map
    satisfy the predicate [p]. Order unspecified *)

val someU: ('k, 'v, 'id) t -> ('k -> 'v -> bool [@bs]) ->  bool
val some: ('k, 'v, 'id) t -> ('k -> 'v -> bool) ->  bool
(** [some m p] checks if at least one binding of the map
    satisfy the predicate [p]. Order unspecified *)

val size: ('k, 'v, 'id) t -> int
(** [size s]

    @example {[
      module IntCmp =
        (val Belt.Id.comparableU (fun[\@bs] (x:int) y -> Pervasives.compare x y));;
      size (fromArray [2,"2"; 2,"1"; 3,"3"] ~id:(module IntCmp)) = 2 ;;
    ]}
*)
val toArray: ('k, 'v, 'id) t -> ('k * 'v) array
(** [toArray s]

    @example {[
      module IntCmp =
        (val Belt.Id.comparableU (fun[\@bs] (x:int) y -> Pervasives.compare x y));;
      toArray (fromArray [2,"2"; 1,"1"; 3,"3"] ~id:(module IntCmp)) = [1,"1";2,"2";3,"3"]
    ]}

*)
val toList: ('k, 'v, 'id) t -> ('k * 'v) list
(** In increasing order

    {b See} {!toArray}
*)
    
val ofArray:  ('k * 'v) array -> id:('k,'id) id -> ('k,'v,'id) t
[@@ocaml.deprecated "Use fromArray instead"]

val fromArray:  ('k * 'v) array -> id:('k,'id) id -> ('k,'v,'id) t
(** [fromArray kvs ~id]
    @example {[
      module IntCmp =
        (val Belt.Id.comparableU (fun[\@bs] (x:int) y -> Pervasives.compare x y));;
      toArray (fromArray [2,"2"; 1,"1"; 3,"3"] ~id:(module IntCmp)) = [1,"1";2,"2";3,"3"]
    ]}
*)    
val keysToArray: ('k, 'v, 'id) t -> 'k  array
(** [keysToArray s]

    @example {[
      module IntCmp =
        (val Belt.Id.comparableU (fun[\@bs] (x:int) y -> Pervasives.compare x y));;
      keysToArray (fromArray [2,"2"; 1,"1"; 3,"3"] ~id:(module IntCmp)) =
      [|1;2;3|];;
    ]}
*)    
val valuesToArray: ('k, 'v, 'id) t -> 'v  array
(** [valuesToArray s]

    @example {[
      module IntCmp =
        (val Belt.Id.comparableU (fun[\@bs] (x:int) y -> Pervasives.compare x y));;
      valuesToArray (fromArray [2,"2"; 1,"1"; 3,"3"] ~id:(module IntCmp)) =
      [|"1";"2";"3"|];;
    ]}

*)
    
val minKey: ('k, _, _) t -> 'k option
(** [minKey s]
    @return thte minimum key, None if not exist 
*)    

val minKeyUndefined: ('k, _, _) t -> 'k Js.undefined
(** {b See} {!minKey}*)    

val maxKey: ('k, _, _) t -> 'k option
(** [maxKey s]
    @return thte maximum key, None if not exist 
*)    
    
val maxKeyUndefined: ('k, _, _) t -> 'k Js.undefined
(** {b See} {!maxKey} *)    

val minimum: ('k, 'v,  _) t -> ('k * 'v) option
(** [minimum s]
    @return thte minimum key value pair, None if not exist 
*)    
    
val minUndefined: ('k, 'v, _) t -> ('k * 'v) Js.undefined
(** {b See} {!minimum} *)    

val maximum: ('k, 'v, _) t -> ('k * 'v) option
(** [maximum s]
    @return thte maximum key value pair, None if not exist 
*)    

val maxUndefined:('k, 'v, _) t -> ('k * 'v) Js.undefined
(** {b See} {!maximum}
*)
    
val get:  ('k, 'v, 'id) t -> 'k -> 'v option
(** [get s k]

    @example {[
      module IntCmp =
        (val Belt.Id.comparableU (fun[\@bs] (x:int) y -> Pervasives.compare x y));;
      get (fromArray [2,"2"; 1,"1"; 3,"3"] ~id:(module IntCmp)) 2 =
      Some "2";;
      get (fromArray [2,"2"; 1,"1"; 3,"3"] ~id:(module IntCmp)) 2 =
      None;;
    ]}
*)
    
val getUndefined: ('k, 'v, 'id) t -> 'k ->  'v Js.undefined
(** {b See} {!get}

    @return [undefined] when not found
*)    
val getWithDefault:
    ('k, 'v, 'id) t -> 'k ->  'v -> 'v
(** [getWithDefault s k default]

   {b See} {!get}

    @return [default] when [k] is not found
    
*)    
val getExn:  ('k, 'v, 'id) t -> 'k -> 'v 
(** [getExn s k]

   {b See} {!getExn}

    {b raise} when [k] not exist
*)

(****************************************************************************)

val remove:  ('k, 'v, 'id) t -> 'k -> ('k, 'v, 'id) t
(** [remove m x] when [x] is not in [m], [m] is returned reference unchanged.

    @example {[
      module IntCmp =
        (val Belt.Id.comparableU (fun[\@bs] (x:int) y -> Pervasives.compare x y));;
      
      let s0 =  (fromArray [2,"2"; 1,"1"; 3,"3"] ~id:(module IntCmp));;

      let s1 = remove s0 1;;
      let s2 = remove s1 1;;
      s1 == s2 ;;
      keysToArray s1 = [|2;3|];;
    ]}
    
*)
    
val removeMany: ('k, 'v, 'id) t -> 'k array -> ('k, 'v, 'id) t  
(** [removeMany s xs]

    Removing each of [xs] to [s], note unlike {!remove},
    the reference of return value might be changed even if none in [xs]
    exists [s]
*)    
  
val set: 
    ('k, 'v, 'id) t -> 'k -> 'v ->  ('k, 'v, 'id) t
(** [set m x y ] returns a map containing the same bindings as
    [m], with a new binding of [x] to [y]. If [x] was already bound
    in [m], its previous binding disappears.

    @example {[
      module IntCmp =
        (val Belt.Id.comparableU (fun[\@bs] (x:int) y -> Pervasives.compare x y));;
      
      let s0 =  (fromArray [2,"2"; 1,"1"; 3,"3"] ~id:(module IntCmp));;

      let s1 = set s0 2 "3";;

      valuesToArray s1 =  ["1";"3";"3"];;
    ]}
*)
      
val updateU: ('k, 'v, 'id) t -> 'k -> ('v option -> 'v option [@bs]) -> ('k, 'v, 'id) t      
val update: ('k, 'v, 'id) t -> 'k -> ('v option -> 'v option) -> ('k, 'v, 'id) t      
(** [update m x f] returns a map containing the same bindings as
    [m], except for the binding of [x].
    Depending on the value of
    [y] where [y] is [f (get x m)], the binding of [x] is
    added, removed or updated. If [y] is [None], the binding is
    removed if it exists; otherwise, if [y] is [Some z] then [x]
    is associated to [z] in the resulting map. 
*)

val mergeMany:
    ('k, 'v, 'id) t -> ('k * 'v) array ->  ('k, 'v, 'id) t
(** [mergeMany s xs]

    Adding each of [xs] to [s], note unlike {!add},
    the reference of return value might be changed even if all values in [xs]
    exist [s]
*)

val mergeU:
   ('k, 'v, 'id ) t -> 
   ('k, 'v2, 'id) t ->
   ('k -> 'v option -> 'v2 option -> 'v3 option [@bs]) -> 
   ('k, 'v3, 'id) t
val merge:
   ('k, 'v, 'id ) t -> 
   ('k, 'v2, 'id) t ->
   ('k -> 'v option -> 'v2 option -> 'v3 option) -> 
   ('k, 'v3, 'id) t
(** [merge m1 m2 f] computes a map whose keys is a subset of keys of [m1]
    and of [m2]. The presence of each such binding, and the corresponding
    value, is determined with the function [f].
*)    


val keepU: 
    ('k, 'v, 'id) t -> 
    ('k -> 'v -> bool [@bs]) -> 
    ('k, 'v, 'id) t
val keep: 
    ('k, 'v, 'id) t -> 
    ('k -> 'v -> bool) -> 
    ('k, 'v, 'id) t
(** [keep m p] returns the map with all the bindings in [m]
    that satisfy predicate [p]. *)
    
val partitionU: 
    ('k, 'v, 'id) t ->
    ('k -> 'v -> bool [@bs]) -> 
    ('k, 'v, 'id) t * ('k, 'v, 'id) t
val partition: 
    ('k, 'v, 'id) t ->
    ('k -> 'v -> bool) -> 
    ('k, 'v, 'id) t * ('k, 'v, 'id) t
(** [partition m p] returns a pair of maps [(m1, m2)], where
    [m1] contains all the bindings of [s] that satisfy the
    predicate [p], and [m2] is the map with all the bindings of
    [s] that do not satisfy [p].
*)

val split: 
    ('k, 'v, 'id) t -> 'k -> 
    (('k, 'v, 'id) t * ('k, 'v, 'id) t )* 'v option 
(** [split x m] returns a tuple [(l r), data], where
      [l] is the map with all the bindings of [m] whose 'k
    is strictly less than [x];
      [r] is the map with all the bindings of [m] whose 'k
    is strictly greater than [x];
      [data] is [None] if [m] contains no binding for [x],
      or [Some v] if [m] binds [v] to [x].
*)

val mapU: ('k, 'v, 'id) t -> ('v -> 'v2 [@bs]) ->  ('k, 'v2, 'id) t
val map: ('k, 'v, 'id) t -> ('v -> 'v2) ->  ('k, 'v2, 'id) t
(** [map m f] returns a map with same domain as [m], where the
    associated value [a] of all bindings of [m] has been
    replaced by the result of the application of [f] to [a].
    The bindings are passed to [f] in increasing order
    with respect to the ordering over the type of the keys. *)

val mapWithKeyU: ('k, 'v, 'id) t -> ('k -> 'v -> 'v2 [@bs]) -> ('k, 'v2, 'id) t
val mapWithKey: ('k, 'v, 'id) t -> ('k -> 'v -> 'v2) -> ('k, 'v2, 'id) t
(** [mapWithKey m f]

    The same as {!map} except that [f] is supplied with one more argument: the key 

    
*)



val getData: ('k, 'v, 'id) t -> ('k, 'v, 'id) Belt_MapDict.t
(** [getData s0]

    {b Advanced usage only}
    
    @return the raw data (detached from comparator),
    but its type is still manifested, so that user can pass identity directly
    without boxing
*)

val getId: ('k, 'v, 'id) t -> ('k, 'id) id
(** [getId s0]

    {b Advanced usage only}
    
    @return the identity of [s0]
*)

val packIdData: id:('k, 'id) id -> data:('k, 'v, 'id) Belt_MapDict.t -> ('k, 'v, 'id) t
(** [packIdData ~id ~data]

    {b Advanced usage only}
    
    @return the packed collection
*)    

(**/**)
val checkInvariantInternal: _ t -> unit
(**
   {b raise} when invariant is not held
*)  
(**/**)
