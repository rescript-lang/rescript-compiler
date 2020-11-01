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

(** {!Belt.AsyncData}
    
    Utilities for async data representation.
*)

(**
  [Belt.AsyncData] is a data type with three variants: [NotAsked], [Loading] and [Done]. Each of these variants represents
  a possible async data state, and the [Done] variant can contain some data. [Belt.AsyncData] is
  useful to store the current state of an async operation (for instance a server fetch).
  
  In the examples, we presume the existence of three variables:
  
  @example {[
  let notAsked = NotAsked
  let loading = Loading
  let done = Done 42
  ]}
*)
type 'a t  =
  | NotAsked
  | Loading
  | Done 'a

val getExn : ('a, 'b) t -> 'a
(**
  [getExn res]
  
  when [res] is [Done n], returns [n]
  when [res] is [NotAsked] or [Loading], {b raise} an exception
  
  @example {[
    getExn done = 42;;
    getExn notAsked;; (* raises exception *)
    getExn loading;; (* raises exception *)
  ]}
*)

val mapWithDefaultU : 'a t -> 'b -> ('a -> 'b [@bs]) -> 'b
val mapWithDefault : 'a t -> 'b -> ('a -> 'b) -> 'b
(**
  [mapWithDefault res default f]
  
  When [res] is [Ok n], returns [f n], otherwise [default].
  
  @example{[
    mapWithDefault done 0 (fun x -> x / 2) = 21
    mapWithDefault notAsked 0 (fun x -> x / 2) = 0
    mapWithDefault loading 0 (fun x -> x / 2) = 0
  ]}
*)

val mapU: 'a t -> ('a -> 'b [@bs]) -> 'b t
val map: 'a t -> ('a -> 'b) -> 'b t
(**
  [map res f]
  
  When [res] is [Done n], returns [Done (f n)]. Otherwise returns [res] unchanged.
  Function [f] takes a value of the same type as [n] and returns an ordinary value.
  
  @example{[
    let f x = sqrt (float_of_int x)
    map (Done 64) f = Done 8.0
    map NotAsked f = NotAsked
    map Loading f = Loading
  ]}
*)

val flatMapU: 'a t -> ('a -> 'b t [@bs]) -> 'b t
val flatMap: 'a t -> ('a -> 'b t) -> 'b t
(**
  [flatMap res f]
  
  When [res] is [Done n], returns [f n]. Otherwise, returns [res] unchanged.
  Function [f] takes a value of the same type as [n] and returns a [Belt.AsyncData].
  
  @example {[
  let recip x = 
    if x != 0.0
    then
      Done (1.0 /. x)
    else
      NotAsked

    flatMap (Done 2.0) recip = Done 0.5
    flatMap (Done 0.0) recip = NotAsked
    flatMap Loading recip = Loading
  ]}
*)

val getWithDefault: 'a t -> 'a -> 'a
(**
  [getWithDefault res defaultValue]
  
  if [res] is [Done n], returns [n], otherwise [default]
  
  @example {[
    getWithDefault (Done 42) 0 = 42
    getWithDefault NotAsked 0 = 0
    getWithDefault Loading 0 = 0
  ]}
*)

val isLoading: 'a t -> bool
(**
  [isLoading res]
  
  Returns [true] if [res] is of the form [Loading], [false] if it is the [Done n] or [NotAsked] variant.
*)

val isDone: 'a t -> bool
(**
  [isDone res]
  
  Returns [true] if [res] is of the form [Done n], [false] if it is the [Loading] or [NotAsked] variant.
*)

val isNotAsked: 'a t -> bool
(**
  [isNotAsked res]
  
  Returns [true] if [res] is of the form [NotAsked], [false] if it is the [Loading] or [Done n] variant.
*)

val eqU : 'a t -> 'b t -> ('a -> 'b -> bool [@bs]) -> bool
val eq : 'a t -> 'b t -> ('a -> 'b -> bool) -> bool
(**
  [eq res1 res2 f]
  
  Determine if two [Belt.AsyncData] variables are equal with respect to an equality function.
  If [res1] and [res2] are of the form [Done n] and [Done m], return the result of [f n m].
  If one of [res1] and [res2] are of both of the form [NotAsked], return true
  If one of [res1] and [res2] are of both of the form [Loading], return true
  If one of [res1] and [res2] are of different forms, return false
  
  @example {[
    let good1 = Done 42
    let good2 = Done 32
    let bad1 = NotAsked
    let bad2 = Loading
    
    let mod10equal a b =
      a mod 10 == b mod 10
      
    eq good1 good2 mod10equal = true
    eq good1 bad1 mod10equal = false
    eq bad2 good2 mod10equal = false
    eq bad1 bad2 mod10equal = false
  ]}
*)

val cmpU : 'a t -> 'b t -> ('a -> 'b -> int [@bs]) -> int
val cmp : 'a t -> 'b t -> ('a -> 'b -> int) -> int
(**
  [cmp res1 res2 f]
  
  Compare two [Belt.AsyncData] variables with respect to a comparison function.
  The comparison function returns -1 if the first variable is "less than" the second,
  0 if the two variables are equal, and 1 if the first is "greater than" the second.
  
  If [res1] and [res2] are of the form [Done n] and [Done m], return the result of [f n m].
  If [res1] is of the form [Loading] and [res2] of the form [Done n], return -1 (nothing is less than something)
  If [res1] is of the form [Done n] and [res2] of the form [Loading], return 1 (something is greater than nothing)
  If [res1] is of the form [NotAsked] and [res2] of the form [Done n], return -1 (nothing is less than something)
  If [res1] is of the form [Done n] and [res2] of the form [NotAsked], return 1 (something is greater than nothing)
  If [res1] is of the form [NotAsked] and [res2] of the form [Loading], return -1 (nothing is less than something)
  If [res1] is of the form [Loading] and [res2] of the form [NotAsked], return 1 (something is greater than nothing)
  If both [res1] and [res2] are of the form [Error e], return 0 (equal)
  
  @example {[
    let mod10cmp a b =
      Pervasives.compare (a mod 10) (b mod 10)
      
    cmp (Done 39) (Done 57) mod10cmp = 1
    cmp (Done 57) (Done 39) mod10cmp = -1
    cmp (Done 39) Loading mod10cmp = 1
    cmp Loading (Done 57) mod10cmp = -1
    cmp NotAsked (Done 57) mod10cmp = -1
    cmp (Done 57) NotAsked mod10cmp = 1
    cmp Loading NotAsked mod10cmp = 1
    cmp NotAsked Loading mod10cmp = -1
    cmp Loading Loading mod10cmp = 0
    cmp NotAsked NotAsked mod10cmp = 0
  ]}
*)
