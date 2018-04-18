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

(** {!Belt.Option}

    Utilities for option data type
*)

val getExn : 'a option -> 'a
(** [getExn optionalValue]
  Returns [value] if [optionalValue] is [Some value], otherwise raises [getExn]
  
  @example {[
    getExn (Some 3) = 3;;
    getExn None (* Raises getExn error *)
  ]}
*)

val mapWithDefaultU : 'a option -> 'b -> ('a -> 'b [@bs]) -> 'b
(** Uncurried version of [mapWithDefault] *)

val mapWithDefault : 'a option -> 'b -> ('a -> 'b) -> 'b
(**
  [mapWithDefault optionValue default f]
  
  If [optionValue] is [Some value], returns [f value]; otherwise returns [default]
  
  @example {[
    mapWithDefault (Some 3) 0 (fun x -> x + 5) = 8;;
    mapWithDefault None 0 (fun x -> x + 5) = 0;;
  ]}
*)

val mapU : 'a option -> ('a -> 'b [@bs]) -> 'b option
(** Uncurried version of [map] *)

val map : 'a option -> ('a -> 'b) -> 'b option
(**
  [map optionValue f]
  
  If [optionValue] is [Some value], returns [Some (f value)]; otherwise returns [None]
  
  @example {[
    map (Some 3) (fun x -> x * x) = (Some 9);;
    map None (fun x -> x * x) = None;;
  ]}
*)

val flatMapU : 'a option -> ('a -> 'b option [@bs]) -> 'b option
(** Uncurried version of [flatMap] *)

val flatMap : 'a option -> ('a -> 'b option) -> 'b option
(**
  [flatMap optionValue f]
  
  If [optionValue] is [Some value], returns [f value]; otherwise returns [None]
  The function [f] must have a return type of ['a option]
  
  @example {[
    let f (x : float =
        if x >= 0.0 then
          Some (sqrt x)
        else
          None;;
  
    flatMap (Some 4.0) f = Some 2.0;;
    flatMap (Some (-4.0)) f = None;;
    flatMap None f = None;;
  ]}
*)

val getWithDefault : 'a option -> 'a -> 'a
(**
  [getWithDefault optionalValue default]
  
  If [optionalValue] is [Some value], returns [value], otherwise [default]
  
  @example {[
    getWithDefault (Some 1812) 1066 = 1812;;
    getWithDefault None 1066 = 1066;;
  ]}
*)

val isSome : 'a option -> bool
(**
  Returns [true] if the argument is [Some value], [false] otherwise
*)

val isNone : 'a option -> bool
(**
  Returns [true] if the argument is [None], [false] otherwise
*)

val eqU : 'a option -> 'b option -> ('a -> 'b -> bool [@bs]) -> bool
(**
  Uncurried version of [eq]
*)

val eq : 'a option -> 'b option -> ('a -> 'b -> bool) -> bool
(**
  [eq optValue1 optvalue2 predicate]
  
  Evaluates two optional values for equality with respect to a predicate function.
  
  If both [optValue1] and [optValue2] are [None], returns [true].
  
  If one of the arguments is [Some value] and the other is [None], returns [false]
  
  If arguments are [Some value1] and [Some value2], returns the result of [predicate value1 value2];
  the [predicate] function must return a [bool]
  
  @example {[
    let clockEqual = (fun a b -> a mod 12 = b mod 12);;
    eq (Some 3) (Some 15) clockEqual = true;;
    eq (Some 3) None clockEqual = false;;
    eq None (Some 3) clockEqual = false;;
    eq None None clockEqual = true;;
  ]}
*)
    
val cmpU : 'a option -> 'b option -> ('a -> 'b -> int [@bs]) -> int
(** Uncurried version of [cmp] *)

val cmp : 'a option -> 'b option -> ('a -> 'b -> int) -> int
(**
  [cmp optValue1 optvalue2 comparisonFcn]
  
  Compares two optional values with respect to a comparison function
  
  If both [optValue1] and [optValue2] are [None], returns 0.
  
  If the first argument is [Some value1] and the second is [None], returns 1 (something is greater than nothing)
  
  If the first argument is [None] and the second is [Some value2], returns -1 (nothing is less than something)
  
  If the arguments are [Some value1] and [Some value2], returns the result of [comparisonFcn value1 value2]; [comparisonFcn] takes two arguments and returns -1 if the first argument is less than the second, 0 if the arguments are equal, and 1 if the first argument is greater than the second.
    
  @example {[
    let clockCompare = fun a b -> compare (a mod 12) (b mod 12);;
    cmp (Some 3) (Some 15) clockCompare = 0;;
    cmp (Some 3) (Some 14) clockCompare = 1;;
    cmp (Some 2) (Some 15) clockCompare = -1;;
    cmp None (Some 15) clockCompare = -1;;
    cmp (Some 14) None clockCompare = 1;;
    cmp None None clockCompare = 0;;
  ]}
*)
