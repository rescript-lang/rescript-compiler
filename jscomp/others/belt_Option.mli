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
(** [getExn (Some x)] {b returns} [x]; [getExn None] {b raises} [getExn] *)

val mapWithDefaultU : 'a option -> 'b -> ('a -> 'b [@bs]) -> 'b
(** Uncurried version of [mapWithDefault] *)

val mapWithDefault : 'a option -> 'b -> ('a -> 'b) -> 'b
(** [mapWithDefault optValue default f]
  {b Returns:} [f x] if [optValue] is [Some x], or [default] if [optValue] is [None]


  @example {[
    let square x = x * x;;
    mapWithDefault (Some 3) 0 square = 9;;
    mapWithDefault None 0 square = 0;;
  ]}
*)

val mapU : 'a option -> ('a -> 'b [@bs]) -> 'b option
(** Uncurried version of [map] *)

val map : 'a option -> ('a -> 'b) -> 'b option
(** [map optValue f]
  {b Returns:} [Some (f x)] if [optValue] is [Some x], or [None] if [optValue] is [None]


  @example {[
    let square x = x * x;;
    map (Some 3) square = Some 9;;
    map None square = None;;
  ]}
*)

val flatMapU : 'a option -> ('a -> 'b option [@bs]) -> 'b option
(** Uncurried version of [flatMap] *)

val flatMap : 'a option -> ('a -> 'b option) -> 'b option
(** [flatMap optValue f]
  {b Returns:} [f x] if [optValue] is [Some x], or [None] if [optValue] is [None]
  Function [f] {i must} return a value of ['b option]

  @example {[
    let validScore percent =
      match percent with
       | n when n >= 0.0 && n <= 100.0 -> Some (n /. 100.0)
       | _ -> None
    ;;  
           
    flatMap (Some 50.0) validScore = Some 0.50;;
    flatMap (Some 102.0) validScore = None;;
    flatMap None validScore = None;;
  ]}
*)

val getWithDefault : 'a option -> 'a -> 'a
(** [getWithDefault optValue defaultValue]
    {b returns} [x] when [optValue] is [Some x], or [defaultValue] when [optValue] is [None]
    
    @example {[
      getWithDefault (Some 33) 100 = 33;;
      getWithDefault None 100 = 100;;
    ]}
*)

val isSome : 'a option -> bool
(** [isSome optValue] returns [true] when [optValue] is [Some x]; [false] otherwise *)

val isNone : 'a option -> bool
(** [isNone optValue] returns [true] when [optValue] is [None]; [false] otherwise *)

val eqU : 'a option -> 'b option -> ('a -> 'b -> bool [@bs]) -> bool
(** Uncurried version of [eq] *)

val eq : 'a option -> 'b option -> ('a -> 'b -> bool) -> bool
(** [eq optA optB f] compares two optional values for equality with respect to function [f]
  {b Returns:}
    [false] if one of [optA] and [optB] is [Some x] and the other is [None]
    [true] if both [optA] and [optB] are [None]
    the result of [f a b] where [optA] is [Some a] and [optB] is [Some b]
    
  The following example tests if two optional integers are equivalent mod 12.
  
  @example {[
    let mod12Same a b = (a mod 12) = (b mod 12);;
    
    eq (Some 13) (Some 1) mod12Same = true;;
    eq (Some 13) (Some 9) mod12Same = false;;
    eq (Some 13) None mod12Same = false;;
    eq None (Some 13) mod12Same = false;;
    eq None None mod12Same = true;;
  ]}
*)
  
val cmpU : 'a option -> 'b option -> ('a -> 'b -> int [@bs]) -> int
(** Uncurried version of [cmp] *)

val cmp : 'a option -> 'b option -> ('a -> 'b -> int) -> int
(** [eq optA optB f] compares optional value [optA] to [optB] with respect to function [f]
  {b Returns:}
    [-1] (meaning "less than") if  [optA] is [None] and [optB] is [Some x]
    [1] (meaning "greater than") if [optA] is [Some x] [optB] is [None]
    [0] (meaning "equal") if both [optA] and [optB] are [None]
    the result of [f a b] where [optA] is [Some a] and [optB] is [Some b]; this result will be
    [-1] for less than, [0] for equal, and [1] for greater than.
    
  The following example tests compares two optional integers using the built-in [compare] function.
  
  @example {[
    cmp None (Some 15) compare = -1;;
    cmp (Some 15) None compare = 1;;
    cmp None None compare = 0;;
    cmp (Some 9) (Some 13) compare = -1;;
    cmp (Some 1) (Some 1) compare = 0;;
    cmp (Some 13) (Some 9) compare = 1;;
  ]}
*)
