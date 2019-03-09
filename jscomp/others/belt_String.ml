(* Copyright (C) 2018 Authors of BuckleScript
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

(** JavaScript String API *)

type t = string


(** [concat append original] returns a new string with [append] added after [original].

@example {[
  concat "a" "b" = "ab";;
]}
*)
external concat : t -> t -> t = "" [@@bs.send]

(** [concat arr original] returns a new string consisting of each item of an array of strings added to the [original] string.

@example {[
  concatMany "1st" [|"2nd"; "3rd"; "4th"|]  = "1st2nd3rd4th";;
]}
*)
external concatMany : t -> t array -> t = "concat" [@@bs.send] [@@bs.splice]

(** ES2015:
    [endsWith substr str] returns [true] if the [str] ends with [substr], [false] otherwise.
    
@example {[
  endsWith "BuckleScript" "Script"  = true;;
  endsWith "BuckleShoes" "Script"  = false;;
]} 
*)
external endsWith : t -> t -> bool = "" [@@bs.send] 


(**
  [includes s searchValue ] returns [true] if [searchValue] is found anywhere within [s], [false] otherwise.
  
@example {[
  includes "programmer" "gram" = true;;
  includes "programmer" "er" = true;;
  includes "programmer" "pro" = true;;
  includes "programmer" "xyz" = false;;
]}
*)
external includes : t -> t -> bool = "" [@@bs.send] (** ES2015 *)



(**
  [repeat n s] returns a string that consists of [n] repetitions of [s]. Raises [RangeError] if [n] is negative.
  
@example {[
  repeat "ha" 3 = "hahaha"
  repeat "empty" 0 = ""
]}
*)
external repeat : t -> int -> t = "" [@@bs.send] (** ES2015 *)

(** [replace string substr newSubstr ] returns a new string which is
identical to [string] except with the first matching instance of [substr]
replaced by [newSubstr].

[substr] is treated as a verbatim string to match, not a regular
expression.

@example {[
  replace "old string" "old" "new"  = "new string"
  replace "the cat and the dog" "the" "this"  = "this cat and the dog"
]}
*)
external replace : t -> t ->  t ->  t = "" [@@bs.send]



(**
  [split delimiter str] splits the given [str] at every occurrence of [delimiter] and returns an
  array of the resulting substrings.
  
@example {[
  split "2018-01-02" "-" = [|"2018"; "01"; "02"|];;
  split "a,b,,c" "," = [|"a"; "b"; ""; "c"|];;
  split "good::bad as great::awful" "::" = [|"good"; "bad as great"; "awful"|];;
  split "has-no-delimiter" ";"  = [|"has-no-delimiter"|];;
]};
*)
external split : t ->  t -> t array  = "" [@@bs.send]

(**
  [splitAtMost str delimiter n] splits the given [str] at every occurrence of [delimiter] and 
  returns an array of the first [n] resulting substrings. If [n] is negative or greater than the
   number of substrings, the array will contain all the substrings.
  
@example {[
  splitAtMost "ant/bee/cat/dog/elk" "/" 3  = [|"ant"; "bee"; "cat"|];;
  splitAtMost "ant/bee/cat/dog/elk" "/"  0  = [| |];;
  splitAtMost "ant/bee/cat/dog/elk" "/" 9  = [|"ant"; "bee"; "cat"; "dog"; "elk"|];;
]}
*)
external splitAtMost: t -> t -> int -> t array = "split" [@@bs.send]


(** ES2015:
    [startsWith str substr] returns [true] if the [str] starts with [substr], [false] otherwise.
    
@example {[
  startsWith "BuckleScript" "Buckle"  = true;;
  startsWith "BuckleScript" ""  = true;;
  startsWith "JavaScript" "Buckle"  = false;;
]} 
*)
external startsWith : t -> t -> bool = "" [@@bs.send]


(**
  [substr ~from: n str] returns the substring of [str] from position [n] to the end of the string.
  
  If [n] is less than zero, the starting position is the length of [str] - [n].
  
  If [n] is greater than or equal to the length of [str], returns the empty string.
  
@example {[
  substr ~from: 3 "abcdefghij" = "defghij"
  substr ~from: (-3) "abcdefghij" = "hij"
  substr ~from: 12 "abcdefghij" = ""
]}
*)
external substr : from:int -> t = "" [@@bs.send.pipe: t]


(**
  [trim str] returns a string that is [str] with whitespace stripped from both ends. Internal whitespace is not removed.

@example {[
  trim "   abc def   " = "abc def"
  trim "\n\r\t abc def \n\n\t\r " = "abc def"
]}
*)
external trim : t -> t = "" [@@bs.send]

(* link to JS built-ins *)
external str_length: t -> int = "length" [@@bs.get]
external str_charAt: t -> int -> t = "charAt" [@@bs.send]
external array_concat: 'a array -> 'a array -> 'a array = "concat" [@@bs.send]

(**
  [reduce str f] takes a string argument, initial value, and a reducer
  function [f] whose arguments are an accumulated value and a character from the string.

  [reduce] starts the accumulated value as the initial value, and updates it by
  applying the reducer function [f] to each character in the string.
  
@example {[
  let addCharCodes (total: float) (s:string) =
    (total +. (Js.String.charCodeAt 0 s): float)

  reduce "abc" 0.0 addCharCodes = 294.0
  
  let reverser (result: string) (item: string) =
    (item ^ result : string)
  
  reduce "abcde" "" reverser = "edcba"
]}
*)

let reduce (s: string) (acc: 'a) (f: 'a -> string -> 'a) =
  (let rec helper (acc: 'a) (index: int) =
    match index with
    | n when n == (str_length s) -> acc
    | n -> helper (f acc (str_charAt s n)) (n + 1) in

  helper acc 0 : 'a)

(**
  [stringMap s f] applies a function [f] to each character in the given string [s],
  returning a new string with the concatenated results of the function calls.

@example {[
let addDash (s : string) = (s ^ "-" : string)
stringMap "abcde" addDash = "a-b-c-d-e-"
]}
*)

let stringMap (s : string) (f : string -> string) =
  (reduce s "" (fun acc item -> acc ^ (f item)) : string)

(**
  [map s f] applies a function [f] to each character in the given string [s],
  returning an array containing the results of the function calls.
  
@example {[
let toCode (s : string) = (Js.String.charCodeAt 0 s : float)
map "abcde" toCode = [| 97.0; 98.0; 99.0; 100.0; 101.0 |]
]}
*)
let map (s : string) (f : string -> 'a) =
  (reduce s [||] (fun acc item -> array_concat acc [|(f item)|]) : 'a array)

(**
  [stringKeep s f] applies a function [f] to each character in the given string [s].
  It returns a new string containing only the characters for which [f] returned [true].

@example {[
let nonVowel (s:string) = 
  (not (s = "a" || s = "e" || s = "i" || s = "o" || s = "u") : bool)
stringKeep "cauliflower" nonVowel = "clflwr"
]}
*)
let stringKeep (s: string) (f: string -> bool) =
  (reduce s "" (fun acc item ->
      if (f item) then acc ^ item else acc) : string)

(**
  [keep s f] applies a function [f] to each character in the given string [s].
  It returns an array containing only the characters for which [f] returned [true].
  
@example {[
let nonVowel (s:string) = 
  (not (s = "a" || s = "e" || s = "i" || s = "o" || s = "u") : bool)
keep "cauliflower" nonVowel = [|"c"; "l"; "f"; "l"; "w"; "r"|]
]}
*)
let keep (s:string) (f: string -> bool) =
  (reduce s [||] (fun acc item ->
    if (f item) then array_concat acc [|item|] else acc) : 'a array)
