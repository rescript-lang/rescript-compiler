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

(** [length str] returns the length of the string.

@example {[
  length "abc" = 3;;
]}
*)
external length : t -> int = "length" [@@bs.get]

(** [get str index] returns the string at [index].

@example {[
  get "abc" 1 = Some("b");;
]}
*)
external get : t -> int -> t option = "%array_unsafe_get"

(** [concat append original] returns a new string with [append] added after [original].

@example {[
  concat "a" "b" = "ab";;
]}
*)
external concat : t -> t -> t = "concat" [@@bs.send]

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
external endsWith : t -> t -> bool = "endsWith" [@@bs.send] 


(**
  [indexOf s searchValue ] returns an option containing the index of [searchValue] in [s] if found.
  
@example {[
  indexOf "programmer" "gram" = Some(3);;
  indexOf "programmer" "er" = Some(8);;
  indexOf "programmer" "pro" = Some(0);;
  indexOf "programmer" "xyz" = None;;
]}
*)
val indexOf: string -> string -> int option

(**
  [includes s searchValue ] returns [true] if [searchValue] is found anywhere within [s], [false] otherwise.
  
@example {[
  includes "programmer" "gram" = true;;
  includes "programmer" "er" = true;;
  includes "programmer" "pro" = true;;
  includes "programmer" "xyz" = false;;
]}
*)
external includes : t -> t -> bool = "includes" [@@bs.send] (** ES2015 *)



(**
  [repeat n s] returns a string that consists of [n] repetitions of [s]. Raises [RangeError] if [n] is negative.
  
@example {[
  repeat "ha" 3 = "hahaha"
  repeat "empty" 0 = ""
]}
*)
external repeat : t -> int -> t = "repeat" [@@bs.send] (** ES2015 *)

(** [replace string substr newSubstr ] returns a new string which is
identical to [string] except with the first matching instance of [substr]
replaced by [newSubstr].

[substr] is treated as a verbatim string to match, not a regular
expression.

@example {[
  replace "old string" ~old="old" ~by="new"  = "new string"
  replace "the cat and the dog" ~old="the" ~by="this"  = "this cat and the dog"
]}
*)
external replace : t -> old:t -> by:t -> t = "replace" [@@bs.send]

(** [replaceRegex string ~old=regex ~by=replacement] returns a new string where occurrences matching [regex]
have been replaced by [replacement].

@example {[
  replaceRegex ~old=[%re "/[aeiou]/g"] ~by="x" "vowels be gone" = "vxwxls bx gxnx"
  replaceRegex ~old=[%re "/(\\w+) (\\w+)/"] ~by="$2, $1" "Juan Fulano" = "Fulano, Juan"
]}
*)
external replaceRegex : t -> old:Js_re.t -> by:t -> t = "replace" [@@bs.send]


(**
  [matchRegex str regexp] matches a string against the given [regexp]. If there is no match, it returns [None].
  For regular expressions without the [g] modifier, if there is a match, the return value is [Some array] where the array contains:
  
  {ul
    {- The entire matched string}
    {- Any capture groups if the [regexp] had parentheses}
  }
  
  For regular expressions with the [g] modifier, a matched expression returns [Some array] with all the matched substrings and no capture groups.
  
@example {[
  matchRegex "The better bats" [%re "/b[aeiou]t/"] = Some [|"bet"|]
  matchRegex "The better bats" [%re "/b[aeiou]t/g"] = Some [|"bet";"bat"|]
  matchRegex "Today is 2018-04-05." [%re "/(\\d+)-(\\d+)-(\\d+)/"] =
    Some [|"2018-04-05"; "2018"; "04"; "05"|]
  matchRegex "The large container." [%re "/b[aeiou]g/"] = None
]}

*)
external matchRegex : string -> Js_re.t -> t array option = "match" [@@bs.send] [@@bs.return {null_to_opt}]

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
external split : t ->  t -> t array  = "split" [@@bs.send]

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
external startsWith : t -> t -> bool = "startsWith" [@@bs.send]


(**
  [substr str ~from: n] returns the substring of [str] from position [n] to the end of the string.
  
  If [n] is less than zero, the starting position is the length of [str] - [n].
  
  If [n] is greater than or equal to the length of [str], returns the empty string.
  
@example {[
  substr "abcdefghij" ~from: 3 ~len: 2 = "de"
  substr "abcdefghij" ~from: (-3) ~len: 3 = "hij"
  substr "abcdefghij" ~from: 12 ~len: 2= ""
]}
*)
external substr : t -> from:int -> len:int -> t = "substr" [@@bs.send]

(**
  [substrToEnd str ~from: n] returns the substring of [str] from position [n] to the end of the string.
  
  If [n] is less than zero, the starting position is the length of [str] - [n].
  
  If [n] is greater than or equal to the length of [str], returns the empty string.
  
@example {[
  substrToEnd "abcdefghij" ~from: 3 = "defghij"
  substrToEnd "abcdefghij" ~from: (-3) = "hij"
  substrToEnd "abcdefghij" ~from: 12 = ""
]}
*)
external substrToEnd : t -> from:int -> t = "substr" [@@bs.send]

(** [slice from:n1 to_:n2 str] returns the substring of [str] starting at character [n1] up to but not including [n2]

If either [n1] or [n2] is negative, then it is evaluated as [length str - n1] (or [length str - n2].

If [n2] is greater than the length of [str], then it is treated as [length str].

If [n1] is greater than [n2], [slice] returns the empty string.

@example {[
  slice "abcdefg" ~from:2 ~to_:5 == "cde";;
  slice "abcdefg" ~from:2 ~to_:9 == "cdefg";;
  slice "abcdefg" ~from:(-4) ~to_:(-2)  == "de";; 
  slice "abcdefg" ~from:5 ~to_:1 == "";;
]}
*)
external slice : t -> from:int -> to_:int -> t = "slice" [@@bs.send]

(** [sliceToEnd from: n str] returns the substring of [str] starting at character [n] to the end of the string

If [n] is negative, then it is evaluated as [length str - n].

If [n] is greater than the length of [str], then [sliceToEnd] returns the empty string.

@example {[
  sliceToEnd "abcdefg" ~from: 4 == "efg";;
  sliceToEnd "abcdefg" ~from: (-2) == "fg";; 
  sliceToEnd "abcdefg" ~from: 7 == "";;
]}
*)
external sliceToEnd : t -> from:int -> t = "slice" [@@bs.send]

(**
  [trim str] returns a string that is [str] with whitespace stripped from both ends. Internal whitespace is not removed.

@example {[
  trim "   abc def   " = "abc def"
  trim "\n\r\t abc def \n\n\t\r " = "abc def"
]}
*)
external trim : t -> t = "trim" [@@bs.send]

(**
  [trimStart str] returns a string that is [str] with whitespace stripped from the start. Internal whitespace is not removed.

@example {[
  trimStart "   abc def   " = "abc def   "
  trimStart "\n\r\t abc def \n\n\t\r " = "abc def \n\n\t\r"
]}
*)
external trimStart : t -> t = "trimStart" [@@bs.send] (** ES2015 *)

(**
  [trimEnd str] returns a string that is [str] with whitespace stripped from the end. Internal whitespace is not removed.

@example {[
  trimEnd "   abc def   " = "   abc def"
  trimEnd "\n\r\t abc def \n\n\t\r " = "\n\r\t abc def"
]}
*)
external trimEnd : t -> t = "trimEnd" [@@bs.send] (** ES2015 *)


(**
  [padStart str int padStr] returns a string that is [str] padded to the left with padStr.

@example {[
  padStart "4" 3 "0" = "004"
  padStart "444" 3 "0" = "444" 
]}
*)
external padStart : t -> int -> t -> t = "padStart" [@@bs.send] (** ES2015 *)

(**
  [padEnd str int padStr] returns a string that is [str] padded to the right with padStr.

@example {[
  padEnd "4" 3 "0" = "400"
  padEnd "444" 3 "0" = "444" 
]}
*)
external padEnd : t -> int -> t -> t = "padEnd" [@@bs.send] (** ES2015 *)

(**
  [toLowerCase str] converts [str] to lower case using the locale-insensitive case mappings in the Unicode Character Database. Notice that the conversion can give different results depending upon context, for example with the Greek letter sigma, which has two different lower case forms when it is the last character in a string or not.
  
@example {[
  toLowerCase "ABC" = "abc";;
  toLowerCase {js|ΣΠ|js} = {js|σπ|js};;
  toLowerCase {js|ΠΣ|js} = {js|πς|js};;
]}
*)
external toLowerCase : t -> t = "toLowerCase" [@@bs.send]

(**
  [toUpperCase str] converts [str] to upper case using the locale-insensitive case mappings in the Unicode Character Database. Notice that the conversion can expand the number of letters in the result; for example the German [ß] capitalizes to two [S]es in a row.
  
@example {[
  toUpperCase "abc" = "ABC";;
  toUpperCase {js|Straße|js} = {js|STRASSE|js};;
  toLowerCase {js|πς|js} = {js|ΠΣ|js};;
]}
*)
external toUpperCase : t -> t = "toUpperCase" [@@bs.send]
