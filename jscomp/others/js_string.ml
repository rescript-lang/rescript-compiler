(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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

(** [make value] converts the given value to a string

@example {[
  make 3.5 = "3.5";;
  make [|1;2;3|]) = "1,2,3";;
]}
*)
external make : 'a -> t = "String" [@@bs.val]

(** [fromCharCode n]
  creates a string containing the character corresponding to that number; {i n} ranges from 0 to 65535. If out of range, the lower 16 bits of the value are used. Thus, [fromCharCode 0x1F63A] gives the same result as [fromCharCode 0xF63A].

@example {[
  fromCharCode 65 = "A";;
  fromCharCode 0x3c8 = {js|ψ|js};;
  fromCharCode 0xd55c = {js|한|js};;
  fromCharCode -64568 = {js|ψ|js};;
]}
*)
external fromCharCode : int -> t = "String.fromCharCode" [@@bs.val]

external fromCharCodeMany : int array -> t = "String.fromCharCode" [@@bs.val] [@@bs.splice]
(** [fromCharCodeMany \[|n1;n2;n3|\]] creates a string from the characters corresponding to the given numbers, using the same rules as [fromCharCode].

@example {[
  fromCharCodeMany([|0xd55c, 0xae00, 33|]) = {js|한글!|js};;
]}
*)

(** [fromCodePoint n]
  creates a string containing the character corresponding to that numeric code point. If the number is not a valid code point, {b raises} [RangeError]. Thus, [fromCodePoint 0x1F63A] will produce a correct value, unlike [fromCharCode 0x1F63A], and [fromCodePoint -5] will raise a [RangeError].
  
@example {[
  fromCodePoint 65 = "A";;
  fromCodePoint 0x3c8 = {js|ψ|js};;
  fromCodePoint 0xd55c = {js|한|js};;
  fromCodePoint 0x1f63a = {js|😺|js};;
]}

*)
external fromCodePoint : int -> t = "String.fromCodePoint" [@@bs.val] (** ES2015 *)

(** [fromCharCodeMany \[|n1;n2;n3|\]] creates a string from the characters corresponding to the given code point numbers, using the same rules as [fromCodePoint].

@example {[
  fromCodePointMany([|0xd55c; 0xae00; 0x1f63a|]) = {js|한글😺|js}
]}
*)
external fromCodePointMany : int array -> t = "String.fromCodePoint" [@@bs.val] [@@bs.splice] (** ES2015 *)


(* String.raw: ES2015, meant to be used with template strings, not directly *)

(** [length s] returns the length of the given string.

@example {[
  length "abcd" = 4;;
]}

*)
external length : t -> int = "" [@@bs.get]

(** [get s n] returns as a string the character at the given index number. If [n] is out of range, this function returns [undefined], so at some point this function may be modified to return [t option].

@example {[
  get "Reason" 0 = "R";;
  get "Reason" 4 = "o";;
  get {js|Rẽasöń|js} 5 = {js|ń|js};;
]}
*)
external get : t -> int -> t = "" [@@bs.get_index]

(** [charAt n s] gets the character at index [n] within string [s]. If [n] is negative or greater than the length of [s], returns the empty string. If the string contains characters outside the range [\u0000-\uffff], it will return the first 16-bit value at that position in the string.

@example {[
  charAt 0, "Reason" = "R"
  charAt( 12, "Reason") = "";
  charAt( 5, {js|Rẽasöń|js} = {js|ń|js}
]}
*)
external charAt : int ->  t = "" [@@bs.send.pipe: t]

(** [charCodeAt n s] returns the character code at position [n] in string [s]; the result is in the range 0-65535, unlke [codePointAt], so it will not work correctly for characters with code points greater than or equal to [0x10000].
The return type is [float] because this function returns [NaN] if [n] is less than zero or greater than the length of the string.

@example {[
  charCodeAt 0 {js|😺|js} returns 0xd83d
  codePointAt 0 {js|😺|js} returns Some 0x1f63a
]}

*)
external charCodeAt : int -> float  = "" [@@bs.send.pipe: t]

(** [codePointAt n s] returns the code point at position [n] within string [s] as a [Some] value. The return value handles code points greater than or equal to [0x10000]. If there is no code point at the given position, the function returns [None].

@example {[
  codePointAt 1 {js|¿😺?|js} = Some 0x1f63a
  codePointAt 5 "abc" = None
]}
*)
external codePointAt : int -> int option = "" [@@bs.send.pipe: t] [@@bs.return {undefined_to_opt}] (** ES2015 *)

(** [concat append original] returns a new string with [append] added after [original].

  @example {[
    concat "bell" "cow" = "cowbell";;
  ]}

*)
external concat : t -> t = "" [@@bs.send.pipe: t]

(** [concat arr original] returns a new string consisting of each item of an array of strings added to the [original] string.

  @example {[
    concatMany [|"2nd"; "3rd"; "4th"|] "1st" = "1st2nd3rd4th";;
  ]}
*)
external concatMany : t array -> t = "concat" [@@bs.send.pipe: t] [@@bs.splice]

(** ES2015:
    [endsWith substr str] returns [true] if the [str] ends with [substr], [false] otherwise.
    
    @example {[
      endsWith "Script" "BuckleScript" = true;;
      endsWith "Script" "COBOL" = false;;
    ]} 
*)
external endsWith : t -> bool = "" [@@bs.send.pipe: t] 

(** [endsWithFrom ending len str] returns [true] if the first [len] characters of [str] end with [ending], [false] otherwise. If [n] is greater than or equal to the length of [str], then it works like [endsWith]. (Honestly, this should have been named [endsWithAt], but oh well.)

  @example {[
    endsWithFrom "cd" 4 "abcd" = true;;
    endsWithFrom "cd" 3 "abcde" = false;;
    endsWithFrom "cde" 99 "abcde" = true;;
    endsWithFrom "ple" 7 "example.dat" = true;;
  ]}
*)
external endsWithFrom : t -> int -> bool = "endsWith" [@@bs.send.pipe: t] (** ES2015 *)

(**
  [includes searchValue s] returns [true] if [searchValue] is found anywhere within [s], [false] otherwise.
  
@example {[
  includes "gram" "programmer" = true;;
  includes "er" "programmer" = true;;
  includes "pro" "programmer" = true;;
  includes "xyz" "programmer" = false;;
]}
*)
external includes : t -> bool = "" [@@bs.send.pipe: t] (** ES2015 *)

(**
  [includes searchValue start s] returns [true] if [searchValue] is found anywhere within [s] starting at character number [start] (where 0 is the first character), [false] otherwise.
  
@example {[
  includesFrom "gram" 1 "programmer" = true;;
  includesFrom "gram" 4 "programmer" = false;;
  includesFrom {js|한|js} 1 {js|대한민국|js} = true;;
]}
*)
external includesFrom : t -> int -> bool = "includes" [@@bs.send.pipe: t] (** ES2015 *)

(**
  [indexOf searchValue s] returns the position at which [searchValue] was first found within [s], or [-1] if [searchValue] is not in [s].
  
@example {[
  indexOf "ok" "bookseller" = 2;;
  indexOf "sell" "bookseller" = 4;;
  indexOf "ee" "beekeeper" = 1;;
  indexOf "xyz" "bookseller" = -1;;
]}
*)
external indexOf : t -> int = "" [@@bs.send.pipe: t]

(**
  [indexOfFrom searchValue start s] returns the position at which [searchValue] was found within [s] starting at character position [start], or [-1] if [searchValue] is not found in that portion of [s]. The return value is relative to the beginning of the string, no matter where the search started from.
  
@example {[
  indexOfFrom "ok" 1 "bookseller" = 2;;
  indexOfFrom "sell" 2 "bookseller" = 4;;
  indexOfFrom "sell" 5 "bookseller" = -1;;
  indexOf "xyz" "bookseller" = -1;;
]}
*)
external indexOfFrom : t -> int -> int = "indexOf" [@@bs.send.pipe: t]

(**
  [lastIndexOf searchValue s] returns the position of the {i last} occurrence of [searchValue] within [s], searching backwards from the end of the string. Returns [-1] if [searchValue] is not in [s]. The return value is always relative to the beginning of the string.
  
@example {[
  lastIndexOf "ok" "bookseller" = 2;;
  lastIndexOf "ee" "beekeeper" = 4;;
  lastIndexOf "xyz" "abcdefg" = -1;;
]}
*)
external lastIndexOf : t -> int = "" [@@bs.send.pipe: t]

(**
  [lastIndexOfFrom searchValue start s] returns the position of the {i last} occurrence of [searchValue] within [s], searching backwards from the given [start] position. Returns [-1] if [searchValue] is not in [s]. The return value is always relative to the beginning of the string.
  
@example {[
  lastIndexOfFrom "ok" 6 "bookseller" = 2;;
  lastIndexOfFrom "ee" 8 "beekeeper" = 4;;
  lastIndexOfFrom "ee" 3 "beekeeper" = 1;;
  lastIndexOfFrom "xyz" 4 "abcdefg" = -1;;
]}
*)
external lastIndexOfFrom : t -> int -> int = "lastIndexOf" [@@bs.send.pipe: t]


(* extended by ECMA-402 *)
(**
  [localeCompare comparison reference] returns
  
{ul
  {- a negative value if [reference] comes before [comparison] in sort order}
  {- zero if [reference] and [comparison] have the same sort order}
  {- a positive value if [reference] comes after [comparison] in sort order}}
  
@example {[
  (localeCompare "ant" "zebra") > 0.0;;
  (localeCompare "zebra" "ant") < 0.0;;
  (localeCompare "cat" "cat") = 0.0;;
  (localeCompare "cat" "CAT") > 0.0;; 
]}
*)
external localeCompare : t -> float = "" [@@bs.send.pipe: t]

(**
  [match regexp str] matches a string against the given [regexp]. If there is no match, it returns [None].
  For regular expressions without the [g] modifier, if there is a match, the return value is [Some array] where the array contains:
  
  {ul
    {- The entire matched string}
    {- Any capture groups if the [regexp] had parentheses}
  }
  
  For regular expressions with the [g] modifier, a matched expression returns [Some array] with all the matched substrings and no capture groups.
  
@example {[
  match [%re "/b[aeiou]t/"] "The better bats" = Some [|"bet"|]
  match [%re "/b[aeiou]t/g"] "The better bats" = Some [|"bet";"bat"|]
  match [%re "/(\\d+)-(\\d+)-(\\d+)/"] "Today is 2018-04-05." =
    Some [|"2018-04-05"; "2018"; "04"; "05"|]
  match [%re "/b[aeiou]g/"] "The large container." = None
]}

*)
external match_ : Js_re.t -> t array option = "match" [@@bs.send.pipe: t] [@@bs.return {null_to_opt}]

(** [normalize str] returns the normalized Unicode string using Normalization Form Canonical (NFC) Composition.

Consider the character [ã], which can be represented as the single codepoint [\u00e3] or the combination of a lower case letter A [\u0061] and a combining tilde [\u0303]. Normalization ensures that both can be stored in an equivalent binary representation.
  
@see <https://www.unicode.org/reports/tr15/tr15-45.html> Unicode technical report for details
*)
external normalize : t = "" [@@bs.send.pipe: t] (** ES2015 *)

(**
  [normalize str form] (ES2015) returns the normalized Unicode string using the specified form of normalization, which may be one of:
  
  {ul
    {- "NFC" — Normalization Form Canonical Composition.}
    {- "NFD" — Normalization Form Canonical Decomposition.}
    {- "NFKC" — Normalization Form Compatibility Composition.}
    {- "NFKD" — Normalization Form Compatibility Decomposition.}
  }
  
  @see <https://www.unicode.org/reports/tr15/tr15-45.html> Unicode technical report for details
*)
external normalizeByForm : t -> t = "normalize" [@@bs.send.pipe: t]

(**
  [repeat n s] returns a string that consists of [n] repetitions of [s]. Raises [RangeError] if [n] is negative.
  
@example {[
  repeat 3 "ha" = "hahaha"
  repeat 0 "empty" = ""
]}
*)
external repeat : int -> t = "" [@@bs.send.pipe: t] (** ES2015 *)

(** [replace substr newSubstr string] returns a new string which is
identical to [string] except with the first matching instance of [substr]
replaced by [newSubstr].

[substr] is treated as a verbatim string to match, not a regular
expression.

@example {[
  replace "old" "new" "old string" = "new string"
  replace "the" "this" "the cat and the dog" = "this cat and the dog"
]}
*)
external replace : t ->  t ->  t = "" [@@bs.send.pipe: t]

(** [replaceByRe regex replacement string] returns a new string where occurrences matching [regex]
have been replaced by [replacement].

@example {[
  replaceByRe [%re "/[aeiou]/g"] "x" "vowels be gone" = "vxwxls bx gxnx"
  replaceByRe [%re "/(\\w+) (\\w+)/"] "$2, $1" "Juan Fulano" = "Fulano, Juan"
]}
*)
external replaceByRe : Js_re.t ->  t ->  t = "replace" [@@bs.send.pipe: t]

(** returns a new string with some or all matches of a pattern with no capturing
parentheses replaced by the value returned from the given function.
The function receives as its parameters the matched string, the offset at which the
match begins, and the whole string being matched

@example {[
let str = "beautiful vowels"
let re = [%re "/[aeiou]/g"]
let matchFn matchPart offset wholeString =
  Js.String.toUpperCase matchPart
  
let replaced = Js.String.unsafeReplaceBy0 re matchFn str

let () = Js.log replaced (* prints "bEAUtifUl vOwEls" *)
]}

@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace#Specifying_a_function_as_a_parameter> MDN
*)
external unsafeReplaceBy0 : Js_re.t -> (t -> int -> t -> t [@bs.uncurry]) -> t = "replace" [@@bs.send.pipe: t]

(** returns a new string with some or all matches of a pattern with one set of capturing
parentheses replaced by the value returned from the given function.
The function receives as its parameters the matched string, the captured string,
the offset at which the match begins, and the whole string being matched.

@example {[
let str = "increment 23"
let re = [%re "/increment (\\d+)/g"]
let matchFn matchPart p1 offset wholeString =
  wholeString ^ " is " ^ (string_of_int ((int_of_string p1) + 1))
  
let replaced = Js.String.unsafeReplaceBy1 re matchFn str

let () = Js.log replaced (* prints "increment 23 is 24" *)
]}

@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace#Specifying_a_function_as_a_parameter> MDN
*)
external unsafeReplaceBy1 : Js_re.t -> (t -> t -> int -> t -> t [@bs.uncurry]) -> t = "replace" [@@bs.send.pipe: t]

(** returns a new string with some or all matches of a pattern with two sets of capturing
parentheses replaced by the value returned from the given function.
The function receives as its parameters the matched string, the captured strings,
the offset at which the match begins, and the whole string being matched.

@example {[
let str = "7 times 6"
let re = [%re "/(\\d+) times (\\d+)/"]
let matchFn matchPart p1 p2 offset wholeString =
  string_of_int ((int_of_string p1) * (int_of_string p2))
  
let replaced = Js.String.unsafeReplaceBy2 re matchFn str

let () = Js.log replaced (* prints "42" *)
]}

@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace#Specifying_a_function_as_a_parameter> MDN
*)
external unsafeReplaceBy2 : Js_re.t -> (t -> t -> t -> int -> t -> t [@bs.uncurry]) -> t = "replace" [@@bs.send.pipe: t]

(** returns a new string with some or all matches of a pattern with three sets of capturing
parentheses replaced by the value returned from the given function.
The function receives as its parameters the matched string, the captured strings,
the offset at which the match begins, and the whole string being matched.

@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace#Specifying_a_function_as_a_parameter> MDN
*)
external unsafeReplaceBy3 : Js_re.t -> (t -> t -> t -> t -> int -> t -> t [@bs.uncurry]) -> t = "replace" [@@bs.send.pipe: t]

external search : Js_re.t -> int = "" [@@bs.send.pipe: t]

external slice : from:int -> to_:int ->  t = "" [@@bs.send.pipe: t]
external sliceToEnd : from:int ->  t = "slice" [@@bs.send.pipe: t]

external split : t -> t array  = "" [@@bs.send.pipe: t]
external splitAtMost: t -> limit:int -> t array = "split" [@@bs.send.pipe: t]
external splitLimited : t -> int -> t array = "split" [@@bs.send.pipe: t]
[@@ocaml.deprecated "Please use splitAtMost"]
external splitByRe : Js_re.t ->  t array = "split" [@@bs.send.pipe: t]
external splitByReAtMost : Js_re.t -> limit:int ->  t array = "split" [@@bs.send.pipe: t]
external splitRegexpLimited : Js_re.t -> int ->  t array = "" [@@bs.send.pipe: t]
[@@ocaml.deprecated "Please use splitByReAtMost"]

external startsWith : t -> bool = "" [@@bs.send.pipe: t] (** ES2015 *)
external startsWithFrom : t -> int -> bool = "startsWith" [@@bs.send.pipe: t] (** ES2015 *)

external substr : from:int -> t = "" [@@bs.send.pipe: t]
external substrAtMost : from:int -> length:int -> t = "substr" [@@bs.send.pipe: t]

external substring : from:int -> to_:int ->  t = "" [@@bs.send.pipe: t]
external substringToEnd : from:int ->  t = "substring" [@@bs.send.pipe: t]

external toLowerCase : t = "" [@@bs.send.pipe: t]
external toLocaleLowerCase : t = "" [@@bs.send.pipe: t]
external toUpperCase : t = "" [@@bs.send.pipe: t]
external toLocaleUpperCase : t = "" [@@bs.send.pipe: t]

external trim : t = "" [@@bs.send.pipe: t]

(* HTML wrappers *)
external anchor : t -> t = "" [@@bs.send.pipe: t] (** ES2015 *)
external link : t -> t = "" [@@bs.send.pipe: t] (** ES2015 *)

external castToArrayLike : t -> t Js_array.array_like = "%identity" 
(* FIXME: we should not encourage people to use [%identity], better
    to provide something using [@@bs.val] so that we can track such 
    casting
*)
