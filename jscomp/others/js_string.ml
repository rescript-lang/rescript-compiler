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

[@@@warning "-103"]

type t = string

(**
  `make value` converts the given value to a string

  ```
  make 3.5 = "3.5";;
  make [|1;2;3|]) = "1,2,3";;
  ```
*)
external make : 'a -> t = "String" [@@bs.val]

(**
  `fromCharCode n`
  creates a string containing the character corresponding to that number; _n_ ranges from 0 to 65535. If out of range, the lower 16 bits of the value are used. Thus, `fromCharCode 0x1F63A` gives the same result as `fromCharCode 0xF63A`.

  ```
  fromCharCode 65 = "A";;
  fromCharCode 0x3c8 = {js|ψ|js};;
  fromCharCode 0xd55c = {js|한|js};;
  fromCharCode -64568 = {js|ψ|js};;
  ```
*)
external fromCharCode : int -> t = "String.fromCharCode" [@@bs.val]

external fromCharCodeMany : int array -> t = "String.fromCharCode" [@@bs.val] [@@bs.splice]
(**
  `fromCharCodeMany [|n1;n2;n3|]` creates a string from the characters corresponding to the given numbers, using the same rules as `fromCharCode`.

  ```
  fromCharCodeMany([|0xd55c, 0xae00, 33|]) = {js|한글!|js};;
  ```
*)

(**
  `fromCodePoint n`
  creates a string containing the character corresponding to that numeric code point. If the number is not a valid code point, **raises** `RangeError`. Thus, `fromCodePoint 0x1F63A` will produce a correct value, unlike `fromCharCode 0x1F63A`, and `fromCodePoint -5` will raise a `RangeError`.

  ```
  fromCodePoint 65 = "A";;
  fromCodePoint 0x3c8 = {js|ψ|js};;
  fromCodePoint 0xd55c = {js|한|js};;
  fromCodePoint 0x1f63a = {js|😺|js};;
  ```
*)
external fromCodePoint : int -> t = "String.fromCodePoint" [@@bs.val] (** ES2015 *)

(**
  `fromCharCodeMany [|n1;n2;n3|]` creates a string from the characters corresponding to the given code point numbers, using the same rules as `fromCodePoint`.

  ```
  fromCodePointMany([|0xd55c; 0xae00; 0x1f63a|]) = {js|한글😺|js}
  ```
*)
external fromCodePointMany : int array -> t = "String.fromCodePoint" [@@bs.val] [@@bs.splice] (** ES2015 *)


(* String.raw: ES2015, meant to be used with template strings, not directly *)

(**
  `length s` returns the length of the given string.

  ```
  length "abcd" = 4;;
  ```
*)
external length : t -> int = "length" [@@bs.get]

(**
  `get s n` returns as a string the character at the given index number. If `n` is out of range, this function returns `undefined`, so at some point this function may be modified to return `t option`.

  ```
  get "Reason" 0 = "R";;
  get "Reason" 4 = "o";;
  get {js|Rẽasöń|js} 5 = {js|ń|js};;
  ```
*)
external get : t -> int -> t = "" [@@bs.get_index]

(**
  `charAt n s` gets the character at index `n` within string `s`. If `n` is negative or greater than the length of `s`, returns the empty string. If the string contains characters outside the range `\u0000-\uffff`, it will return the first 16-bit value at that position in the string.

  ```
  charAt 0, "Reason" = "R"
  charAt( 12, "Reason") = "";
  charAt( 5, {js|Rẽasöń|js} = {js|ń|js}
  ```
*)
external charAt : int ->  t = "charAt" [@@bs.send.pipe: t]

(**
  `charCodeAt n s` returns the character code at position `n` in string `s`; the result is in the range 0-65535, unlke `codePointAt`, so it will not work correctly for characters with code points greater than or equal to `0x10000`.
  The return type is `float` because this function returns `NaN` if `n` is less than zero or greater than the length of the string.

  ```
  charCodeAt 0 {js|😺|js} returns 0xd83d
  codePointAt 0 {js|😺|js} returns Some 0x1f63a
  ```
*)
external charCodeAt : int -> float  = "charCodeAt" [@@bs.send.pipe: t]

(**
  `codePointAt n s` returns the code point at position `n` within string `s` as a `Some` value. The return value handles code points greater than or equal to `0x10000`. If there is no code point at the given position, the function returns `None`.

  ```
  codePointAt 1 {js|¿😺?|js} = Some 0x1f63a
  codePointAt 5 "abc" = None
  ```
*)
external codePointAt : int -> int option = "codePointAt" [@@bs.send.pipe: t] (** ES2015 *)

(**
  `concat append original` returns a new string with `append` added after `original`.

  ```
  concat "bell" "cow" = "cowbell";;
  ```
*)
external concat : t -> t = "concat" [@@bs.send.pipe: t]

(**
  `concat arr original` returns a new string consisting of each item of an array of strings added to the `original` string.

  ```
  concatMany [|"2nd"; "3rd"; "4th"|] "1st" = "1st2nd3rd4th";;
  ```
*)
external concatMany : t array -> t = "concat" [@@bs.send.pipe: t] [@@bs.splice]

(**
  ES2015:
  `endsWith substr str` returns `true` if the `str` ends with `substr`, `false` otherwise.

  ```
  endsWith "Script" "ReScript" = true;;
  endsWith "Script" "ReShoes" = false;;
  ```
*)
external endsWith : t -> bool = "endsWith" [@@bs.send.pipe: t]

(**
  `endsWithFrom ending len str` returns `true` if the first `len` characters of `str` end with `ending`, `false` otherwise. If `n` is greater than or equal to the length of `str`, then it works like `endsWith`. (Honestly, this should have been named `endsWithAt`, but oh well.)

  ```
  endsWithFrom "cd" 4 "abcd" = true;;
  endsWithFrom "cd" 3 "abcde" = false;;
  endsWithFrom "cde" 99 "abcde" = true;;
  endsWithFrom "ple" 7 "example.dat" = true;;
  ```
*)
external endsWithFrom : t -> int -> bool = "endsWith" [@@bs.send.pipe: t] (** ES2015 *)

(**
  `includes searchValue s` returns `true` if `searchValue` is found anywhere within `s`, `false` otherwise.

  ```
  includes "gram" "programmer" = true;;
  includes "er" "programmer" = true;;
  includes "pro" "programmer" = true;;
  includes "xyz" "programmer" = false;;
  ```
*)
external includes : t -> bool = "includes" [@@bs.send.pipe: t] (** ES2015 *)

(**
  `includes searchValue start s` returns `true` if `searchValue` is found anywhere within `s` starting at character number `start` (where 0 is the first character), `false` otherwise.

  ```
  includesFrom "gram" 1 "programmer" = true;;
  includesFrom "gram" 4 "programmer" = false;;
  includesFrom {js|한|js} 1 {js|대한민국|js} = true;;
  ```
*)
external includesFrom : t -> int -> bool = "includes" [@@bs.send.pipe: t] (** ES2015 *)

(**
  `indexOf searchValue s` returns the position at which `searchValue` was first found within `s`, or `-1` if `searchValue` is not in `s`.

  ```
  indexOf "ok" "bookseller" = 2;;
  indexOf "sell" "bookseller" = 4;;
  indexOf "ee" "beekeeper" = 1;;
  indexOf "xyz" "bookseller" = -1;;
  ```
*)
external indexOf : t -> int = "indexOf" [@@bs.send.pipe: t]

(**
  `indexOfFrom searchValue start s` returns the position at which `searchValue` was found within `s` starting at character position `start`, or `-1` if `searchValue` is not found in that portion of `s`. The return value is relative to the beginning of the string, no matter where the search started from.

  ```
  indexOfFrom "ok" 1 "bookseller" = 2;;
  indexOfFrom "sell" 2 "bookseller" = 4;;
  indexOfFrom "sell" 5 "bookseller" = -1;;
  indexOf "xyz" "bookseller" = -1;;
  ```
*)
external indexOfFrom : t -> int -> int = "indexOf" [@@bs.send.pipe: t]

(**
  `lastIndexOf searchValue s` returns the position of the _last_ occurrence of `searchValue` within `s`, searching backwards from the end of the string. Returns `-1` if `searchValue` is not in `s`. The return value is always relative to the beginning of the string.

  ```
  lastIndexOf "ok" "bookseller" = 2;;
  lastIndexOf "ee" "beekeeper" = 4;;
  lastIndexOf "xyz" "abcdefg" = -1;;
  ```
*)
external lastIndexOf : t -> int = "lastIndexOf" [@@bs.send.pipe: t]

(**
  `lastIndexOfFrom searchValue start s` returns the position of the _last_ occurrence of `searchValue` within `s`, searching backwards from the given `start` position. Returns `-1` if `searchValue` is not in `s`. The return value is always relative to the beginning of the string.

  ```
  lastIndexOfFrom "ok" 6 "bookseller" = 2;;
  lastIndexOfFrom "ee" 8 "beekeeper" = 4;;
  lastIndexOfFrom "ee" 3 "beekeeper" = 1;;
  lastIndexOfFrom "xyz" 4 "abcdefg" = -1;;
  ```
*)
external lastIndexOfFrom : t -> int -> int = "lastIndexOf" [@@bs.send.pipe: t]


(* extended by ECMA-402 *)
(**
  `localeCompare comparison reference` returns

  - a negative value if `reference` comes before `comparison` in sort order
  - zero if `reference` and `comparison` have the same sort order
  - a positive value if `reference` comes after `comparison` in sort order

  ```
  (localeCompare "ant" "zebra") > 0.0;;
  (localeCompare "zebra" "ant") < 0.0;;
  (localeCompare "cat" "cat") = 0.0;;
  (localeCompare "cat" "CAT") > 0.0;;
  ```
*)
external localeCompare : t -> float = "localeCompare" [@@bs.send.pipe: t]

(**
  `match regexp str` matches a string against the given `regexp`. If there is no match, it returns `None`.
  For regular expressions without the `g` modifier, if there is a match, the return value is `Some array` where the array contains:

  - The entire matched string
  - Any capture groups if the `regexp` had parentheses

  For regular expressions with the `g` modifier, a matched expression returns `Some array` with all the matched substrings and no capture groups.

  ```
  match [%re "/b`aeiou`t/"] "The better bats" = Some [|"bet"|]
  match [%re "/b`aeiou`t/g"] "The better bats" = Some [|"bet";"bat"|]
  match [%re "/(\d+)-(\d+)-(\d+)/"] "Today is 2018-04-05." =
    Some [|"2018-04-05"; "2018"; "04"; "05"|]
  match [%re "/b`aeiou`g/"] "The large container." = None
  ```
*)
external match_ : Js_re.t -> t array option = "match" [@@bs.send.pipe: t] [@@bs.return {null_to_opt}]

(**
  `normalize str` returns the normalized Unicode string using Normalization Form Canonical (NFC) Composition.

  Consider the character `ã`, which can be represented as the single codepoint `\u00e3` or the combination of a lower case letter A `\u0061` and a combining tilde `\u0303`. Normalization ensures that both can be stored in an equivalent binary representation.

  **see** [Unicode technical report for details](https://www.unicode.org/reports/tr15/tr15-45.html)
*)
external normalize : t = "normalize" [@@bs.send.pipe: t] (** ES2015 *)

(**
  `normalize str form` (ES2015) returns the normalized Unicode string using the specified form of normalization, which may be one of:

  - "NFC" — Normalization Form Canonical Composition.
  - "NFD" — Normalization Form Canonical Decomposition.
  - "NFKC" — Normalization Form Compatibility Composition.
  - "NFKD" — Normalization Form Compatibility Decomposition.

  **see** [Unicode technical report for details](https://www.unicode.org/reports/tr15/tr15-45.html)
*)
external normalizeByForm : t -> t = "normalize" [@@bs.send.pipe: t]

(**
  `repeat n s` returns a string that consists of `n` repetitions of `s`. Raises `RangeError` if `n` is negative.

  ```
  repeat 3 "ha" = "hahaha"
  repeat 0 "empty" = ""
  ```
*)
external repeat : int -> t = "repeat" [@@bs.send.pipe: t] (** ES2015 *)

(**
  `replace substr newSubstr string` returns a new string which is
  identical to `string` except with the first matching instance of `substr`
  replaced by `newSubstr`.

  `substr` is treated as a verbatim string to match, not a regular
  expression.

  ```
  replace "old" "new" "old string" = "new string"
  replace "the" "this" "the cat and the dog" = "this cat and the dog"
  ```
*)
external replace : t ->  t ->  t = "replace" [@@bs.send.pipe: t]

(**
  `replaceByRe regex replacement string` returns a new string where occurrences matching `regex`
  have been replaced by `replacement`.

  ```
  replaceByRe [%re "/`aeiou`/g"] "x" "vowels be gone" = "vxwxls bx gxnx"
  replaceByRe [%re "/(\w+) (\w+)/"] "$2, $1" "Juan Fulano" = "Fulano, Juan"
  ```
*)
external replaceByRe : Js_re.t ->  t ->  t = "replace" [@@bs.send.pipe: t]

(**
  **return** a new string with some or all matches of a pattern with no capturing
  parentheses replaced by the value returned from the given function.
  The function receives as its parameters the matched string, the offset at which the
  match begins, and the whole string being matched

  ```
  let str = "beautiful vowels"
  let re = [%re "/`aeiou`/g"]
  let matchFn matchPart offset wholeString =
    Js.String.toUpperCase matchPart

  let replaced = Js.String.unsafeReplaceBy0 re matchFn str

  let () = Js.log replaced (* prints "bEAUtifUl vOwEls" *)
  ```

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace#Specifying_a_function_as_a_parameter)
*)
external unsafeReplaceBy0 : Js_re.t -> (t -> int -> t -> t [@bs.uncurry]) -> t = "replace" [@@bs.send.pipe: t]

(**
  **return** a new string with some or all matches of a pattern with one set of capturing
  parentheses replaced by the value returned from the given function.
  The function receives as its parameters the matched string, the captured string,
  the offset at which the match begins, and the whole string being matched.

  ```
  let str = "increment 23"
  let re = [%re "/increment (\d+)/g"]
  let matchFn matchPart p1 offset wholeString =
    wholeString ^ " is " ^ (string_of_int ((int_of_string p1) + 1))

  let replaced = Js.String.unsafeReplaceBy1 re matchFn str

  let () = Js.log replaced (* prints "increment 23 is 24" *)
  ```

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace#Specifying_a_function_as_a_parameter)
*)
external unsafeReplaceBy1 : Js_re.t -> (t -> t -> int -> t -> t [@bs.uncurry]) -> t = "replace" [@@bs.send.pipe: t]

(**
  **return** a new string with some or all matches of a pattern with two sets of capturing
  parentheses replaced by the value returned from the given function.
  The function receives as its parameters the matched string, the captured strings,
  the offset at which the match begins, and the whole string being matched.

  ```
  let str = "7 times 6"
  let re = [%re "/(\d+) times (\d+)/"]
  let matchFn matchPart p1 p2 offset wholeString =
    string_of_int ((int_of_string p1) * (int_of_string p2))

  let replaced = Js.String.unsafeReplaceBy2 re matchFn str

  let () = Js.log replaced (* prints "42" *)
  ```

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace#Specifying_a_function_as_a_parameter)
*)
external unsafeReplaceBy2 : Js_re.t -> (t -> t -> t -> int -> t -> t [@bs.uncurry]) -> t = "replace" [@@bs.send.pipe: t]

(**
  **return** a new string with some or all matches of a pattern with three sets of capturing
  parentheses replaced by the value returned from the given function.
  The function receives as its parameters the matched string, the captured strings,
  the offset at which the match begins, and the whole string being matched.

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace#Specifying_a_function_as_a_parameter)
*)
external unsafeReplaceBy3 : Js_re.t -> (t -> t -> t -> t -> int -> t -> t [@bs.uncurry]) -> t = "replace" [@@bs.send.pipe: t]

(**
  `search regexp str` returns the starting position of the first match of `regexp` in the given `str`, or -1 if there is no match.

  ```
  search [%re "/\d+/"] "testing 1 2 3" = 8;;
  search [%re "/\d+/"] "no numbers" = -1;;
  ```
*)
external search : Js_re.t -> int = "search" [@@bs.send.pipe: t]

(**
  `slice from:n1 to_:n2 str` returns the substring of `str` starting at character `n1` up to but not including `n2`

  If either `n1` or `n2` is negative, then it is evaluated as `length str - n1` (or `length str - n2`.

  If `n2` is greater than the length of `str`, then it is treated as `length str`.

  If `n1` is greater than `n2`, `slice` returns the empty string.

  ```
  slice ~from:2 ~to_:5 "abcdefg" == "cde";;
  slice ~from:2 ~to_:9 "abcdefg" == "cdefg";;
  slice ~from:(-4) ~to_:(-2) "abcdefg" == "de";;
  slice ~from:5 ~to_:1 "abcdefg" == "";;
  ```
*)
external slice : from:int -> to_:int ->  t = "slice" [@@bs.send.pipe: t]

(**
  `sliceToEnd from: n str` returns the substring of `str` starting at character `n` to the end of the string

  If `n` is negative, then it is evaluated as `length str - n`.

  If `n` is greater than the length of `str`, then `sliceToEnd` returns the empty string.

  ```
  sliceToEnd ~from: 4 "abcdefg" == "efg";;
  sliceToEnd ~from: (-2) "abcdefg" == "fg";;
  sliceToEnd ~from: 7 "abcdefg" == "";;
  ```
*)
external sliceToEnd : from:int ->  t = "slice" [@@bs.send.pipe: t]

(**
  `split delimiter str` splits the given `str` at every occurrence of `delimiter` and returns an
  array of the resulting substrings.

  ```
  split "-" "2018-01-02" = [|"2018"; "01"; "02"|];;
  split "," "a,b,,c" = [|"a"; "b"; ""; "c"|];;
  split "::" "good::bad as great::awful" = [|"good"; "bad as great"; "awful"|];;
  split ";" "has-no-delimiter" = [|"has-no-delimiter"|];;
  ```
*)
external split : t -> t array  = "split" [@@bs.send.pipe: t]

(**
  `splitAtMost delimiter ~limit: n str` splits the given `str` at every occurrence of `delimiter` and returns an array of the first `n` resulting substrings. If `n` is negative or greater than the number of substrings, the array will contain all the substrings.

  ```
  splitAtMost "/" ~limit: 3 "ant/bee/cat/dog/elk" = [|"ant"; "bee"; "cat"|];;
  splitAtMost "/" ~limit: 0 "ant/bee/cat/dog/elk" = [| |];;
  splitAtMost "/" ~limit: 9 "ant/bee/cat/dog/elk" = [|"ant"; "bee"; "cat"; "dog"; "elk"|];;
  ```
*)
external splitAtMost: t -> limit:int -> t array = "split" [@@bs.send.pipe: t]

(**
  Deprecated - Please use `splitAtMost`
*)
external splitLimited : t -> int -> t array = "split" [@@bs.send.pipe: t]
[@@deprecated "Please use splitAtMost"]

(**
  `splitByRe regex str` splits the given `str` at every occurrence of `regex` and returns an
  array of the resulting substrings.

  ```
  splitByRe [%re "/\s*[,;]\s*/"] "art; bed , cog ;dad" = [|Some "art"; Some "bed"; Some "cog"; Some "dad"|];;
  splitByRe [%re "/[,;]/"] "has:no:match" = [|Some "has:no:match"|];;
  splitByRe [%re "/(#)(:)?/"] "a#b#:c" = [|Some "a"; Some "#"; None; Some "b"; Some "#"; Some ":"; Some "c"|];;
  ```
*)
external splitByRe : Js_re.t ->  t option array = "split" [@@bs.send.pipe: t]

(**
  `splitByReAtMost regex ~limit: n str` splits the given `str` at every occurrence of `regex` and returns an
  array of the first `n` resulting substrings. If `n` is negative or greater than the number of substrings, the array will contain all the substrings.

  ```
  splitByReAtMost [%re "/\s*:\s*/"] ~limit: 3 "one: two: three: four" = [|Some "one"; Some "two"; Some "three"|];;
  splitByReAtMost [%re "/\s*:\s*/"] ~limit: 0 "one: two: three: four" = [| |];;
  splitByReAtMost [%re "/\s*:\s*/"] ~limit: 8 "one: two: three: four" = [|Some "one"; Some "two"; Some "three"; Some "four"|];;
  splitByReAtMost [%re "/(#)(:)?/"] ~limit:3 "a#b#:c" = [|Some "a"; Some "#"; None|];;
  ```
*)
external splitByReAtMost : Js_re.t -> limit:int ->  t option array = "split" [@@bs.send.pipe: t]

(**
  Deprecated - Please use `splitByReAtMost`
*)
external splitRegexpLimited : Js_re.t -> int ->  t array = "split" [@@bs.send.pipe: t]
[@@deprecated "Please use splitByReAtMost"]

(**
  ES2015:
  `startsWith substr str` returns `true` if the `str` starts with `substr`, `false` otherwise.

  ```
  startsWith "Re" "ReScript" = true;;
  startsWith "" "ReScript" = true;;
  startsWith "Re" "JavaScript" = false;;
  ```
*)
external startsWith : t -> bool = "startsWith" [@@bs.send.pipe: t]

(**
  ES2015:
  `startsWithFrom substr n str` returns `true` if the `str` starts with `substr` starting at position `n`, `false` otherwise. If `n` is negative, the search starts at the beginning of `str`.

  ```
  startsWithFrom "cri" 3 "ReScript" = true;;
  startsWithFrom "" 3 "ReScript" = true;;
  startsWithFrom "Re" 2 "JavaScript" = false;;
  ```
*)
external startsWithFrom : t -> int -> bool = "startsWith" [@@bs.send.pipe: t]

(**
  `substr ~from: n str` returns the substring of `str` from position `n` to the end of the string.

  If `n` is less than zero, the starting position is the length of `str` - `n`.

  If `n` is greater than or equal to the length of `str`, returns the empty string.

  ```
  substr ~from: 3 "abcdefghij" = "defghij"
  substr ~from: (-3) "abcdefghij" = "hij"
  substr ~from: 12 "abcdefghij" = ""
  ```
*)
external substr : from:int -> t = "substr" [@@bs.send.pipe: t]

(**
  `substrAtMost ~from: pos ~length: n str` returns the substring of `str` of length `n` starting at position `pos`.

  If `pos` is less than zero, the starting position is the length of `str` - `pos`.

  If `pos` is greater than or equal to the length of `str`, returns the empty string.

  If `n` is less than or equal to zero, returns the empty string.

  ```
  substrAtMost ~from: 3 ~length: 4 "abcdefghij" = "defghij"
  substrAtMost ~from: (-3) ~length: 4 "abcdefghij" = "hij"
  substrAtMost ~from: 12 ~ length: 2 "abcdefghij" = ""
  ```
*)
external substrAtMost : from:int -> length:int -> t = "substr" [@@bs.send.pipe: t]

(**
  `substring ~from: start ~to_: finish str` returns characters `start` up to but not including `finish` from `str`.

  If `start` is less than zero, it is treated as zero.

  If `finish` is zero or negative, the empty string is returned.

  If `start` is greater than `finish`, the start and finish points are swapped.

  ```
  substring ~from: 3 ~to_: 6 "playground" = "ygr";;
  substring ~from: 6 ~to_: 3 "playground" = "ygr";;
  substring ~from: 4 ~to_: 12 "playground" = "ground";;
  ```
*)
external substring : from:int -> to_:int ->  t = "substring" [@@bs.send.pipe: t]

(**
  `substringToEnd ~from: start str` returns the substring of `str` from position `start` to the end.

  If `start` is less than or equal to zero, the entire string is returned.

  If `start` is greater than or equal to the length of `str`, the empty string is returned.

  ```
  substringToEnd ~from: 4 "playground" = "ground";;
  substringToEnd ~from: (-3) "playground" = "playground";;
  substringToEnd ~from: 12 "playground" = "";
  ```
*)
external substringToEnd : from:int ->  t = "substring" [@@bs.send.pipe: t]

(**
  `toLowerCase str` converts `str` to lower case using the locale-insensitive case mappings in the Unicode Character Database. Notice that the conversion can give different results depending upon context, for example with the Greek letter sigma, which has two different lower case forms when it is the last character in a string or not.

  ```
  toLowerCase "ABC" = "abc";;
  toLowerCase {js|ΣΠ|js} = {js|σπ|js};;
  toLowerCase {js|ΠΣ|js} = {js|πς|js};;
  ```
*)
external toLowerCase : t = "toLowerCase" [@@bs.send.pipe: t]

(**
  `toLocaleLowerCase str` converts `str` to lower case using the current locale
*)
external toLocaleLowerCase : t = "toLocaleLowerCase" [@@bs.send.pipe: t]

(**
  `toUpperCase str` converts `str` to upper case using the locale-insensitive case mappings in the Unicode Character Database. Notice that the conversion can expand the number of letters in the result; for example the German `ß` capitalizes to two `S`es in a row.

  ```
  toUpperCase "abc" = "ABC";;
  toUpperCase {js|Straße|js} = {js|STRASSE|js};;
  toLowerCase {js|πς|js} = {js|ΠΣ|js};;
  ```
*)
external toUpperCase : t = "toUpperCase" [@@bs.send.pipe: t]

(**
  `toLocaleUpperCase str` converts `str` to upper case using the current locale
*)
external toLocaleUpperCase : t = "toLocaleUpperCase" [@@bs.send.pipe: t]

(**
  `trim str` returns a string that is `str` with whitespace stripped from both ends. Internal whitespace is not removed.

  ```
  trim "   abc def   " = "abc def"
  trim "\n\r\t abc def \n\n\t\r " = "abc def"
  ```
*)
external trim : t = "trim" [@@bs.send.pipe: t]

(* HTML wrappers *)

(**
  `anchor anchorName anchorText` creates a string with an HTML `<a>` element with `name` attribute of `anchorName` and `anchorText` as its content.

  ```
  anchor "page1" "Page One" = "<a name=\"page1\">Page One</a>"
  ```
*)
external anchor : t -> t = "anchor" [@@bs.send.pipe: t] (** ES2015 *)

(**
  `link urlText linkText` creates a string withan HTML `<a>` element with `href` attribute of `urlText` and `linkText` as its content.

  ```
  link "page2.html" "Go to page two" = "<a href=\"page2.html\">Go to page two</a>"
  ```
*)
external link : t -> t = "link" [@@bs.send.pipe: t] (** ES2015 *)

external castToArrayLike : t -> t Js_array2.array_like = "%identity"
(* FIXME: we should not encourage people to use [%identity], better
    to provide something using [@@bs.val] so that we can track such
    casting
*)
