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

external make : 'a -> t = "String" [@@bs.val]
external fromCharCode : int -> t = "String.fromCharCode" [@@bs.val]
external fromCharCodeMany : int array -> t = "String.fromCharCode" [@@bs.val] [@@bs.splice]
external fromCodePoint : int -> t = "String.fromCodePoint" [@@bs.val] (** ES2015 *)
external fromCodePointMany : int array -> t = "String.fromCodePoint" [@@bs.val] [@@bs.splice] (** ES2015 *)
(* String.raw: ES2015, meant to be used with template strings, not directly *)

external length : t -> int = "" [@@bs.get]
external get : t -> int -> t = "" [@@bs.get_index]

external charAt : int ->  t = "" [@@bs.send.pipe: t]
external charCodeAt : int -> float  = "" [@@bs.send.pipe: t]

(** type it as [float] due to that it may return NAN  
    different from [charCodeAt]
    {[
        "\u{01003f}".charCodeAt(0) - 55296
        "\u{01003f}".codePointAt(0) - 65599
    ]}
*)
external codePointAt : int -> int option = "" [@@bs.send.pipe: t] [@@bs.return {undefined_to_opt}] (** ES2015 *)

external concat : t -> t = "" [@@bs.send.pipe: t]
external concatMany : t array -> t = "concat" [@@bs.send.pipe: t] [@@bs.splice]

external endsWith : t -> bool = "" [@@bs.send.pipe: t] 
(** ES2015:
    Sample usage: {[ x |> Js.String.endsWith ".ml"]} 
*)

external endsWithFrom : t -> int -> bool = "endsWith" [@@bs.send.pipe: t] (** ES2015 *)

external includes : t -> bool = "" [@@bs.send.pipe: t] (** ES2015 *)
external includesFrom : t -> int -> bool = "includes" [@@bs.send.pipe: t] (** ES2015 *)

external indexOf : t -> int = "" [@@bs.send.pipe: t]
external indexOfFrom : t -> int -> int = "indexOf" [@@bs.send.pipe: t]

external lastIndexOf : t -> int = "" [@@bs.send.pipe: t]
external lastIndexOfFrom : t -> int -> int = "lastIndexOf" [@@bs.send.pipe: t]

external localeCompare : t -> float = "" [@@bs.send.pipe: t]
(* extended by ECMA-402 *)

external match_ : Js_re.t -> t array option = "match" [@@bs.send.pipe: t] [@@bs.return {null_to_opt}]

external normalize : t = "" [@@bs.send.pipe: t] (** ES2015 *)
external normalizeByForm : t -> t = "normalize" [@@bs.send.pipe: t] (** ES2015 *)

external repeat : int -> t = "" [@@bs.send.pipe: t] (** ES2015 *)

external replace : t ->  t ->  t = "" [@@bs.send.pipe: t]

(** [replace substr newSubstr string] returns a new string which is
identical to [string] except with the first matching instance of [substr]
replaced by [newSubstr].

[substr] is treated as a verbatim string to match, not a regular
expression.

@example {[
let result = Js.String.replace "a" "b" "aaaa"
let () = Js.log result (* prints "baaa" *)
]}
*)

external replaceByRe : Js_re.t ->  t ->  t = "replace" [@@bs.send.pipe: t]

(** returns a new string with some or all matches of a pattern replaced by the
value returned from the given function

@example {[
let str = "hello world!"
let re = [%re "/hello/g"]
let replaced = Js.String.unsafeReplaceBy0 re (fun match offset whole -> "hi")

let () = Js.log replaced (* prints "hi world!" *)
]}

@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace#Specifying_a_function_as_a_parameter> MDN
*)
external unsafeReplaceBy0 : Js_re.t -> (t -> int -> t -> t [@bs.uncurry]) -> t = "replace" [@@bs.send.pipe: t]

(** returns a new string with some or all matches of a pattern replaced by the
value returned from the given function

@example {[
let str = "hello world!"
let re = [%re "/hello/g"]
let replaced = Js.String.unsafeReplaceBy1 re (fun match p1 offset whole -> "hi")

let () = Js.log replaced (* prints "hi world!" *)
]}

@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace#Specifying_a_function_as_a_parameter> MDN
*)
external unsafeReplaceBy1 : Js_re.t -> (t -> t -> int -> t -> t [@bs.uncurry]) -> t = "replace" [@@bs.send.pipe: t]

(** returns a new string with some or all matches of a pattern replaced by the
value returned from the given function

@example {[
let str = "hello world!"
let re = [%re "/hello/g"]
let replaced = Js.String.unsafeReplaceBy2 re (fun match p1 p2 offset whole -> "hi")

let () = Js.log replaced (* prints "hi world!" *)
]}

@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace#Specifying_a_function_as_a_parameter> MDN
*)
external unsafeReplaceBy2 : Js_re.t -> (t -> t -> t -> int -> t -> t [@bs.uncurry]) -> t = "replace" [@@bs.send.pipe: t]

(** returns a new string with some or all matches of a pattern replaced by the
value returned from the given function

@example {[
let str = "hello world!"
let re = [%re "/hello/g"]
let replaced = Js.String.unsafeReplaceBy3 re (fun match p1 p2 p3 offset whole -> "hi")

let () = Js.log replaced (* prints "hi world!" *)
]}

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
