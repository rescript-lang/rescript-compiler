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

(** Efficient JSON handling

This module has four aspects to it:
- Parsing, which turns a JSON string into an encoded JSON data structure
- Stringificaiton, which produces a JSON string from an encoded JSON data structure
- Encoding, which is the process of construction a JSON data strcture
- Decoding, which is the process of deconstructing a JSON data structure

{3 Parsing}

{! parse} and {! exnParse} will both (try to) parse a JSON string into a JSON
data structure ({! Js.Json.t}), but behhaves differently when encountering a
parse error. [exnParse] will raise a [SyntaxError], while [parse] will return
a [Js.Json.t result] indicating whether or not the parsing succeeded. There's
not much more to it: [string] in, [Js.Json.t] out.

The parsed result, and encoded JSON data structure, then needs to be decoded to
avtually be usable. See {!section:Decoding} below.

{3 Stringification}

Stringificaiton is the exact reverse of parsing. {! stringify} and {! stringifyAny}
both technically do the same thing, but where [stringifyAny] will take any value
and try to do its best with it, retuning a [string option], [stringify] on the
other handuses the type system to guarantee success, but requires that the data
has been encoded in a JSON data structure first. See {!section:Encoding} below.

{3 Encoding}

Encoding creates a JSON data structure which can stringified directly with
{! stringify} or passed to other APIs requiring a typed JSON data structure. Or
you could just go straight to decoding it again, if that's your thing. ENcoding
functions are in the {! Encode} module.

{3 Decoding}

Decoding is a more complex process, due to the highly dynamic nature of JSON
data structures. There are several ways to decode a JSON data structure,
depending on your needs. This module provides two fairly low level methods
of decoding, the assertive, more convenient but also more rigid {! Decode}
module and the more flexible but cumbersome {! reifyType} and {! test} functions.
The third way is to use a opinionated third-party APIs that makes other tradeoffs.

@example {[
(* Parsing a JSON string using Js.Json.parse *)
open Js.Json

let arrayOfInts str
  match parse str with
  | Ok value ->
    match Decode.(array int value)
    | Ok arr -> arr
    | Error _ -> []
  | Error message -> failWith message

(* prints `[3, 2, 1]` *)
let _ = Js.log \@\@ arrayOfInts "[1, 2, 3]" |> Js.Array.reverse
]}

@example {[
(* Stringifying a value using Js.Json.stringifyAny *)
open Js.Json

(* prints `null` *)
let _ =
  match stringifyAny Js.null with
  | Some str -> Js.log str
  | None -> Js.log "Unable to stringify value"
]}

@example {[
(* Encoding a JSON data structure using Js.Json.Encode *)
open Js.Json

(* prints ["foo", "bar"] *)
let _ =
  [| "foo", "bar" |]
  |> Encode.stringArray
  |> stringify
  |> Js.log

(* prints ["foo", "bar"] *)
let _ =
  [| "foo", "bar" |]
  |> Js.Array.map Encode.int
  |> Encode.array
  |> stringify
  |> Js.log
]}

@example {[
(* Decoding a fixed JSON data structure using Js.Json.Decode *)
open Js.Json

let mapJsonObjectString f decoder encoder str =
  match parse str with
  | Ok json ->
    match Decode.(dict decoder json) with
    | Ok dict ->
      dict |> Js.Dict.map f
           |> Js.Dict.map encoder
           |> Encode.dict
           |> Js.stringify
    | Error _ -> []
  | Error _ -> []

let sum ns =
  Array.fold_left (+) 0

(* prints `{ "foo": 6, "bar": 24 }` *)
let _ =
  Js.log \@\@
    mapJsonObjectString sun Decode.(array int) Encode.int {|
      {
        "foo": [1, 2, 3],
        "bar": [9, 8, 7]
      }
    |} 
]}

@example {[
(* Decoding a highly dynamic JSON data structure using reifyType *)
open Js.Json

let getIds s =
  let json = 
    try parse s with
    | _ -> failwith "Error parsing JSON string"
  in 
  match reifyType json with
  | (Object, value) ->
    (* In this branch, compiler infer value : t Js.Dict.t *)
    begin match Js.Dict.get value "ids" with
    | Some ids -> 
      begin match reifyType ids with
      | (Array, ids) -> 
        (* In this branch compiler infer ids : t array *)
        ids
      | _ -> failWith "Expected an array"
      end 
    | None -> failWith "Expected an `ids` property"
    end 
  | _ -> failWith "Expected an object"

(* prints `1, 2, 3` *)
let _ =
  Js.log \@\@ getIds {| { "ids" : [1, 2, 3 ] } |} 
]}

@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON> MDN
*) 

(** {2 Types} *)

(** The JSON data structure *)
type t = Js.json

(** Underlying type of a JSON value *) 
type _ kind = 
  | String : Js_string.t kind
  | Number : float kind 
  | Object : t Js_dict.t kind 
  | Array : t array kind 
  | Boolean : Js.boolean kind
  | Null : Js_types.null_val kind


(** {2 Accessor} *)

val reifyType : t -> 'b kind * 'b 
(** [reifyType v] returns both type and underlying value

@example {[
(* Decoding a highly dynamic JSON data structure using reifyType *)
open Js.Json

let getIds s =
  let json = 
    try parse s with
    | _ -> failwith "Error parsing JSON string"
  in 
  match reifyType json with
  | (Object, value) ->
    (* In this branch, compiler infer value : t Js.Dict.t *)
    begin match Js.Dict.get value "ids" with
    | Some ids -> 
      begin match reifyType ids with
      | (Array, ids) -> 
        (* In this branch compiler infer ids : t array *)
        ids
      | _ -> failWith "Expected an array"
      end 
    | None -> failWith "Expected an `ids` property"
    end 
  | _ -> failWith "Expected an object"

(* prints `1, 2, 3` *)
let _ =
  Js.log \@\@ getIds {| { "ids" : [1, 2, 3 ] } |} 
]}
*) 

val test : 'a  -> 'b kind -> bool
(** [test v kind] returns true if [v] is of [kind] *)

(** {2 String conversion} *)

val parse : string -> (t, string) Bs.Result.result
(** [parse s] parses the string [s] into a JSON data structure

{b Returns} [Ok of t] if successful, [Error of string] if not.

@example {[
(* parse a simple JSON string *)

let json = 
  matcbh Js_json.parse {| "foo" |} with
  | Ok value -> value
  | Error message -> failwith message
in
match Js.Json.reifyType json in
| (Js.Json.String, value) -> Js.log value
| _ -> failWith "Expected a string"
]}

@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/parse> MDN
*)

external exnParse : string -> t = "JSON.parse" [@@bs.val]
(** [exnParse s] parses the string [s] into a JSON data structure

{b Returns} a JSON data structure

@raise SyntaxError if given string is not a valid JSON. Note [SyntaxError] is a JavaScript exception. 

@example {[
(* parse a simple JSON string *)

let json = 
  try Js_json.exnParse {| "foo" |}  with
  | _ -> failwith "Error parsing JSON string"
in
match Js.Json.reifyType json in
| (Js.Json.String, value) -> Js.log value
| _ -> failWith "Expected a string"
]}

@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/parse> MDN
*)

external stringify: t -> string = "JSON.stringify" [@@bs.val]
(** [stringify json] formats the JSON data structure as a string

{b Returns} the string representation of a given JSON data structure

@example {[
(* Creates and stringifies a simple JS object *)

let dict = Js.Dict.empty () in 
Js.Dict.set dict "name" (Js.Json.string "John Doe"); 
Js.Dict.set dict "age" (Js.Json.numberOfInt 30); 
Js.Dict.set dict "likes" 
  (Js.Json.stringArray [|"bucklescript";"ocaml";"js"|]);

Js.log \@\@ Js.Json.stringify (Js.Json.object_ dict) 
]}

@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify> MDN
*)

external stringifyAny : 'a -> string option = "JSON.stringify" [@@bs.val] [@@bs.return undefined_to_opt]
(** [stringifyAny value] formats any [value] into a JSON string

@example {[
(* prints `["foo", "bar"]` *)
Js.log \@\@ Js.Json.stringifyANy [| "foo"; "bar" |]
]}

@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify> MDN
*)


module Decode = Js_json_decode
module Encode = Js_json_encode


val decodeBoolean : t -> Js.boolean option
[@@deprecated "Please use `Decode.boolean` instead"]
(** @deprecated Please use {! Decode.boolean} instead *)

val decodeNumber : t -> float option
[@@deprecated "Please use `Decode.number` instead"]
(** @deprecated Please use {! Decode.float} or {! Decode.int} instead *)

val decodeString : t -> Js_string.t option
[@@deprecated "Please use `Decode.string` instead"]
(** @deprecated Please use {! Decode.string} instead *)

val decodeNull : t -> 'a Js_null.t option
[@@deprecated "Please use `Decode.nullAs` instead"]
(** @deprecated Please use {! Decode.nullAs} instead *)

val decodeArray : t -> t array option
[@@deprecated "Please use `Decode.array` instead"]
(** @deprecated Please use {! Decode.array} instead *)

val decodeObject : t -> t Js_dict.t option
[@@deprecated "Please use `Decode.dict` instead"]
(** @deprecated Please use {! Decode.dict} instead *)



external boolean : Js.boolean -> t = "%identity" 
[@@deprecated "Please use `Encode.boolean` instead"]
(** @deprecated Please use {! Encode.boolean} instead *)

external number : float -> t = "%identity"
[@@deprecated "Please use `Encode.float` instead"]
(** @deprecated Please use {! Encode.float} instead *)

external string : string -> t = "%identity"
[@@deprecated "Please use `Encode.string` instead"]
(** @deprecated Please use {! Encode.string} instead *)

external null : t = "" [@@bs.val]
[@@deprecated "Please use `Encode.null` instead"]
(** @deprecated Please use {! Encode.null} instead *)

external array_ : t array -> t = "%identity"
[@@deprecated "Please use `Encode.array` instead"]
(** @deprecated Please use {! Encode.array} instead *)

external object_ : t Js_dict.t -> t = "%identity"
[@@deprecated "Please use `Encode.object_` instead"]
(** @deprecated Please use {! Encode.object_} instead *)

external booleanArray : Js.boolean array -> t = "%identity"
[@@deprecated "Please use `Encode.booleanArray` instead"]
(** @deprecated Please use {! Encode.booleanArray} instead *)

external numberArray : float array -> t = "%identity"
[@@deprecated "Please use `Encode.numberArray` instead"]
(** @deprecated Please use {! Encode.numberArray} instead *)

external stringArray : string array -> t = "%identity"
[@@deprecated "Please use `Encode.stringArray` instead"]
(** @deprecated Please use {! Encode.stringArray} instead *)

external objectArray : t Js_dict.t array -> t = "%identity"
[@@deprecated "Please use `Encode.objectArray` instead"]
(** @deprecated Please use {! Encode.objectArray} instead *)



(**
@deprecated Please use {! reifyType} instead
*) 
val reify_type : 'a -> 'b kind * 'b 
[@@deprecated "Please use `reifyType`"]
