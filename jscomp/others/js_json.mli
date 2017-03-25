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

{! parse} and {! unsafeParse} will both (try to) parse a JSON string into a JSON
data structure ({! Js.Json.t}), but behhaves differently when encountering a
parse error. [unsafeParse] will raise a [SyntaxError], while [parse] will return
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
    match Decode.(array_ int value)
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
    mapJsonObjectString sun Decode.(array_ int) Encode.int {|
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
type t

(** Underlying type of a JSON value *) 
type _ kind = 
  | String : Js_string.t kind
  | Number : float kind 
  | Object : t Js_dict.t kind 
  | Array : t array kind 
  | Boolean : Js.boolean kind
  | Null : Js_types.null_val kind

(** temporary "polyfill" for the 4.03 [result] type *)
type ('a, 'e) result =
  | Ok of 'a
  | Error of 'e

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

val parse : string -> (t, string) result
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

external unsafeParse : string -> t = "JSON.parse" [@@bs.val]
(** [unsafeParse s] parses the string [s] into a JSON data structure

{b Returns} a JSON data structure

@raise SyntaxError if given string is not a valid JSON. Note [SyntaxError] is a JavaScript exception. 

@example {[
(* parse a simple JSON string *)

let json = 
  try Js_json.unsafeParse {| "foo" |}  with
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



module Decode: sig
  (** Provides a set of low level combinator primitives to decode Js.Json.t data
  structures

  A decoder combinator will return an [('a, string) result] where [Ok of 'a]
  indicates that the decoding succeeded and should contain the decoded value.
  [Error of string] indicates the decoding failed and should contain a [string]
  message explaining why.

  Decoders are designed to be combined to produce more complex decoders that can
  decode arbitrary data structures, though the emphasis for this library is for
  it to be {i possible} to decode any given data structure, not necessarily for
  it to be {i convenient}. For convenience you should look towards opinionated
  third-party libraries.
  *)

  type 'a decoder = t -> ('a, string) result
  (** The type of a decoder combinator *)

  val boolean : Js.boolean decoder
  (** Decodes a JSON value into a [Js.boolean]
      
  {b Returns} [Ok of Js.boolean] if the JSON value is a boolean, [Error of string] otherwise.
  
  @example {[
    open Js.Json
    let _ =

    (* prints [Ok Js.true_] *)
    Js.log \@\@ parse "true" |> Decode.boolean

    (* prints [Ok Js.false_] *)
    Js.log \@\@ parse "true" |> Decode.boolean

    (* prints [Error ...] *)
    Js.log \@\@ parse "123" |> Decode.boolean

    (* prints [Error ...] *)
    Js.log \@\@ parse "null" |> Decode.boolean
  ]}
  *)

  val float : float decoder
  (** Decodes a JSON value into a [float]
      
  {b Returns} [Ok of float] if the JSON value is a number, [Error of string] otherwise.
  
  @example {[
    open Js.Json
    let _ =

    (* prints [Ok 1.23] *)
    Js.log \@\@ parse "1.23" |> Decode.float

    (* prints [Ok 23.] *)
    Js.log \@\@ parse "23" |> Decode.float

    (* prints [Error ...] *)
    Js.log \@\@ parse "true" |> Decode.float

    (* prints [Error ...] *)
    Js.log \@\@ parse "null" |> Decode.float
  ]}
  *)

  val int : int decoder
  (** Decodes a JSON value into an [int]
      
  {b Returns} [Ok of int] if the JSON value is an integer, [Error of string] otherwise.
  
  @example {[
    open Js.Json
    let _ =

    (* prints [Ok 23] *)
    Js.log \@\@ parse "23" |> Decode.int

    (* prints [Error ...] *)
    Js.log \@\@ parse "1.23" |> Decode.int

    (* prints [Error ...] *)
    Js.log \@\@ parse "true" |> Decode.int

    (* prints [Error ...] *)
    Js.log \@\@ parse "null" |> Decode.int
  ]}
  *)

  val string : string decoder
  (** Decodes a JSON value into a [string]
      
  {b Returns} [Ok of string] if the JSON value is a string, [Error of string] otherwise.
  
  @example {[
    open Js.Json
    let _ =

    (* prints [Ok "foo"] *)
    Js.log \@\@ parse "\"foo\"" |> Decode.string

    (* prints [Error ...] *)
    Js.log \@\@ parse "1.23" |> Decode.string

    (* prints [Error ...] *)
    Js.log \@\@ parse "null" |> Decode.string
  ]}
  *)

  val nullable : 'a decoder -> 'a Js.null decoder
  (** Decodes a JSON value into an ['a Js.null]
      
  {b Returns} [Ok Js.null] if the JSON value is [null], [Ok of 'a Js.null] if the
  given decoder succeeds, [Error of string] otherwise.
  
  @example {[
    open Js.Json
    let _ =

    (* prints [Ok (Js.Null.return 23)] *)
    Js.log \@\@ parse "23" |> Decode.(nullable int)

    (* prints [Error ...] *)
    Js.log \@\@ parse "1.23" |> Decode.(nullable int)

    (* prints [Ok Js.null] *)
    Js.log \@\@ parse "null" |> Decode.(nullable int)
  ]}
  *)

  val nullAs : 'a -> 'a decoder
  (** Returns the given value if the JSON value is [null]
      
  {b Returns} [Ok of 'a] if the JSON value is [null], [Error of string] otherwise.
  
  @example {[
    open Js.Json
    let _ =

    (* prints [Error ...] *)
    Js.log \@\@ parse "\"x\"" |> Decode.nullAs "x"

    (* prints [Ok "x"] *)
    Js.log \@\@ parse "null" |> Decode.nullAs "x"

    (* prints [Ok None] *)
    Js.log \@\@ parse "null" |> Decode.nullAs None
  ]}
  *)

  val array_ : 'a decoder -> 'a array decoder
  (** Decodes a JSON array into an ['a array] using the given decoder on each array element
      
  {b Returns} [Ok of 'a array] if the JSON value is a JSON array and all its
  elements are successfully decoded, [Error of string] otherwise.
  
  @example {[
    open Js.Json
    let _ =

    (* prints [Ok [| 1; 2; 3 |]] *)
    Js.log \@\@ parse "[1, 2, 3]" |> Decode.(array_ int)

    (* prints [Error ...] *)
    Js.log \@\@ parse "[1, 2, "c"]" |> Decode.(array_ int)

    (* prints [Error ...] *)
    Js.log \@\@ parse "123" |> Decode.(array_ int)

    (* prints [Ok None] *)
    Js.log \@\@ parse "null" |> Decode.(array_ int)
  ]}
  *)

  val dict : 'a decoder -> 'a Js.Dict.t decoder
  (** Decodes a JSON object into a dict using the given decoder on each of its values
      
  {b Returns} [Ok of 'a Js.Dict.t] if the JSON value is a JSON object and all its
  values are successfully decoded, [Error of string] otherwise.
  
  @example {[
    open Js.Json
    let _ =

    (* prints [Ok (Js.Dict.fromList [("x", 23); ("y", 42)])] *)
    Js.log \@\@ parse {| { "x": 23, "y": 42 } |} |> Decode.(dict int)

    (* prints [Error ...] *)
    Js.log \@\@ parse {| { "x": 23, "y": "b" } |} |> Decode.(dict int)

    (* prints [Error ...] *)
    Js.log \@\@ parse "123" |> Decode.(dict int)

    (* prints [Ok None] *)
    Js.log \@\@ parse "null" |> Decode.(dict int)
  ]}
  *)

  val field : string -> 'a decoder -> 'a decoder
  (** Decodes a JSON object with a specific field into the value of that field
      
  {b Returns} [Ok of 'a] if the JSON value is a JSON object with the given field
  and a value that is successfully decoded with the given decoder, [Error of string] otherwise.
  
  @example {[
    open Js.Json
    let _ =

    (* prints [Ok 23] *)
    Js.log \@\@ parse {| { "x": 23, "y": 42 } |} |> Decode.(field "x" int)

    (* prints [Ok 23] *)
    Js.log \@\@ parse {| { "x": 23, "y": "b" } |} |> Decode.(field "x" int)

    (* prints [Error ...] *)
    Js.log \@\@ parse {| { "x": 23, "y": "b" } |} |> Decode.(field "y" int)

    (* prints [Error ...] *)
    Js.log \@\@ parse "123" |> Decode.(field "x" int)

    (* prints [Ok None] *)
    Js.log \@\@ parse "null" |> Decode.(field "x" int)
  ]}
  *)

  val optional : 'a decoder -> 'a option decoder
  (** Maps a decoder [result] to an option wrapped in an [Ok] result
      
  {b Returns} [Ok (Some of 'a)] if the given decoder is successful, [Ok None] if
  it is not.
  
  This decoder will never return an [Error]. Its purpose is to transform [Error]s
  of a given decoder into [Ok]s by mapping its [result] into an [option] and then
  returning that wrapped in an [Ok]. This prevents a decoder error from terminating
  a composite decoder, and is useful to decode optional JSON object fields.
  
  @example {[
    open Js.Json
    let _ =

    (* prints [Ok (Some 23)] *)
    Js.log \@\@ parse "23" |> Decode.(optional int)

    (* prints [Ok None] *)
    Js.log \@\@ parse 1.23 |> Decode.(optional int)

    (* prints [Ok None] *)
    Js.log \@\@ parse "null" |> Decode.(optional int)

    (* prints [Ok (Some 23)] *)
    Js.log \@\@ parse {| { "x": 23, "y": "b" } |} |> Decode.(optional (field "x" int))

    (* prints [Ok None] *)
    Js.log \@\@ parse {| { "x": 23, "y": "b" } |} |> Decode.(optional (field "y" int))

    (* prints [Ok None] *)
    Js.log \@\@ parse {| { "x": 23, "y": "b" } |} |> Decode.(optional (field "z" int))

    (* prints [Ok (Some 23)] *)
    Js.log \@\@ parse {| { "x": 23, "y": "b" } |} |> Decode.(field "x" (optional int))

    (* prints [Ok None] *)
    Js.log \@\@ parse {| { "x": 23, "y": "b" } |} |> Decode.(field "y" (optional int))

    (* prints [Error ...] *)
    Js.log \@\@ parse {| { "x": 23, "y": "b" } |} |> Decode.(field "z" (optional int))
  ]}
  *)

end

module Encode: sig
  external null : t = "" [@@bs.val]
  (** [null] is the singleton null JSON value *)

  external string : string -> t = "%identity"
  (** [string s] makes a JSON string of the [string] [s] *)

  external float : float -> t = "%identity"
  (** [float n] makes a JSON number of the [float] [n] *)

  external int : int -> t = "%identity"
  (** [int n] makes a JSON number of the [int] [n] *)

  external boolean : Js.boolean -> t = "%identity" 
  (** [boolean b] makes a JSON boolean of the [Js.boolean] [b] *)

  external object_ : t Js_dict.t -> t = "%identity"
  (** [object_ dict] makes a JSON objet of the [Js.Dict.t] [dict] *)

  external array_ : t array -> t = "%identity"
  (** [array_ a] makes a JSON array of the [Js.Json.t array] [a] *)

  (** The functions below are specialized for specific array type which 
      happened to be already JSON object in the BuckleScript runtime. Therefore
      they are more efficient (constant time rather than linear conversion). *) 

  external stringArray : string array -> t = "%identity"
  (** [stringArray a] makes a JSON array of the [string array] [a] *) 

  external numberArray : float array -> t = "%identity"
  (** [numberArray a] makes a JSON array of the [float array] [a] *)

  external booleanArray : Js.boolean array -> t = "%identity"
  (** [booleanArray] makes a JSON array of the [Js.boolean array] [a] *)

  external objectArray : t Js_dict.t array -> t = "%identity"
  (** [objectArray a] makes a JSON array of the [JsDict.t array] [a] *)
end


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
[@@deprecated "Please use `Decode.array_` instead"]
(** @deprecated Please use {! Decode.array_} instead *)

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
[@@deprecated "Please use `Encode.array_` instead"]
(** @deprecated Please use {! Encode.array_} instead *)

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
