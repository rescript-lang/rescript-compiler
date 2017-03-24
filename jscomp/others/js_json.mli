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

(** Efficient JSON encoding using JavaScript API

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

(** {2 Accessor} *)

val reifyType : t -> 'b kind * 'b 
(** [reifyType v] returns both type and underlying value *) 

val test : 'a  -> 'b kind -> bool
(** [test v kind] returns true if [v] is of [kind] *)

module Decode: sig
  type ('a, 'e) result =
  | Ok of 'a
  | Error of 'e

  val boolean : t -> (Js.boolean, string) result 
  (** [boolean json] returns [Some b] if [json] is a boolean, [None]
      otherwise *)

  val number : t -> (float, string) result
  (** [number json] returns [Some n] if [json] is a number, [None]
      otherwise *)

  val string : t -> (Js_string.t, string) result
  (** [string json] returns [Some s] if [json] is a string, [None]
      otherwise *)

  val null : t -> ('a Js_null.t, string) result
  (** [null json] returns [Some null] if [json] is a null, [None]
      otherwise *)

  val array_ : t -> (t array, string) result
  (** [array_ json] returns [Some a] if [json] is an array, [None]
      otherwise *)

  val dict : t -> (t Js_dict.t, string) result
  (** [dict json] returns [Some o] if [json] is an object, [None]
      otherwise
  *)
end

module Encode: sig
  external null : t = "" [@@bs.val]
  (** [null] is the singleton null JSON value *)

  external string : string -> t = "%identity"
  (** [string s] makes a JSON string of the [string] [s] *)

  external number : float -> t = "%identity"
  (** [number n] makes a JSON number of the [float] [n] *)

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
(** @deprecated Please use {! Decode.boolean} istead *)

val decodeNumber : t -> float option
[@@deprecated "Please use `Decode.number` instead"]
(** @deprecated Please use {! Decode.number} istead *)

val decodeString : t -> Js_string.t option
[@@deprecated "Please use `Decode.string` instead"]
(** @deprecated Please use {! Decode.string} istead *)

val decodeNull : t -> 'a Js_null.t option
[@@deprecated "Please use `Decode.null` instead"]
(** @deprecated Please use {! Decode.null} istead *)

val decodeArray : t -> t array option
[@@deprecated "Please use `Decode.array_` instead"]
(** @deprecated Please use {! Decode.array_} istead *)

val decodeObject : t -> t Js_dict.t option
[@@deprecated "Please use `Decode.dict` instead"]
(** @deprecated Please use {! Decode.dict} istead *)



external boolean : Js.boolean -> t = "%identity" 
[@@deprecated "Please use `Encode.boolean` instead"]
(** @deprecated Please use {! Encode.boolean} istead *)

external number : float -> t = "%identity"
[@@deprecated "Please use `Encode.number` instead"]
(** @deprecated Please use {! Encode.number} istead *)

external string : string -> t = "%identity"
[@@deprecated "Please use `Encode.string` instead"]
(** @deprecated Please use {! Encode.string} istead *)

external null : t = "" [@@bs.val]
[@@deprecated "Please use `Encode.null` instead"]
(** @deprecated Please use {! Encode.null} istead *)

external array_ : t array -> t = "%identity"
[@@deprecated "Please use `Encode.array_` instead"]
(** @deprecated Please use {! Encode.array_} istead *)

external object_ : t Js_dict.t -> t = "%identity"
[@@deprecated "Please use `Encode.object_` instead"]
(** @deprecated Please use {! Encode.object_} istead *)

external booleanArray : Js.boolean array -> t = "%identity"
[@@deprecated "Please use `Encode.booleanArray` instead"]
(** @deprecated Please use {! Encode.booleanArray} istead *)

external numberArray : float array -> t = "%identity"
[@@deprecated "Please use `Encode.numberArray` instead"]
(** @deprecated Please use {! Encode.numberArray} istead *)

external stringArray : string array -> t = "%identity"
[@@deprecated "Please use `Encode.stringArray` instead"]
(** @deprecated Please use {! Encode.stringArray} istead *)

external objectArray : t Js_dict.t array -> t = "%identity"
[@@deprecated "Please use `Encode.objectArray` instead"]
(** @deprecated Please use {! Encode.objectArray} istead *)



(** {2 String conversion} *)

external parse : string -> t = "JSON.parse" [@@bs.val]
(** [parse s] parses the string [s] into a JSON data structure

{b Returns} a JSON data structure

@raise SyntaxError if given string is not a valid JSON. Note [SyntaxError] is a JavaScript exception. 

@example {[
(* parse a simple JSON string *)

let json = 
  try
    Js_json.parse {| "foo" |} 
  with
  | _ -> failwith "Error parsing JSON string"
in
match Js.Json.reifyType json in
| (Js.Json.String, value) -> Js.log value
| _ -> failWith "Expected a string"
]}

@example {[
(* parse a complex JSON string *)

let getIds s =
  let json = 
    try
      Js.Json.parse s
    with
    | _ -> failwith "Error parsing JSON string"
  in 
  match Js.Json.reifyType json with
  | (Js.Json.Object, value) ->
    (* In this branch, compiler infer value : Js.Json.t Js.Dict.t *)
    begin match Js.Dict.get value "ids" with
    | Some ids -> 
      begin match Js.Json.reifyType ids with
      | (Js.Json.Array, ids) -> 
        (* In this branch compiler infer ids : Js.Json.t array *)
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
Js.log \@\@ Js.Json.stringify [| "foo"; "bar" |]
]}

@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify> MDN
*)


(**
@deprecated Please use {! reifyType} instead
*) 
val reify_type : 'a -> 'b kind * 'b 
[@@deprecated "Please use `reifyType`"]
