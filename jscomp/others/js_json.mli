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

(**
  Efficient JSON encoding using JavaScript API

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON)
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
  | Boolean : bool kind
  | Null : Js_types.null_val kind

type tagged_t =
  | JSONFalse
  | JSONTrue
  | JSONNull
  | JSONString of string
  | JSONNumber of float
  | JSONObject of t Js_dict.t
  | JSONArray of t array


(** {2 Accessor} *)

val classify : t -> tagged_t


val test : 'a  -> 'b kind -> bool
(** `test v kind` returns true if `v` is of `kind` *)

val decodeString : t -> Js_string.t option
(** `decodeString json` returns `Some s` if `json` is a string, `None`
    otherwise *)

val decodeNumber : t -> float option
(** `decodeNumber json` returns `Some n` if `json` is a number, `None`
    otherwise *)

val decodeObject : t -> t Js_dict.t option
(** `decodeObject json` returns `Some o` if `json` is an object, `None`
    otherwise *)

val decodeArray : t -> t array option
(** `decodeArray json` returns `Some a` if `json` is an array, `None`
    otherwise *)

val decodeBoolean : t -> bool option
(** `decodeBoolean json` returns `Some b` if `json` is a boolean, `None`
    otherwise *)

val decodeNull : t -> 'a Js_null.t option
(** `decodeNull json` returns `Some null` if `json` is a null, `None`
    otherwise *)

(** {2 Construtors} *)

(** Those functions allows the construction of an arbitrary complex
    JSON values.
*)

external null : t = "null" [@@bs.val]
(** `null` is the singleton null JSON value *)

external string : string -> t = "%identity"
(** `string s` makes a JSON string of the `string` `s` *)

external number : float -> t = "%identity"
(** `number n` makes a JSON number of the `float` `n` *)

external boolean : bool -> t = "%identity"
(** `boolean b` makes a JSON boolean of the `bool` `b` *)

external object_ : t Js_dict.t -> t = "%identity"
(** `object_ dict` makes a JSON object of the `Js.Dict.t` `dict` *)


external array : t array -> t = "%identity"
(** `array_ a` makes a JSON array of the `Js.Json.t array` `a` *)

(**
  The functions below are specialized for specific array type which
  happened to be already JSON object in the ReScript runtime. Therefore
  they are more efficient (constant time rather than linear conversion).
*)

external stringArray : string array -> t = "%identity"
(** `stringArray a` makes a JSON array of the `string array` `a` *)

external numberArray : float array -> t = "%identity"
(** `numberArray a` makes a JSON array of the `float array` `a` *)

external booleanArray : bool array -> t = "%identity"
(** `booleanArray` makes a JSON array of the `bool array` `a` *)

external objectArray : t Js_dict.t array -> t = "%identity"
(** `objectArray a` makes a JSON array of the `JsDict.t array` `a` *)

(** {2 String conversion} *)

external parseExn : string -> t = "parse" [@@bs.val] [@@bs.scope "JSON"]
(**
  `parseExn s` parses the string `s` into a JSON data structure

  **return** a JSON data structure

  **raise** SyntaxError if given string is not a valid JSON. Note `SyntaxError` is a JavaScript exception.

  ```
  (* parse a simple JSON string *)

  let json =
    try
      Js.Json.parseExn {| "foo" |}
    with
    | _ -> failwith "Error parsing JSON string"
  in
  match Js.Json.classify json with
  | Js.Json.JSONString value -> Js.log value
  | _ -> failwith "Expected a string"
  ```

  ```
  (* parse a complex JSON string *)

  let getIds s =
    let json =
      try
        Js.Json.parseExn s
      with
      | _ -> failwith "Error parsing JSON string"
    in
    match Js.Json.classify json with
    | Js.Json.JSONObject value ->
      (* In this branch, compiler infer value : Js.Json.t Js.Dict.t *)
      begin match Js.Dict.get value "ids" with
        | Some ids ->
          begin match Js.Json.classify ids with
            | Js.Json.JSONArray ids ->
              (* In this branch compiler infer ids : Js.Json.t array *)
              ids
            | _ -> failwith "Expected an array"
          end
        | None -> failwith "Expected an `ids` property"
      end
    | _ -> failwith "Expected an object"

  (* prints `1, 2, 3` *)
  let _ =
    Js.log @@ getIds {| { "ids" : [1, 2, 3] } |}
  ```

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/parse)
*)

external stringify: t -> string = "stringify"
[@@bs.val] [@@bs.scope "JSON"]
(**
  `stringify json` formats the JSON data structure as a string

  **return** the string representation of a given JSON data structure

  ```
  (* Creates and stringifies a simple JS object *)

  let dict = Js.Dict.empty () in
  Js.Dict.set dict "name" (Js.Json.string "John Doe");
  Js.Dict.set dict "age" (Js.Json.number 30.0);
  Js.Dict.set dict "likes"
    (Js.Json.stringArray [|"bucklescript";"ocaml";"js"|]);

  Js.log @@ Js.Json.stringify (Js.Json.object_ dict)
  ```

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify)
*)

external stringifyWithSpace: t -> (_ [@bs.as {json|null|json}]) -> int -> string = "stringify"
[@@bs.val] [@@bs.scope "JSON"]
(**
  `stringify json` formats the JSON data structure as a string

  **return** the string representation of a given JSON data structure

  ```
  (* Creates and stringifies a simple JS object with spacing *)

  let dict = Js.Dict.empty () in
  Js.Dict.set dict "name" (Js.Json.string "John Doe");
  Js.Dict.set dict "age" (Js.Json.number 30.0);
  Js.Dict.set dict "likes"
    (Js.Json.stringArray [|"bucklescript";"ocaml";"js"|]);

  Js.log @@ Js.Json.stringifyWithSpace (Js.Json.object_ dict) 2
  ```

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify)
*)


external stringifyAny : 'a -> string option = "stringify"
[@@bs.val]  [@@bs.scope "JSON"]
(**
  `stringifyAny value` formats any `value` into a JSON string

  ```
  (* prints ``"foo", "bar"`` *)
  Js.log @@ Js.Json.stringifyAny [| "foo"; "bar" |]
  ```

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify)
*)


(**
  Best-effort serialization, it tries to seralize as
  many objects as possible and deserialize it back *)

(**
  It is unsafe in two aspects
  - It may throw during  parsing
  - when you cast it to a specific type, it may have a type mismatch
*)
val deserializeUnsafe : string -> 'a


(**
  It will raise in such situations:
  - The object can not be serlialized to a JSON
  - There are cycles
  - Some JS engines can not stringify deeply nested json objects
*)
val serializeExn : 'a -> string
