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

(** Efficient JSON encoding using JavaScript API *)

type t

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

let classify  (x : t) : tagged_t =
  let ty = Js.typeof x in  
  if ty = "string" then 
    JSONString (Obj.magic x)
  else if ty = "number" then 
    JSONNumber (Obj.magic x )
  else if ty = "boolean" then
    if (Obj.magic x) = true then JSONTrue
    else JSONFalse 
  else if (Obj.magic x) == Js.null then
    JSONNull 
  else if Js_array.isArray x  then 
    JSONArray (Obj.magic x)
  else 
    JSONObject (Obj.magic x)


let test (type a) (x : 'a) (v : a kind) : bool =
  match v with
  | Number -> Js.typeof x = "number"
  | Boolean -> Js.typeof x = "boolean" 
  | String -> Js.typeof x = "string"
  | Null -> (Obj.magic x) == Js.null 
  | Array -> Js_array.isArray x 
  | Object -> (Obj.magic x) != Js.null && Js.typeof x = "object" && not (Js_array.isArray x )

let decodeString json = 
  if Js.typeof json = "string" 
  then Some (Obj.magic (json:t) : string)
  else None 

let decodeNumber json = 
  if Js.typeof json = "number" 
  then Some (Obj.magic (json:t) : float)
  else None 

let decodeObject json = 
  if  Js.typeof json = "object" && 
      not (Js_array.isArray json) && 
      not ((Obj.magic json : 'a Js.null) == Js.null)
  then Some (Obj.magic (json:t) : t Js_dict.t)
  else None 

let decodeArray json = 
  if Js_array.isArray json
  then Some (Obj.magic (json:t) : t array)
  else None 

let decodeBoolean (json : t) = 
  if Js.typeof json = "boolean"
  then Some (Obj.magic (json:t) : bool)
  else None 

let decodeNull json = 
  if (Obj.magic json : 'a Js.null) == Js.null
  then Some Js.null
  else None 

external parse : string -> t = "parse" 
  [@@bs.val][@@bs.scope "JSON"]

external parseExn : string -> t = "parse" 
  [@@bs.val] [@@bs.scope "JSON"]

external stringifyAny : 'a -> string option = 
"stringify" [@@bs.val] [@@bs.return undefined_to_opt] [@@bs.scope "JSON"]
(* TODO: more docs when parse error happens or stringify non-stringfy value *)

external null : t = "" [@@bs.val]
external string : string -> t = "%identity"
external number : float -> t = "%identity"
external boolean : bool -> t = "%identity" 
external object_ : t Js_dict.t -> t = "%identity"

external array_ : t array -> t = "%identity"

external array : t array -> t = "%identity"
external stringArray : string array -> t = "%identity"
external numberArray : float array -> t = "%identity"
external booleanArray : bool array -> t = "%identity"
external objectArray : t Js_dict.t array -> t = "%identity"
external stringify: t -> string = "stringify" 
  [@@bs.val] [@@bs.scope "JSON"]
external stringifyWithSpace: t -> (_ [@bs.as {json|null|json}]) -> int -> string = "stringify" 
  [@@bs.val] [@@bs.scope "JSON"]

type preJson
let _toJson: preJson -> t = fun%raw data -> {|
  if (!data) {
    return data
  } else if (typeof data === 'function') {
    throw new Error("Cannot serialize a function")
  } else if (Array.isArray(data)) {
    if (data.tag != null) {
      return {
        $$tag: data.tag,
        $$contents: data.map(_toJson),
        $$bsVariant: data[Symbol.for('BsVariant')],
      }
    } else if (data[Symbol.for('BsVariant')] != null) {
      return {
        $$bsVariant: data[Symbol.for('BsVariant')],
        $$contents: data.map(_toJson)
      }
    } else if (data[Symbol.for('BsLocalModule')] != null) {
      return {
        $$bsLocalModule: data[Symbol.for('BsLocalModule')],
        $$contents: data.map(_toJson)
      }
    } else if (data[Symbol.for('BsPolyVar')] != null) {
      return {
        $$bsPolyVar: data[Symbol.for('BsPolyVar')],
        $$contents: data.map(_toJson)
      }
    } else if (data[Symbol.for('BsRecord')] != null) {
      return {
        $$bsRecord: data[Symbol.for('BsRecord')],
        $$contents: data.map(_toJson)
      }
    } else {
      return data.map(_toJson)
    }
  } else if (typeof data == 'object') {
    var result = {}
    Object.keys(data).forEach(key => result[key] = _toJson(data[key]))
    return result
  } else {
    return data
  }
|}

(** This dance is required to appease the type checker -- doing 
 * `toJson: 'a -> t = fun%raw` gives the error "contains type variables
 * that cannot be generalized". *)
external toT : 'a -> preJson = "%identity"
let serializeAnyToJson data = _toJson (toT data)

let unserializeAnyFromJsonUnsafe: t -> 'a = fun%raw data -> {|
  if (!data) {
    return data
  } else if (typeof data == 'object') {
    if (Array.isArray(data)) {
      return data.map(unserializeAnyFromJsonUnsafe)
    } else if (data.$$contents) {
      var result = data.$$contents.map(unserializeAnyFromJsonUnsafe)
      if (data.$$tag != null) {
        result.tag = data.$$tag
      }
      if (data.$$bsRecord) {
        result[Symbol.for('BsRecord')] = data.$$bsRecord
      }
      if (data.$$bsPolyVar) {
        result[Symbol.for('BsPolyVar')] = data.$$bsPolyVar
      }
      if (data.$$bsVariant) {
        result[Symbol.for('BsVariant')] = data.$$bsVariant
      }
      if (data.$$bsLocalModule) {
        result[Symbol.for('BsLocalModule')] = data.$$bsLocalModule
      }
      return result
    } else {
      var result = {}
      Object.keys(data).forEach(key => result[key] = unserializeAnyFromJsonUnsafe(data[key]))
      return result
    }
  } else {
    return data
  }
|}