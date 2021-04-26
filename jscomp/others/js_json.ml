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
  else if Js_array2.isArray x  then
    JSONArray (Obj.magic x)
  else
    JSONObject (Obj.magic x)


let test (type a) (x : 'a) (v : a kind) : bool =
  match v with
  | Number -> Js.typeof x = "number"
  | Boolean -> Js.typeof x = "boolean"
  | String -> Js.typeof x = "string"
  | Null -> (Obj.magic x) == Js.null
  | Array -> Js_array2.isArray x
  | Object -> (Obj.magic x) != Js.null && Js.typeof x = "object" && not (Js_array2.isArray x )

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
      not (Js_array2.isArray json) &&
      not ((Obj.magic json : 'a Js.null) == Js.null)
  then Some (Obj.magic (json:t) : t Js_dict.t)
  else None

let decodeArray json =
  if Js_array2.isArray json
  then Some (Obj.magic (json:t) : t array)
  else None

let decodeBoolean (json : t) =
  if Js.typeof json = "boolean"
  then Some (Obj.magic (json:t) : bool)
  else None

let decodeNull json : _ Js.null option =
  if (Obj.magic json : 'a Js.null) == Js.null
  then Some Js.null
  else None

(* external parse : string -> t = "parse"
  [@@bs.val][@@bs.scope "JSON"] *)

external parseExn : string -> t = "parse"
  [@@bs.val] [@@bs.scope "JSON"]

external stringifyAny : 'a -> string option =
"stringify" [@@bs.val]  [@@bs.scope "JSON"]
(* TODO: more docs when parse error happens or stringify non-stringfy value *)

external null : t = "null" [@@bs.val]
external string : string -> t = "%identity"
external number : float -> t = "%identity"
external boolean : bool -> t = "%identity"
external object_ : t Js_dict.t -> t = "%identity"

(* external array_ : t array -> t = "%identity" *)

external array : t array -> t = "%identity"
external stringArray : string array -> t = "%identity"
external numberArray : float array -> t = "%identity"
external booleanArray : bool array -> t = "%identity"
external objectArray : t Js_dict.t array -> t = "%identity"
external stringify: t -> string = "stringify"
  [@@bs.val] [@@bs.scope "JSON"]
external stringifyWithSpace: t -> (_ [@bs.as {json|null|json}]) -> int -> string = "stringify"
  [@@bs.val] [@@bs.scope "JSON"]


(* in memory modification does not work until your root is
   actually None, so we need wrap it as ``v`` and
   return the first element instead *)

let patch : _ -> _ = [%raw{|function (json) {
  var x = [json];
  var q = [{ kind: 0, i: 0, parent: x }];
  while (q.length !== 0) {
    // begin pop the stack
    var cur = q[q.length - 1];
    if (cur.kind === 0) {
      cur.val = cur.parent[cur.i]; // patch the undefined value for array
      if (++cur.i === cur.parent.length) {
        q.pop();
      }
    } else {
      q.pop();
    }
    // finish
    var task = cur.val;
    if (typeof task === "object") {
      if (Array.isArray(task) && task.length !== 0) {
        q.push({ kind: 0, i: 0, parent: task, val: undefined });
      } else {
        for (var k in task) {
          if (k === "RE_PRIVATE_NONE") {
            if (cur.kind === 0) {
              cur.parent[cur.i - 1] = undefined;
            } else {
              cur.parent[cur.i] = undefined;
            }
            continue;
          }
          q.push({ kind: 1, i: k, parent: task, val: task[k] });
        }
      }
    }
  }
  return x[0];
}
|}]


let serializeExn (type t) (x : t) : string  = [%raw{| function(obj){
  var output= JSON.stringify(obj,function(_,value){
      if(value===undefined){
          return {RE_PRIVATE_NONE : true}
      }
    return value
  });
  
 if(output === undefined){
   // JSON.stringify will raise TypeError when it detects cylic objects
   throw new TypeError("output is undefined")
 }
 return output 
 }
|}] x 

let deserializeUnsafe (s: string) : 'a = 
  patch (parseExn s)

