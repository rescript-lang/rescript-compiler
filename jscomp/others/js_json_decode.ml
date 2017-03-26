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


open Bs.Result

type 'a decoder = Js.json -> ('a, string) result

let boolean json = 
  if Js.typeof json = "boolean"
  then Ok (Obj.magic (json : Js.json) : Js.boolean)
  else Error ("Expected boolean, got " ^ Js_json.stringify json)

let float json = 
  if Js.typeof json = "number" 
  then Ok (Obj.magic (json : Js.json) : float)
  else Error ("Expected number, got " ^ Js_json.stringify json)

let int json = 
  match float json with
  | Ok float ->
    if Js.Float.isInteger float
    then Ok (Obj.magic (float : float) : int)
    else Error ("Expected integer, got " ^ Js_json.stringify json)
  | Error message -> Error message

let string json = 
  if Js.typeof json = "string" 
  then Ok (Obj.magic (json:Js.json) : string)
  else Error ("Expected string, got " ^ Js_json.stringify json)

let nullable decode json =
  if (Obj.magic json : 'a Js.null) == Js.null
  then Ok Js.null
  else
    match decode json with
    | Ok value -> Ok (Js.Null.return value)
    | Error message -> Error message

(* TODO: remove this? *)
let nullAs value json = 
  if (Obj.magic json : 'a Js.null) == Js.null
  then Ok value
  else Error ("Expected null, got " ^ Js_json.stringify json)

let array decode json = 
  if Js_array.isArray json
  then begin
    (* TODO: Seriously rethink this for better balance between readability and perf *)
    let source = (Obj.magic (json : Js.json) : Js.json array) in
    let l = Js.Array.length source in

    if l = 0 then Ok [||]

    else begin
      let target = Array.make l (Obj.magic 0) in
      let break = ref false in
      let result = ref (Ok target) in
      let i = ref 0 in
      while not !break do
        if !i >= l then break := true
        else
          match decode (Array.unsafe_get source !i) with
          | Ok value ->
            Array.set target !i value;
            i := !i + 1
          | Error message ->
            result := Error message;
            break := true
      done;
      !result
    end
  end
  else Error ("Expected array, got " ^ Js_json.stringify json)

let dict decode json = 
  if Js.typeof json = "object" && 
      not (Js_array.isArray json) && 
      not ((Obj.magic json : 'a Js.null) == Js.null)
  then begin
    (* TODO: Seriously rethink this for better balance between readability and perf *)
    let source = (Obj.magic (json : Js.json) : Js.json Js_dict.t) in
    let keys = Js_dict.keys source in
    let l = Js.Array.length keys in

    if l = 0 then Ok (Js_dict.empty ())

    else begin
      let target = Js_dict.empty () in
      let break = ref false in
      let result = ref (Ok target) in
      let i = ref 0 in
      while not !break do
        if !i >= l then break := true
        else
          let key = (Array.unsafe_get keys !i) in
          match decode (Js_dict.unsafeGet source key) with
          | Ok value ->
            Js_dict.set target key value;
            i := !i + 1
          | Error message ->
            result := Error message;
            break := true
      done;
      !result
    end
  end
  else Error ("Expected object, got " ^ Js_json.stringify json)

let field key decode json =
  if Js.typeof json = "object" && 
      not (Js_array.isArray json) && 
      not ((Obj.magic json : 'a Js.null) == Js.null)
  then begin
    let dict = (Obj.magic (json : Js.json) : Js.json Js.Dict.t) in
    match Js.Dict.get dict key with
    | Some value -> decode value
    | None -> Error ("Expected field '" ^ key ^ "'")
  end
  else Error ("Expected object, got " ^ Js_json.stringify json)

let optional decode json =
  match decode json with
  | Ok value -> Ok (Some value)
  | Error message -> Ok None
