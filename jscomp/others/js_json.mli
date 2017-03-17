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

(** Efficient JSON encoding using Javascript API *) 

(** {2 Types} *)

(** Json type *)
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

val reify_type : 'a -> 'b kind * 'b 
  [@@ocaml.deprecated "Please use `reifyType`"]
(** [reify_type v] returns both type and underlying value. @deprecated *) 

val reifyType : t -> 'b kind * 'b 
(** [reifyType v] returns both type and underlying value *) 

val test : 'a  -> 'b kind -> bool
(** [test v kind] returns true if [v] is of [kind] *)

(** {2 Construtors} *)

(** Those functions allows the construction of an arbitrary complex 
    JSON values. 

*)

val null : t 
(** Null JSON value *)

external string : string -> t = "%identity"
(** Make a JSON string *)

external number : float -> t = "%identity"
(** Make a JSON number *)

external boolean : Js.boolean -> t = "%identity" 
(** Make a JSON boolean *)

external object_ : t Js_dict.t -> t = "%identity"
(** Make a JSON objet *)

external array_ : t array -> t = "%identity"
(** Make a JSON array *)

(** The functions below are specialized for specific array type which 
    happened to be already JSON object in the BuckleScript runtime. Therefore
    they are more efficient (constant time rather than linear conversion). *) 

external stringArray : string array -> t = "%identity"
(** Make a JSON string array *) 

external numberArray : float array -> t = "%identity"
(** Make a JSON number array *)

external booleanArray : Js.boolean array -> t = "%identity"
(** Make a JSON bool array *)

external objectArray : t Js_dict.t array -> t = "%identity"
(** Make a JSON object array *)

(** {2 String conversion} *)

external parse : string -> t = "JSON.parse" [@@bs.val]
(** [parse s] returns JSON value and @raises SyntaxError in the 
    case [s] is not a valid JSON string. 
    Note [SyntaxError] is a JavaScript exception. 

@example {[
let json = 
  try Js_json.parse {| { "x" : [1, 2, 3 ] } |} 
  with | e -> Js.log e; failwith "Error parsing JSON string"
in 
let ty, ob = Js_json.reifyType json in 
match ty with
| Js_json.Object ->
  (* In this branch, compiler infer ob : Js_json.t Js_dict.t *)
  begin match Js_dict.get ob "x" with
  | None -> assert(false) 
  | Some xValue -> 
    let ty, xValue = Js_json.reifyType xValue in 
    begin match ty with
    | Js_json.Array -> 
      (* In this branch compiler infer xValue : Js_json.t array *)
      assert(3 = Array.length xValue) 
    | _ -> assert(false) 
    end 
  end 
| _ -> assert(false)
]}*)

external stringify: t -> string = "JSON.stringify" [@@bs.val]
(** [stringify json] returns JSON string 

@example {[
(* This example shows how to create a simple JSON string made of a single 
   JSON object with a few key/values *)

let dict = Js_dict.empty () in 
Js_dict.set dict "name" (Js_json.string "John Doe"); 
Js_dict.set dict "age" (Js_json.numberOfInt 30); 
Js_dict.set dict "likes" 
  (Js_json.stringArray [|"bucklescript";"ocaml";"js"|]);
Js.log \@\@ Js_json.stringify (Js_json.object_ dict) 
]}

*)

external stringifyAny : 'a -> string option = "JSON.stringify" [@@bs.val] [@@bs.return undefined_to_opt]
