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

val reify_type : t -> 'b kind * 'b
(** [reify_type v] returns both type and underlying value *) 

val test : 'a  -> 'b kind -> bool
(** [test v kind] returns true if [v] is of [kind] *)

(** {2 Construtors} *)

val null : t 
(** Null JSON value *)

val string : string -> t 
(** Make a JSON string *)

val number : float -> t 
(** Make a JSON number *)

val number_of_int : int -> t 
(** Make a JSON number from int*)

val boolean : bool -> t 
(** Make a JSON boolean *)

val object_ : t Js_dict.t -> t
(** Make a JSON objet *)

val array_ : t array -> t 
(** Make a JSON array *)

(** The functions below are specialized for specific array type which 
    happened to be already JSON object in the BuckleScript runtime. Therefore
    they are more efficient (constant time rather than linear conversion). *) 

val string_array : string array -> t
(** Make a JSON string array *) 

val number_array : float array -> t
(** Make a JSON number array *)

val int_array : int array -> t
(** Make a JSON number array *)

val boolean_array : bool array -> t
(** Make a JSON bool array *)

(** {2 String conversion} *)

external parse : string -> t = "JSON.parse" [@@bs.val]
(** [parse s] returns JSON value *)

external to_string : 'a -> string = "JSON.stringify" [@@bs.val]
(** [to_string json] returns JSON string *)
