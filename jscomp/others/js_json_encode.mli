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


(** Provides functions for encoding a JSON data structure *)

external null : Js.json = "" [@@bs.val]
(** [null] is the singleton null JSON value *)

external string : string -> Js.json = "%identity"
(** [string s] makes a JSON string of the [string] [s] *)

external float : float -> Js.json = "%identity"
(** [float n] makes a JSON number of the [float] [n] *)

external int : int -> Js.json = "%identity"
(** [int n] makes a JSON number of the [int] [n] *)

external boolean : Js.boolean -> Js.json = "%identity" 
(** [boolean b] makes a JSON boolean of the [Js.boolean] [b] *)

external object_ : Js.json Js_dict.t -> Js.json = "%identity"
(** [object_ dict] makes a JSON objet of the [Js.Dict.t] [dict] *)

external array : Js.json array -> Js.json = "%identity"
(** [array a] makes a JSON array of the [Js.Json.t array] [a] *)

(** The functions below are specialized for specific array type which 
    happened to be already JSON object in the BuckleScript runtime. Therefore
    they are more efficient (constant time rather than linear conversion). *) 

external stringArray : string array -> Js.json = "%identity"
(** [stringArray a] makes a JSON array of the [string array] [a] *) 

external numberArray : float array -> Js.json = "%identity"
(** [numberArray a] makes a JSON array of the [float array] [a] *)

external booleanArray : Js.boolean array -> Js.json = "%identity"
(** [booleanArray] makes a JSON array of the [Js.boolean array] [a] *)