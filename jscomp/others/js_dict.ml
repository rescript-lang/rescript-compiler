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

(** Provides a simple key-value dictionary abstraction over native JavaScript objects *)


type 'a t
type key = string

external empty : unit -> 'a t = "" [@@bs.obj]

external get : 'a t -> key -> 'a option = "" [@@bs.get_index] [@@bs.return {undefined_to_opt}]
external unsafeGet : 'a t -> key -> 'a = "" [@@bs.get_index] 
external set : 'a t -> key -> 'a -> unit = "" [@@bs.set_index]  
external keys : 'a t -> key array = "Object.keys" [@@bs.val]

(* TODO: this is ES2017, should we polyfill? *)
external entries : 'a t -> (key * 'a) array = "Object.entries" [@@bs.val]

(* TODO: this is ES2017, should we polyfill? *)
external values : 'a t -> 'a array = "Object.values" [@@bs.val]

let fromList entries =
  let dict = empty () in
  entries |> List.iter (fun (key, value) -> set dict key value);
  dict

let fromArray entries =
  let dict = empty () in
  entries |> Array.iter (fun (key, value) -> set dict key value);
  dict

let map f source =
  let target = empty () in
  keys source
  |> Array.iter (fun key -> set target key (f @@ unsafeGet source key));
  target

