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

(** The dict type *)
type 'a t

(** The key type, an alias of string *)
type key = string

(**
  `unsafeGet dict key` returns the value associated with `key` in `dict`

  This function will return an invalid value (`undefined`) if `key` does not exist in `dict`. It
  will not throw an error.
*)
external unsafeGet : 'a t -> key -> 'a = "" [@@bs.get_index]
let (.!()) = unsafeGet

(** `get dict key` returns the value associated with `key` in `dict` *)
let get (type u) (dict : u t) (k : key) : u option =
  if [%raw {|k in dict|}] then
    Some dict.!(k)
  else None

(** `set dict key value` sets the value of `key` in `dict` to `value` *)
external set : 'a t -> key -> 'a -> unit = "" [@@bs.set_index]

(** `keys dict` returns an array of all the keys in `dict` *)
external keys : 'a t -> key array = "Object.keys" [@@bs.val]

(** `empty ()` creates an empty dictionary *)
external empty : unit -> 'a t = "" [@@bs.obj]


let unsafeDeleteKey : string t -> string -> unit [@bs] =
  [%raw {| function (dict,key){
      delete dict[key];
     }
  |}]


external unsafeCreate : int -> 'a array = "Array" [@@bs.new]
(* external entries : 'a t -> (key * 'a) array = "Object.entries" [@@bs.val] (* ES2017 *) *)
let entries dict =
  let keys = keys dict in
  let l = Js_array2.length keys in
  let values = unsafeCreate l in
  for i = 0 to l - 1 do
    let key = Js_array2.unsafe_get keys i in
    Js_array2.unsafe_set values i (key, dict.!(key))
  done;
  values

(* external values : 'a t -> 'a array = "Object.values" [@@bs.val] (* ES2017 *) *)
let values dict =
  let keys = keys dict in
  let l = Js_array2.length keys in
  let values = unsafeCreate l in
  for i = 0 to l - 1 do
    Js_array2.unsafe_set values i  dict.!(Js_array2.unsafe_get keys i)
  done;
  values

let fromList entries =
  let dict = empty () in
  let rec loop = function
  | [] -> dict
  | (key, value) :: rest ->
    set dict key value;
    loop rest
  in
  loop entries

let fromArray entries =
  let dict = empty () in
  let l = Js_array2.length entries in
  for i = 0 to l - 1 do
    let (key, value) = Js_array2.unsafe_get entries i in
    set dict key value
  done;
  dict

let map f source =
  let target = empty () in
  let keys = keys source in
  let l = Js_array2.length keys in
  for i = 0 to l - 1 do
    let key = Js_array2.unsafe_get keys i in
    set target key (f (unsafeGet source key) [@bs])
  done;
  target
