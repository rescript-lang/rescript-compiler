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

(** [get dict key] returns the value associated with [key] in [dict] *)
external get : 'a t -> key -> 'a option = "" [@@bs.get_index] [@@bs.return {undefined_to_opt}]

(** [unsafeGet dict key] returns the value associated with [key] in [dict]

This function will return an invalid value ([undefined]) if [key] does not exist in [dict]. It
will not throw an error.
*)
external unsafeGet : 'a t -> key -> 'a = "" [@@bs.get_index] 

(** [set dict key value] sets the value of [key] in [dict] to [value] *)
external set : 'a t -> key -> 'a -> unit = "" [@@bs.set_index]  

(** [keys dict] returns an array of all the keys in [dict] *)
external keys : 'a t -> key array = "Object.keys" [@@bs.val]

(** [empty ()] creates an empty dictionary *)
external empty : unit -> 'a t = "" [@@bs.obj]


let unsafeDeleteKey : string t -> string -> unit [@bs] = [%raw{|
  function(dict,key){
     delete dict[key];
     return 0
   }
|}]
