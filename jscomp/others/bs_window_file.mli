(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2011 Pierre Chambart
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*)
(** File API
  @see <https://developer.mozilla.org/en-US/docs/Web/API/File> the documentation of the API. *)

open Js
open Bs_window_dom

class type blob = object
  method size : int 
  method _type : string 
  method slice : int -> int -> blob t 
  method slice_withContentType : int -> int -> string -> blob t 
end

class type file = object
  inherit blob
  method name : string 
  method lastModifiedDate : string 
end

type file_any

module CoerceTo : sig
  val document : file_any -> element document t Null.t
  val blob : file_any -> #blob t Null.t
  val json : file_any -> 'a Null.t
  val string : file_any -> string Null.t
  (* val arrayBuffer : file_any -> Typed_array.arrayBuffer t Null.t *)
end

class type fileList = object
  inherit [file] nodeList
end

class type fileError = object
  method code : int 
end

(* {2 Events} *)
class type ['a] progressEvent = object
  inherit ['a] event
  method lengthComputable : bool t 
  method loaded : int 
  method total : int 
end

class type progressEventTarget = object ('self)
  (* method onloadstart : ('self t, 'self progressEvent t) event_listener writeonly_prop *)
  (* method onprogress : ('self t, 'self progressEvent t) event_listener writeonly_prop *)
  (* method onload : ('self t, 'self progressEvent t) event_listener writeonly_prop *)
  (* method onabort : ('self t, 'self progressEvent t) event_listener writeonly_prop *)
  (* method onerror : ('self t, 'self progressEvent t) event_listener writeonly_prop *)
  (* method onloadend : ('self t, 'self progressEvent t) event_listener writeonly_prop *)
end

type readyState = EMPTY | LOADING | DONE

class type fileReader = object ('self)

  method readAsArrayBuffer : #blob t -> unit 
  method readAsBinaryString : #blob t -> unit 
  method readAsText : #blob t -> unit 
  method readAsText_withEncoding : #blob t -> string -> unit 
  method readAsDataURL : #blob t -> unit 

  method abort : unit 

  method readyState : readyState 

  method result : file_any 
  method error : fileError t 

  (* method onloadstart : ('self t, 'self progressEvent t) event_listener writeonly_prop *)
  (* method onprogress : ('self t, 'self progressEvent t) event_listener writeonly_prop *)
  (* method onload : ('self t, 'self progressEvent t) event_listener writeonly_prop *)
  (* method onabort : ('self t, 'self progressEvent t) event_listener writeonly_prop *)
  (* method onerror : ('self t, 'self progressEvent t) event_listener writeonly_prop *)
  (* method onloadend : ('self t, 'self progressEvent t) event_listener writeonly_prop *)

  inherit progressEventTarget
end
