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

(** Placeholder for Node bindings *)

module Path = Node_path

module Fs = Node_fs

module Process = Node_process

module Module =  Node_module
module Buffer = Node_buffer
module Child_process = Node_child_process

type node_exports 
type node_module = <
       id : string ;
       exports : node_exports ;
       parrent : node_module Js.null_undefined;
       (* in REPL V4 it is [undefined]
          in CLI it can be [null]
       *)              
       
       filename : string ;       

       loaded : bool;
       children : node_module array ;
       paths : string array;       
> Js.t
 (* WARN: 
     its path name should not be changed 
     see {!Ppx_entry}
*)

type node_require = <
  main : node_module Js.undefined;

  resolve : string -> string [@bs]
  (* @raise exception   *)
> Js.t 
(* WARN:
    its path name should not be changed 
    see {!Ppx_entry}
*)

type string_buffer (* can be either string or buffer *)

type buffer  

type _ string_buffer_kind = 
  | String :  string string_buffer_kind
  | Buffer :  buffer string_buffer_kind


(** We expect a good inliner will eliminate such boxing in the future *)
let test (type t) (x : string_buffer) : (t string_buffer_kind * t)= 
  if Js.typeof x = "string" then 
    (Obj.magic String : t string_buffer_kind),  (Obj.magic x : t)
  else 
    (Obj.magic Buffer : t string_buffer_kind), (Obj.magic x : t)
