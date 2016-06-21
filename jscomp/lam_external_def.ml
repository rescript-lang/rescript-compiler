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



type external_module_name = 
  { bundle : string ; 
    bind_name : string option
  }
type 'a external_module = {
  txt : 'a ;
  external_module_name : external_module_name option;
}


type js_call = { 
  splice : bool ;
  qualifiers : string list;
  name : string;
}

type js_send = { 
  splice : bool ; 
  name : string 
} (* we know it is a js send, but what will happen if you pass an ocaml objct *)

type js_val = { 
  name : string ;
  external_module_name : external_module_name option;
  
} 

type js_new = {  name : string }
type js_set = { name : string }
type js_get = { name : string }

type ffi = 
  | Obj_create 
  | Js_global of js_val 
  | Js_global_as_var of  external_module_name
  | Js_call of js_call external_module
  | Js_send of js_send
  | Js_new of js_new external_module
  | Js_set of js_set
  | Js_get of js_get
  | Js_get_index
  | Js_set_index
  | Normal 
  (* When it's normal, it is handled as normal c functional ffi call *)
type prim = Types.type_expr option Primitive.description

let check_external_module_name ?loc x = 
  match x with 
  | {bundle = ""; _ } | {bind_name = Some ""} -> 
    Location.raise_errorf ?loc "empty name encountered"
  | _ -> ()
let check_external_module_name_opt ?loc x = 
  match x with 
  | None -> ()
  | Some v -> check_external_module_name ?loc v 


let check_ffi ?loc ffi = 
  match ffi with 
  | Js_global {name = ""} 
  | Js_send {name = ""}
  | Js_set {name = ""}
  | Js_get {name = ""}
    -> Location.raise_errorf ?loc "empty name encountered"
  | Js_global _ | Js_send _ | Js_set _ | Js_get _  
  | Obj_create 
  | Js_get_index | Js_set_index | Normal -> ()

  | Js_global_as_var external_module_name 
    -> check_external_module_name external_module_name
  | Js_new {external_module_name ; txt = {name ; _}}
  | Js_call {external_module_name ; txt = {name ; _}}
    -> 
    check_external_module_name_opt ?loc external_module_name ; 
    if name = "" then
      Location.raise_errorf ?loc "empty name in externals"
