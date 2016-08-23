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
  name : string;
}

type js_send = { 
  splice : bool ; 
  name : string 
} (* we know it is a js send, but what will happen if you pass an ocaml objct *)

type js_val = string external_module 

type arg_type = Ast_core_type.arg_type
  
type arg_label = Ast_core_type.arg_label 

type arg_kind = 
  {
    arg_type : arg_type;
    arg_label : arg_label
  }

type ffi = 
  | Obj_create of arg_label list
  | Js_global of js_val 
  | Js_module_as_var of  external_module_name
  | Js_module_as_fn of external_module_name
  | Js_module_as_class of external_module_name       
  | Js_call of js_call external_module
  | Js_send of js_send
  | Js_new of js_val
  | Js_set of string
  | Js_get of string
  | Js_get_index
  | Js_set_index

  (* When it's normal, it is handled as normal c functional ffi call *)

type t  = 
  | Bs of arg_kind list  * arg_type *   ffi
  | Normal 






val handle_attributes_as_string : 
  Bs_loc.t ->
  string  ->
  Ast_core_type.t ->
  Ast_attributes.t -> 
  string   ->
  Ast_core_type.t * string list

val bs_external : string 
val to_string : t -> string 
val from_string : string -> t 
val unsafe_from_string : string -> t 
val is_bs_external_prefix : string -> bool

