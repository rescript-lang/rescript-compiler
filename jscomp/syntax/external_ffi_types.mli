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

type module_bind_name = 
  | Phint_name of string 
  (* explicit hint name *)
  | Phint_nothing

type external_module_name = 
  { bundle : string ; 
    module_bind_name : module_bind_name
  }

type pipe = bool 
type js_call = { 
  name : string;
  external_module_name : external_module_name option;
  splice : bool ;
  scopes : string list 
}

type js_send = { 
  name : string ;
  splice : bool ; 
  pipe : pipe  ;
  js_send_scopes : string list; 
} (* we know it is a js send, but what will happen if you pass an ocaml objct *)

type js_global_val = {
  name : string ; 
  external_module_name : external_module_name option;
  scopes : string list
}

type js_new_val = {
  name : string ; 
  external_module_name : external_module_name option;
  splice : bool ;
  scopes : string list;
}

type js_module_as_fn = 
  { external_module_name : external_module_name;
    splice : bool 
  }

type arg_type = External_arg_spec.attr

type arg_label = External_arg_spec.label 


type obj_create = External_arg_spec.t list

type js_get =  
  { js_get_name : string   ;
    js_get_scopes :  string list;
  }

type js_set = 
  { js_set_name : string  ;
    js_set_scopes : string list 
  }


type js_get_index =   {
  js_get_index_scopes : string list 
}

type js_set_index = {
  js_set_index_scopes : string list 
} 



type attr  = 
  | Js_global of js_global_val 
  | Js_module_as_var of  external_module_name
  | Js_module_as_fn of js_module_as_fn
  | Js_module_as_class of external_module_name             
  | Js_call of js_call 
  | Js_send of js_send
  | Js_new of js_new_val
  | Js_set of js_set
  | Js_get of js_get
  | Js_get_index of js_get_index
  | Js_set_index of js_set_index 

type return_wrapper = 
  | Return_unset 
  | Return_identity
  | Return_undefined_to_opt  
  | Return_null_to_opt
  | Return_null_undefined_to_opt
  | Return_to_ocaml_bool
  | Return_replaced_with_unit    

type t  = 
  | Ffi_bs of 
      External_arg_spec.t list  *
      return_wrapper * attr
  | Ffi_obj_create of obj_create
  | Ffi_normal 
  (* When it's normal, it is handled as normal c functional ffi call *)


val name_of_ffi : attr -> string

val check_ffi : ?loc:Location.t ->  attr -> unit 

val to_string : t -> string 

(** Note *)
val from_string : string -> t 

