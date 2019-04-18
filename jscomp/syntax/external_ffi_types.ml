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
  scopes : string list ;
}

type js_send = {
  name : string ;
  splice : bool ;
  pipe : pipe   ;
  js_send_scopes : string list;
} (* we know it is a js send, but what will happen if you pass an ocaml objct *)

type js_var = {
  name : string ;
  external_module_name : external_module_name option;
  scopes : string list ;
}

type js_new_val = {
  name : string ;
  external_module_name : external_module_name option;
  scopes : string list;
}

type js_module_as_fn =
  { external_module_name : external_module_name;
    splice : bool ;
  }
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
(** TODO: information between [arg_type] and [arg_label] are duplicated,
  design a more compact representation so that it is also easy to seralize by hand
*)
type arg_type = External_arg_spec.attr

type arg_label = External_arg_spec.label


(**TODO: maybe we can merge [arg_label] and [arg_type] *)
type obj_create = External_arg_spec.t list

type external_spec =
  | Js_var of js_var
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

(* let not_inlineable (x : external_spec) =     *)


let name_of_ffi ffi =
  match ffi with
  | Js_get_index _scope -> "[@@bs.get_index ..]"
  | Js_set_index _scope -> "[@@bs.set_index ..]"
  | Js_get { js_get_name = s} -> Printf.sprintf "[@@bs.get %S]" s
  | Js_set { js_set_name = s} -> Printf.sprintf "[@@bs.set %S]" s
  | Js_call v  -> Printf.sprintf "[@@bs.val %S]" v.name
  | Js_send v  -> Printf.sprintf "[@@bs.send %S]" v.name
  | Js_module_as_fn v  -> Printf.sprintf "[@@bs.val %S]" v.external_module_name.bundle
  | Js_new v  -> Printf.sprintf "[@@bs.new %S]" v.name
  | Js_module_as_class v
    -> Printf.sprintf "[@@bs.module] %S " v.bundle
  | Js_module_as_var v
    ->
    Printf.sprintf "[@@bs.module] %S " v.bundle
  | Js_var v (* FIXME: could be [@@bs.module "xx"] as well *)
    ->
    Printf.sprintf "[@@bs.val] %S " v.name

type return_wrapper =
  | Return_unset
  | Return_identity
  | Return_undefined_to_opt
  | Return_null_to_opt
  | Return_null_undefined_to_opt
  | Return_replaced_with_unit
type t  =
  | Ffi_bs of External_arg_spec.t list  *
     return_wrapper * external_spec
  (**  [Ffi_bs(args,return,attr) ]
       [return] means return value is unit or not,
        [true] means is [unit]
  *)
  | Ffi_obj_create of obj_create
  | Ffi_inline_const of Lam_constant.t
  | Ffi_normal
  (* When it's normal, it is handled as normal c functional ffi call *)



let valid_js_char =
  let a = Array.init 256 (fun i ->
      let c = Char.chr i in
      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' || c = '$'
    ) in
  (fun c -> Array.unsafe_get a (Char.code c))

let valid_first_js_char =
  let a = Array.init 256 (fun i ->
      let c = Char.chr i in
      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = '$'
    ) in
  (fun c -> Array.unsafe_get a (Char.code c))

(** Approximation could be improved *)
let valid_ident (s : string) =
  let len = String.length s in
  len > 0 && valid_js_char s.[0] && valid_first_js_char s.[0] &&
  (let module E = struct exception E end in
   try
     for i = 1 to len - 1 do
       if not (valid_js_char (String.unsafe_get s i)) then
         raise E.E
     done ;
     true
   with E.E -> false )

let is_package_relative_path (x : string) = 
     Ext_string.starts_with x "./" ||
     Ext_string.starts_with x "../"
  
let valid_global_name ?loc txt =
  if not (valid_ident txt) then
    let v = Ext_string.split_by ~keep_empty:true (fun x -> x = '.') txt in
    Ext_list.iter v
      (fun s ->
         if not (valid_ident s) then
           Location.raise_errorf ?loc "Not a valid global name %s"  txt
      ) 

(*
  We loose such check (see #2583),
  it also helps with the implementation deriving abstract [@bs.as]
*)

let valid_method_name ?loc txt =
    ()
  (* if not (valid_ident txt) then
    Location.raise_errorf ?loc "Not a valid method name %s"  txt *)



let check_external_module_name ?loc x =
  match x with
  | {bundle = ""; _ }
  | { module_bind_name = Phint_name "" } ->
    Location.raise_errorf ?loc "empty name encountered"
  | _ -> ()



let check_ffi ?loc ffi : bool =  
  let xrelative = ref false in 
  let upgrade bool =    
    if not (!xrelative) then xrelative := bool in 
  begin match ffi with
  | Js_var {name; external_module_name} ->     
    upgrade (is_package_relative_path name);
    Ext_option.iter external_module_name (fun name -> 
    upgrade (is_package_relative_path name.bundle));
    valid_global_name ?loc  name
  | Js_send {name }
  | Js_set  {js_set_name = name}
  | Js_get { js_get_name = name}
    ->  valid_method_name ?loc name
  | Js_get_index  _ (* TODO: check scopes *)
  | Js_set_index _
    -> ()

  | Js_module_as_var external_module_name
  | Js_module_as_fn {external_module_name; splice = _}
  | Js_module_as_class external_module_name
    -> 
      upgrade (is_package_relative_path external_module_name.bundle);
      check_external_module_name external_module_name
  | Js_new {external_module_name ;  name}
  | Js_call {external_module_name ;  name ; splice = _; scopes = _ }
    ->
    Ext_option.iter external_module_name (fun external_module_name ->
        upgrade (is_package_relative_path external_module_name.bundle));
    Ext_option.iter external_module_name (fun name ->
        check_external_module_name ?loc name
      );

    valid_global_name ?loc name 
  end; 
  !xrelative

let bs_prefix = "BS:"
let bs_prefix_length = String.length bs_prefix


(** TODO: Make sure each version is not prefix of each other
    Solution:
    1. fixed length
    2. non-prefix approach
*)
let bs_external = bs_prefix ^ Bs_version.version


let bs_external_length = String.length bs_external


let to_string  t =
  bs_external ^ Marshal.to_string t []


(* TODO:  better error message when version mismatch *)
let from_string s : t =
  let s_len = String.length s in
  if s_len >= bs_prefix_length &&
     String.unsafe_get s 0 = 'B' &&
     String.unsafe_get s 1 = 'S' &&
     String.unsafe_get s 2 = ':' then
    if Ext_string.starts_with s bs_external then
      Marshal.from_string s bs_external_length
    else
      Ext_pervasives.failwithf
        ~loc:__LOC__
        "Compiler version mismatch. The project might have been built with one version of BuckleScript, and then with another. Please wipe the artifacts and do a clean build."
  else Ffi_normal


let inline_string_primitive (s : string) (op : string option) : string list = 
  let lam : Lam_constant.t = 
    match op with 
    | Some op
    when Ast_utf8_string_interp.is_unicode_string op ->
      Const_unicode s
    | _ ->
      (Const_string s) in 
  [""; to_string (Ffi_inline_const lam )]

(* Let's only do it for string ATM
    for boolean, and ints, a good optimizer should     
    do it by default?
    But it may not work after layers of indirection
    e.g, submodule
*)
let inline_bool_primitive b : string list = 
  let lam : Lam_constant.t = 
    if  b then Lam_constant.Const_js_true 
    else Lam_constant.Const_js_false
  in 
  [""; to_string (Ffi_inline_const lam )]

(* FIXME: check overflow ?*)
let inline_int_primitive i : string list =   
  [""; 
    to_string 
    (Ffi_inline_const 
      (Lam_constant.Const_int32 (Int32.of_int i)))
  ]