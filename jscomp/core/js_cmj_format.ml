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



[@@@ocaml.warning "+9"]


type arity = 
  | Single of Lam_arity.t
  | Submodule of Lam_arity.t array

(* TODO: add a magic number *)
type cmj_value = {
  arity : arity ;
  closed_lambda : Lam.t option ; 
  (** Either constant or closed functor *)
}

type effect = string option


let single_na = Single Lam_arity.na
(** we don't force people to use package *)
type cmj_case = Ext_namespace.file_kind
  
type t = {
  values : cmj_value String_map.t;
  effect : effect;
  npm_package_path : Js_packages_info.t ;
  cmj_case : cmj_case; 
}

let cmj_magic_number =  "BUCKLE20171012"
let cmj_magic_number_length = 
  String.length cmj_magic_number

let pure_dummy = 
  {
    values = String_map.empty;
    effect = None;
    npm_package_path = Js_packages_info.empty;
    cmj_case = Little_js;
  }

let no_pure_dummy = 
  {
    values = String_map.empty;
    effect = Some Ext_string.empty;
    npm_package_path = Js_packages_info.empty;  
    cmj_case = Little_js; (** TODO: consistent with Js_config.bs_suffix default *)
  }



let from_file name : t =
  let ic = open_in_bin name in 
  let buffer = really_input_string ic cmj_magic_number_length in 
  if buffer <> cmj_magic_number then
    Ext_pervasives.failwithf ~loc:__LOC__ 
      "cmj files have incompatible versions, please rebuilt using the new compiler : %s" 
        __LOC__
  else 
    let v  : t = input_value ic in 
    close_in ic ;
    v 


let from_string s : t = 
  let magic_number = String.sub s 0 cmj_magic_number_length in 
  if magic_number = cmj_magic_number then 
    Marshal.from_string s  cmj_magic_number_length
  else 
    Ext_pervasives.failwithf ~loc:__LOC__ 
      "cmj files have incompatible versions, please rebuilt using the new compiler : %s"
        __LOC__

let rec for_sure_not_changed (name : string) ({npm_package_path ; effect; cmj_case ; values} : t) =   
  if Sys.file_exists name then 
    let data = from_file name in 
    Js_packages_info.equal data.npm_package_path npm_package_path &&
    for_sure_effect data.effect  effect &&
    data.cmj_case = cmj_case &&
    for_sure_equal data.values values
  else false  
and for_sure_effect (x : string option) (y : string option) = 
  match x, y with   
  | None, None -> true
  | Some _, Some _ -> true  (* we dont care about what effect it has when making use of cmj*)
  | None, Some _ -> false
  | Some _, None -> false
and for_sure_equal valuesa valuesb = 
  String_map.equal fore_sure_cmj_value valuesa valuesb 
and fore_sure_cmj_value (x : cmj_value) {arity; closed_lambda} =   
  for_sure_arity x.arity arity &&
  for_sure_eq_optional_lambda x.closed_lambda closed_lambda 
and for_sure_arity  (x : arity) y = 
  match x, y with 
  | Single x0, Single y0 -> Lam_arity.equal x0 y0
  | Submodule xs, Submodule ys -> 
    Ext_array.for_all2_no_exn  xs ys Lam_arity.equal
  | Single _, Submodule _ -> false
  | Submodule _, Single _ -> false
and for_sure_eq_optional_lambda 
  (lama : Lam.t option)  lamb = 
  match lama,lamb with 
  | None, None -> true 
  | None, Some _ 
  | Some _ , None -> false
  | Some a, Some b -> for_sure_lam a b 
and for_sure_lam (a : Lam.t) (b : Lam.t) = 
  match a, b with     
  | Lconst a0, Lconst b0 -> 
    Lam_constant.eq_approx a0 b0
  | _, _ -> false 
(* This may cause some build system always rebuild
  maybe should not be turned on by default
*) 
let to_file name ~check_exists (v : t) = 
  if  not (check_exists && for_sure_not_changed name v) then 
    let oc = open_out_bin name in 
    output_string oc cmj_magic_number;
    output_value oc v;
    close_out oc 


