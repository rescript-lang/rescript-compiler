(* Copyright (C) 2017 Authors of BuckleScript
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


let (//) = Ext_filename.combine 

let common_js_prefix p  =  Bsb_config.lib_js  // p
let amd_js_prefix p = Bsb_config.lib_amd // p 
let goog_prefix p = Bsb_config.lib_goog // p  
let es6_prefix p = Bsb_config.lib_es6 // p 
let es6_global_prefix p =  Bsb_config.lib_es6_global // p
let amdjs_global_prefix p = Bsb_config.lib_amd_global // p 
type t = String_set.t

let supported_format x = 
  x = Literals.amdjs ||
  x = Literals.commonjs ||
  x = Literals.goog ||
  x = Literals.es6 ||
  x = Literals.es6_global ||
  x = Literals.amdjs_global


let get_package_specs_from_array arr =  
  arr
  |> Bsb_build_util.get_list_string
  |> List.fold_left (fun acc x ->
      let v =
        if supported_format x    then String_set.add x acc
        else
          failwith ("Unkonwn package spec" ^ x) in
      v
    ) String_set.empty 

    
let bs_package_output = "-bs-package-output"

(** Assume input is valid 
    {[ -bs-package-output commonjs:lib/js/jscomp/test ]}
*)
let package_flag ~format dir =
  Ext_string.inter2
    bs_package_output 
    (Ext_string.concat3
       format
       Ext_string.single_colon
       (if format = Literals.amdjs then 
          amd_js_prefix dir 
        else if format = Literals.commonjs then 
          common_js_prefix dir 
        else if format = Literals.es6 then 
          es6_prefix dir 
        else if format = Literals.es6_global then 
          es6_global_prefix dir   
        else if format = Literals.amdjs_global then 
          amdjs_global_prefix dir 
        else goog_prefix dir))

let package_flag_of_package_specs (package_specs : t) 
  (dirname : string ) = 
    (String_set.fold (fun format acc ->
             Ext_string.inter2 acc (package_flag ~format dirname )

           ) package_specs Ext_string.empty)

let default_package_specs = String_set.singleton Literals.commonjs
(** js output for each package *)
let package_output ~format output=
  let prefix  =
    if format = Literals.commonjs then
      common_js_prefix
    else if format = Literals.amdjs then
      amd_js_prefix
    else if format = Literals.es6 then 
      es6_prefix   
    else if format = Literals.es6_global then 
      es6_global_prefix  
    else  if format = Literals.amdjs_global then 
      amdjs_global_prefix
    else goog_prefix
  in
  (Bsb_config.proj_rel @@ prefix output )

(**
    [get_list_of_output_js specs "src/hi/hello"]

*)
let get_list_of_output_js 
  package_specs output_file_sans_extension = 
      String_set.fold (fun format acc ->
          package_output ~format 
              (Ext_filename.output_js_basename output_file_sans_extension)
          :: acc
        ) package_specs []

