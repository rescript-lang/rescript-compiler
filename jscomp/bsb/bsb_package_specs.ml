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


let (//) = Ext_path.combine 



(* TODO: sync up with {!Js_packages_info.module_system}  *)
type format = 
  | NodeJS | Es6 | Es6_global

type spec = {
  format : format;
  in_source : bool 
}

module Spec_set = Set.Make( struct type t = spec 
    let compare = Pervasives.compare 
  end)

type t = Spec_set.t 


let bad_module_format_message_exn ~loc format =
  Bsb_exception.errorf ~loc "package-specs: `%s` isn't a valid output module format. It has to be one of:  %s, %s or %s"
    format
    Literals.commonjs
    Literals.es6
    Literals.es6_global

let supported_format (x : string) loc = 
  if x = Literals.commonjs then NodeJS
  else if x = Literals.es6 then Es6
  else if x = Literals.es6_global then Es6_global
  else bad_module_format_message_exn ~loc x 

let string_of_format (x : format) =
  match x with 
  | NodeJS -> Literals.commonjs
  | Es6 -> Literals.es6
  | Es6_global -> Literals.es6_global

let prefix_of_format (x : format) s =   
  (match x with 
  | NodeJS -> Bsb_config.lib_js 
  | Es6 -> Bsb_config.lib_es6 
  | Es6_global -> Bsb_config.lib_es6_global ) // s

let rec from_array (arr : Ext_json_types.t array) : Spec_set.t =
  let spec = ref Spec_set.empty in
  let has_in_source = ref false in
  Ext_array.iter arr (fun x ->
      let result = from_json_single x  in
      if result.in_source then 
        (
          if not !has_in_source then
            has_in_source:= true
          else 
            Bsb_exception.errorf 
              ~loc:(Ext_json.loc_of x) 
              "package-specs: we've detected two module formats that are both configured to be in-source." 
        );
      spec := Spec_set.add result !spec
    );
  !spec

(* TODO: FIXME: better API without mutating *)
and from_json_single (x : Ext_json_types.t) : spec =
  match x with
  | Str {str = format; loc } ->    
      {format = supported_format format loc  ; in_source = false }    
  | Obj {map; loc} ->
    begin match String_map.find_exn map "module" with
      | Str {str = format} ->
        let in_source = 
          match String_map.find_opt map  Bsb_build_schemas.in_source with
          | Some (True _) -> true
          | Some _
          | None -> false
        in        
          {format = supported_format format loc ; in_source  }        
      | Arr _ ->
        Bsb_exception.errorf ~loc
          "package-specs: when the configuration is an object, `module` field should be a string, not an array. If you want to pass multiple module specs, try turning package-specs into an array of objects (or strings) instead."
      | _ ->
        Bsb_exception.errorf ~loc
          "package-specs: the `module` field of the configuration object should be a string."
      | exception _ ->
        Bsb_exception.errorf ~loc
          "package-specs: when the configuration is an object, the `module` field is mandatory."
    end
  | _ -> Bsb_exception.errorf ~loc:(Ext_json.loc_of x)
           "package-specs: we expect either a string or an object."

let  from_json (x : Ext_json_types.t) : Spec_set.t =
  match x with
  | Arr {content ; _} -> from_array content
  | _ -> Spec_set.singleton (from_json_single x )


let bs_package_output = "-bs-package-output"

(** Assume input is valid 
    {[ -bs-package-output commonjs:lib/js/jscomp/test ]}
*)
let package_flag ({format; in_source } : spec) dir =
  Ext_string.inter2
    bs_package_output 
    (Ext_string.concat3
       (string_of_format format)
       Ext_string.single_colon
       (if in_source then dir else
        prefix_of_format format dir))

let package_flag_of_package_specs (package_specs : t) 
    (dirname : string ) : string  = 
  Spec_set.fold (fun format acc ->
      Ext_string.inter2 acc (package_flag format dirname )
    ) package_specs Ext_string.empty

let default_package_specs = 
  Spec_set.singleton 
    { format = NodeJS ; in_source = false }



(**
    [get_list_of_output_js specs "src/hi/hello"]

*)
let get_list_of_output_js 
    (package_specs : Spec_set.t)
    (bs_suffix : bool)
    (output_file_sans_extension : string)
    = 
  Spec_set.fold 
    (fun (format : spec) acc ->
        let basename =  Ext_namespace.change_ext_ns_suffix
             output_file_sans_extension
             (if bs_suffix then Literals.suffix_bs_js else Literals.suffix_js)
        in 
        (Bsb_config.proj_rel @@ (if format.in_source then basename
        else prefix_of_format format.format basename))         
       :: acc
    ) package_specs []

