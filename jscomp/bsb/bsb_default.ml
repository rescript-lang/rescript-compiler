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

let get_list_string = Bsb_build_util.get_list_string
let (//) = Ext_filename.combine


(* Magic path resolution:
   foo => foo 
   foo/ => /absolute/path/to/projectRoot/node_modules/foo
   foo/bar => /absolute/path/to/projectRoot/node_modules/foo.bar
   /foo/bar => /foo/bar
   ./foo/bar => /absolute/path/to/projectRoot/./foo/bar 
   Input is node path, output is OS dependent path
*)
let resolve_bsb_magic_file ~cwd ~desc p =
  let p_len = String.length p in 
  let no_slash = Ext_string.no_slash p in  
  if no_slash then 
    p 
  else if Filename.is_relative p &&
     p_len > 0 &&
     String.unsafe_get p 0 <> '.' then
    let name = String.sub p 0 (String.index p '/') in
    let package = (Bs_pkg.resolve_bs_package ~cwd name) in
    match package with
    | None -> failwith (name ^ " not found when resolving " ^ desc)
    | Some package -> Bsb_build_util.convert_and_resolve_path (Filename.dirname package // p)
  else
    Bsb_build_util.convert_and_resolve_path p

(* Key is the path *)
let (|?)  m (key, cb) =
  m  |> Ext_json.test key cb



(** 
  TODO: check duplicate package name 
   ?use path as identity?
*)
let rec walk_all_deps dir cb =   
  let bsconfig_json =  (dir // Literals.bsconfig_json) in 
  match Ext_json.parse_json_from_file bsconfig_json with 
  | `Obj map ->
    map
    |? 
    (Bsb_build_schemas.bs_dependencies, 
      `Arr (fun (new_packages : Ext_json.t array) -> 
         new_packages
         |> Array.iter (fun (js : Ext_json.t) -> 
          begin match js with 
          | `Str {Ext_json.str = new_package} -> 
            begin match Bs_pkg.resolve_bs_package ~cwd:dir new_package with 
            | None -> failwith (new_package ^ " not found as dependency of " ^ bsconfig_json )
            | Some package_dir  -> 
              walk_all_deps package_dir cb  ;              
            end;            
          | _ -> () (* TODO: add a log framework, warning here *)
          end 
      )))
    |> ignore ;
    cb dir 
  | _ -> ()
  | exception _ -> failwith ( "failed to parse" ^ bsconfig_json ^ " properly")

let package_name = ref None
let set_package_name s = package_name := Some s
let get_package_name () = !package_name




let bsc_flags = ref []
let get_bsc_flags () = !bsc_flags
let set_bsc_flags s = bsc_flags := get_list_string s




let bs_dependencies = ref []
let get_bs_dependencies () = !bs_dependencies
let set_bs_dependencies  s =
  bs_dependencies := get_list_string s


let bs_external_includes = ref []
let set_bs_external_includes s =
  bs_external_includes := List.map Bsb_build_util.convert_and_resolve_path (get_list_string s )
let get_bs_external_includes () = !bs_external_includes


let ocamllex =  ref  "ocamllex.opt"
let set_ocamllex ~cwd s = 
  ocamllex := resolve_bsb_magic_file ~cwd ~desc:"ocamllex" s
let get_ocamllex () = !ocamllex



let refmt = ref "refmt"
let get_refmt () = !refmt
let set_refmt ~cwd p = 
  refmt := resolve_bsb_magic_file ~cwd ~desc:"refmt" p


let ppx_flags = ref []
let get_ppx_flags () = !ppx_flags
let set_ppx_flags ~cwd s =
  let s =
    s (* TODO: unix conversion *)
    |> get_list_string
    |> List.map (fun p ->
        if p = "" then failwith "invalid ppx, empty string found"
        else resolve_bsb_magic_file ~cwd ~desc:"ppx" p
      ) in
  ppx_flags := s


let js_post_build_cmd = ref None 
let get_js_post_build_cmd () = !js_post_build_cmd
let set_js_post_build_cmd ~cwd s =
  js_post_build_cmd := Some (resolve_bsb_magic_file ~cwd ~desc:"js-post-build:cmd" s )

let ninja = ref "ninja"
let get_ninja () = !ninja

(* Setting ninja is a bit complex
   First if [build.ninja] does use [ninja] we need set a variable
   Second we need store it so that we can call ninja correctly
*)
let set_ninja ~cwd p  =
  ninja := resolve_bsb_magic_file ~cwd ~desc:"ninja" p 


type package_specs = String_set.t

let package_specs = ref (String_set.singleton Literals.commonjs)

let get_package_specs () = !package_specs

let set_package_specs_from_array arr = 
    let new_package_specs = 
      arr 
      |> get_list_string
      |> List.fold_left (fun acc x ->
          let v = 
            if x = Literals.amdjs || x = Literals.commonjs || x = Literals.goog   then String_set.add x acc
            else   
              failwith ("Unkonwn package spec" ^ x) in 
          v
        ) String_set.empty in 
   package_specs := new_package_specs

let generate_merlin = ref false

let get_generate_merlin () = !generate_merlin 

let set_generate_merlin b = 
  generate_merlin := b
