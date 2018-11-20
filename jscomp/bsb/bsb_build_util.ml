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

let flag_concat flag xs =   
  String.concat Ext_string.single_space
    (Ext_list.flat_map xs  (fun x -> [flag ; x]))
  
let (//) = Ext_path.combine


(*TODO: optimize *)
let ppx_flags xs =
  flag_concat "-ppx"
    (Ext_list.map xs Filename.quote)

let pp_flag (xs : string) = 
   "-pp " ^ Filename.quote xs

let include_dirs = flag_concat "-I"


(* we use lazy $src_root_dir *)



(* It does several conversion:
   First, it will convert unix path to windows backward on windows platform.
   Then if it is absolute path, it will do thing
   Else if it is relative path, it will be rebased on project's root directory  *)

let convert_and_resolve_path : string -> string -> string =
  if Sys.unix then (//)
  else fun cwd path ->
    if Ext_sys.is_windows_or_cygwin then 
      let p = Ext_string.replace_slash_backward path in
      cwd // p
    else failwith ("Unknown OS :" ^ Sys.os_type)
(* we only need convert the path in the beginning *)


(* Magic path resolution:
   foo => foo
   foo/ => /absolute/path/to/projectRoot/node_modules/foo
   foo/bar => /absolute/path/to/projectRoot/node_modules/foo/bar
   /foo/bar => /foo/bar
   ./foo/bar => /absolute/path/to/projectRoot/./foo/bar
   Input is node path, output is OS dependent (normalized) path
*)
let resolve_bsb_magic_file ~cwd ~desc p =
  let no_slash = Ext_string.no_slash_idx p in
  if no_slash < 0 then
    p (*FIXME: better error message for "" input *)
  else 
  let first_char = String.unsafe_get p 0 in 
  if Filename.is_relative p &&  
     first_char  <> '.' then
    let package_name, rest = 
      Bsb_pkg_types.extract_pkg_name_and_file p 
    in 
    let relative_path = 
        if Ext_sys.is_windows_or_cygwin then Ext_string.replace_slash_backward rest 
        else rest in       
    (* let p = if Ext_sys.is_windows_or_cygwin then Ext_string.replace_slash_backward p else p in *)
    let package_dir = Bsb_pkg.resolve_bs_package ~cwd package_name in
    let path = package_dir // relative_path in 
    if Sys.file_exists path then path
    else 
      begin 
        Bsb_log.error "@{<error>Could not resolve @} %s in %s@." p cwd ; 
        failwith (p ^ " not found when resolving " ^ desc)
      end

  else
    (* relative path [./x/y]*)
    convert_and_resolve_path cwd p



(** converting a file from Linux path format to Windows *)

(**
   If [Sys.executable_name] gives an absolute path, 
   nothing needs to be done.
   
   If [Sys.executable_name] is not an absolute path, for example
   (rlwrap ./ocaml)
   it is a relative path, 
   it needs be adapted based on cwd
*)

let get_bsc_dir ~cwd = 
  Filename.dirname 
    (Ext_path.normalize_absolute_path 
       (Ext_path.combine cwd  Sys.executable_name))


let get_bsc_bsdep cwd = 
  let dir = get_bsc_dir ~cwd in    
  Filename.concat dir  "bsc.exe", 
  Filename.concat dir  "bsb_helper.exe"

(** 
   {[
     mkp "a/b/c/d";;
     mkp "/a/b/c/d"
   ]}
*)
let rec mkp dir = 
  if not (Sys.file_exists dir) then 
    let parent_dir  = Filename.dirname dir in
    if  parent_dir = Filename.current_dir_name then 
      Unix.mkdir dir 0o777 (* leaf node *)
    else 
      begin 
        mkp parent_dir ; 
        Unix.mkdir dir 0o777 
      end
  else if not  @@ Sys.is_directory dir then 
    failwith ( dir ^ " exists but it is not a directory, plz remove it first")
  else ()


let get_list_string_acc s acc = 
  Ext_array.to_list_map_acc  (fun (x : Ext_json_types.t) ->
      match x with 
      | Str x -> Some x.str
      | _ -> None
    ) s  acc 

let get_list_string s = get_list_string_acc s []   


(* Key is the path *)
let (|?)  m (key, cb) =
  m  |> Ext_json.test key cb

type package_context = {
  cwd : string ; 
  top : bool ; 
}

(**
   TODO: check duplicate package name
   ?use path as identity?

   Basic requirements
     1. cycle detection
     2. avoid duplication
     3. deterministic, since -make-world will also comes with -clean-world

*)

let pp_packages_rev ppf lst = 
  Ext_list.rev_iter lst (fun  s ->  Format.fprintf ppf "%s " s) 

let rec walk_all_deps_aux visited paths top dir cb =
  let bsconfig_json =  (dir // Literals.bsconfig_json) in
  match Ext_json_parse.parse_json_from_file bsconfig_json with
  | Obj {map; loc} ->
    let cur_package_name = 
      match String_map.find_opt map Bsb_build_schemas.name with 
      | Some (Str {str }) -> str
      | Some _ 
      | None -> Bsb_exception.errorf ~loc "package name missing in %s/bsconfig.json" dir 
    in 
    let package_stacks = cur_package_name :: paths in 
    let () = 
      Bsb_log.info "@{<info>Package stack:@} %a @." pp_packages_rev
        package_stacks 
    in 
    if List.mem cur_package_name paths then
      begin
        Bsb_log.error "@{<error>Cyclic dependencies in package stack@}@.";
        exit 2 
      end;
    if String_hashtbl.mem visited cur_package_name then 
      Bsb_log.info
        "@{<info>Visited before@} %s@." cur_package_name
    else 
      begin 
        map
        |?
        (Bsb_build_schemas.bs_dependencies,
         `Arr (fun (new_packages : Ext_json_types.t array) ->             
             Ext_array.iter new_packages(fun js ->
                 begin match js with
                   | Str {str = new_package} ->
                     let package_dir = 
                       Bsb_pkg.resolve_bs_package ~cwd:dir 
                        (Bsb_pkg_types.string_as_package   new_package) in 
                     walk_all_deps_aux visited package_stacks  false package_dir cb  ;
                   | _ -> 
                     Bsb_exception.errorf ~loc 
                       "%s expect an array"
                       Bsb_build_schemas.bs_dependencies
                 end
               )))
        |> ignore ;
        if top then begin
          map
          |?
          (Bsb_build_schemas.bs_dev_dependencies,
           `Arr (fun (new_packages : Ext_json_types.t array) ->               
               Ext_array.iter new_packages (fun (js : Ext_json_types.t) ->
                   match js with
                   | Str {str = new_package} ->
                     let package_dir = 
                       Bsb_pkg.resolve_bs_package ~cwd:dir 
                        (Bsb_pkg_types.string_as_package new_package) in 
                     walk_all_deps_aux visited package_stacks  false package_dir cb  ;
                   | _ -> 
                     Bsb_exception.errorf ~loc 
                       "%s expect an array"
                       Bsb_build_schemas.bs_dev_dependencies

                 )))
          |> ignore ;
        end
        ;
        cb {top ; cwd = dir};
        String_hashtbl.add visited cur_package_name dir;
      end
  | _ -> ()
  | exception _ -> 
    Bsb_exception.invalid_json bsconfig_json
    

let walk_all_deps dir cb = 
  let visited = String_hashtbl.create 0 in 
  walk_all_deps_aux visited [] true dir cb 

#if BS_NATIVE then
let get_ocaml_dir cwd =
  (Filename.dirname (get_bsc_dir ~cwd)) // "vendor" // "ocaml"

let get_ocaml_lib_dir ~is_js cwd =
  if is_js then (Filename.dirname (get_bsc_dir ~cwd)) // "lib" // "ocaml"
  else (get_ocaml_dir cwd) // "lib" // "ocaml"

let build_artifacts_dir = ref None

let get_build_artifacts_location cwd =
  (* If the project's parent folder is not node_modules, we know it's the top level one. *)
  if (Filename.basename (Filename.dirname cwd)) <> "node_modules" then
    match !build_artifacts_dir with
    | None -> cwd
    | Some dir -> dir
  else begin
    match !build_artifacts_dir with
    | None -> cwd
      (* (Filename.dirname (Filename.dirname cwd)) // Bsb_config.lib_lit // Bsb_config.node_modules // project_name *)
    | Some dir ->
      let project_name = Filename.basename cwd in
      dir // Bsb_config.lib_lit // Bsb_config.node_modules // project_name
  end

let get_bs_ppx_tools root_project_dir = 
  let bs_ppx_tools = root_project_dir // Literals.node_modules // Bs_version.package_name // "lib" // "bs_ppx_tools.exe" in
  if Sys.file_exists bs_ppx_tools then 
    bs_ppx_tools
  else begin
    let bs_ppx_tools = (Filename.dirname root_project_dir) // Bs_version.package_name // "lib" // "bs_ppx_tools.exe" in
    if (Filename.basename (Filename.dirname root_project_dir)) = "node_modules" 
        && Sys.file_exists bs_ppx_tools then 
      bs_ppx_tools
    else
      Bsb_exception.bs_ppx_tools_not_found ()
  end

let get_static_libraries ~build_artifacts_dir ?clibs:(clibs=[]) ?package_name ~nested () =
  let artifacts_installed = ref [] in
  let filename = build_artifacts_dir // Bsb_config.lib_bs // nested // Literals.dot_static_libraries in
  if not (Sys.file_exists filename) then []
  else begin
    let ic = open_in_bin filename in
    (try
       while true do
         artifacts_installed := (String.trim (input_line ic)) :: !artifacts_installed
       done
     with End_of_file -> ());
     close_in ic;

    let package_name = match package_name with
      | None -> "Your package's"
      | Some package_name -> "package: " ^ package_name
    in
    (* This is just for the 3.0 release, so it goes a bit smoother. Once all of our packages 
       are fixed we don't need to dedupe. 
                April 17th 2018
     *)
    (List.filter (fun i -> 
      let is_already_linked = List.mem i clibs in
        if is_already_linked then 
          Bsb_log.warn "@{<warn>Warning@} %s `static-libraries` doesn't need to have '%s' \
                        as it's automatically linked by the build-script, you can safely remove it from that list.@." package_name i;
      not is_already_linked) !artifacts_installed)
    end

#end
