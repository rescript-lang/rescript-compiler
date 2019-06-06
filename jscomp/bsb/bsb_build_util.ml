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
let resolve_bsb_magic_file ~cwd ~desc p : string * bool =

  let no_slash = Ext_string.no_slash_idx p in
  if no_slash < 0 then
    (* Single file FIXME: better error message for "" input *)
    p, false  
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
      if Sys.file_exists path then path, true
      else 
        begin 
          Bsb_log.error "@{<error>Could not resolve @} %s in %s@." p cwd ; 
          failwith (p ^ " not found when resolving " ^ desc)
        end

    else
      (* relative path [./x/y]*)
      convert_and_resolve_path cwd p, true



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


let get_list_string_acc (s : Ext_json_types.t array) acc = 
  Ext_array.to_list_map_acc s acc (fun x ->
      match x with 
      | Str x -> Some x.str
      | _ -> None
    ) 

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

let rec walk_all_deps_aux 
  (visited : string String_hashtbl.t) 
  (paths : string list) 
  (top : bool) 
  (dir : string) 
  (cb : package_context -> unit) =
  let bsconfig_json =  dir // Literals.bsconfig_json in
  match Ext_json_parse.parse_json_from_file bsconfig_json with
  | Obj {map; loc} ->
    let cur_package_name = 
      match String_map.find_opt map Bsb_build_schemas.name with 
      | Some (Str {str }) -> str
      | Some _ 
      | None -> Bsb_exception.errorf ~loc "package name missing in %s/bsconfig.json" dir 
    in 
    let package_stacks = cur_package_name :: paths in 
    Bsb_log.info "@{<info>Package stack:@} %a @." pp_packages_rev
      package_stacks ;    
    if Ext_list.mem_string paths cur_package_name  then
      begin
        Bsb_log.error "@{<error>Cyclic dependencies in package stack@}@.";
        exit 2 
      end;
    if String_hashtbl.mem visited cur_package_name then 
      Bsb_log.info
        "@{<info>Visited before@} %s@." cur_package_name
    else 
      let explore_deps (deps : string) =   
        map
        |?
        (deps,
         `Arr (fun (new_packages : Ext_json_types.t array) ->             
             Ext_array.iter new_packages(fun js ->
                 match js with
                 | Str {str = new_package} ->
                   let package_dir = 
                     Bsb_pkg.resolve_bs_package ~cwd:dir 
                       (Bsb_pkg_types.string_as_package   new_package) in 
                   walk_all_deps_aux visited package_stacks  false package_dir cb  ;
                 | _ -> 
                   Bsb_exception.errorf ~loc 
                     "%s expect an array"
                     deps
               )))
        |> ignore in
      begin 
        explore_deps Bsb_build_schemas.bs_dependencies;          
        if top then explore_deps Bsb_build_schemas.bs_dependencies;
        cb {top ; cwd = dir};
        String_hashtbl.add visited cur_package_name dir;
      end
  | _ -> ()
  | exception _ -> 
    Bsb_exception.invalid_json bsconfig_json
    

let walk_all_deps dir cb = 
  let visited = String_hashtbl.create 0 in 
  walk_all_deps_aux visited [] true dir cb 
