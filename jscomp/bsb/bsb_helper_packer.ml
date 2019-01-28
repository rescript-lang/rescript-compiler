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

#if BS_NATIVE then
type pack_t = PackBytecode | PackNative

let (//) = Ext_path.combine

(* The packer is called with object files (.cmo / .cmx) which will be namespaced and we're using
   those names to read-in the mlast files which are not namespaced. So we strip the namespace
   before reading them in. *)
let module_of_filename filename = 
  let str = Ext_path.chop_extension filename in
  match (String.rindex str '-') with 
  | exception Not_found -> str
  | len -> String.sub str 0 len

let pack pack_byte_or_native 
  ~main_module
  ~batch_files
  ~includes
  ~flags
  ~ocamlfind_packages
  ~bs_super_errors
  ~namespace
  ~warnings 
  ~warn_error
  ~verbose
  ~build_library
  ~build_artifacts_dir
  cwd =
  let suffix_object_files, suffix_library_files, compiler, nested, custom_flag = begin match pack_byte_or_native with
  | PackBytecode -> Literals.suffix_cmo, Literals.suffix_cma , "ocamlc", "bytecode", true
  | PackNative   -> Literals.suffix_cmx, Literals.suffix_cmxa, "ocamlopt", "native", false
  end in
  let module_to_filepath = List.fold_left
    (fun m v ->
      String_map.add m
      (Ext_modulename.module_name_of_file_if_any (module_of_filename v))
      (Ext_path.chop_extension_if_any v)
      )
    String_map.empty
    batch_files in
  let dependency_graph = List.fold_left
    (fun m file ->
      String_map.add m
        (Ext_modulename.module_name_of_file_if_any (module_of_filename file))
        (Bsb_helper_extract.read_dependency_graph_from_mlast_file ((module_of_filename file) ^ Literals.suffix_mlast))
        )
    String_map.empty
    batch_files in
  let all_object_files = match build_library with 
  | None -> 
    begin match main_module with 
    | None -> 
      let domain =
        String_map.fold
          (fun k _ acc -> String_set.add acc k)
          dependency_graph String_set.empty in
      let sorted_tasks = Bsb_helper_dep_graph.sort_files_by_dependencies ~domain dependency_graph in
      List.rev (Queue.fold
      (fun acc v -> match String_map.find_opt module_to_filepath v with
        | Some file -> (file ^ suffix_object_files) :: acc
        | None -> Bsb_exception.missing_object_file v
        )
      []
      sorted_tasks)
    | Some main_module -> 
      let tasks = Bsb_helper_dep_graph.simple_collect_from_main dependency_graph main_module in
      let namespace = match namespace with 
        | None -> ""
        | Some namespace -> "-" ^ namespace
      in
      Queue.fold
        (fun acc v -> match String_map.find_opt module_to_filepath v with
          | Some file -> (file ^ namespace ^ suffix_object_files) :: acc
          | None -> Bsb_exception.missing_object_file v
          )
        []
        tasks
    end
  | Some build_library -> 
    let tasks = Bsb_helper_dep_graph.simple_collect_from_main dependency_graph build_library in
    let namespace = match namespace with 
      | None -> ""
      | Some namespace -> "-" ^ namespace
    in
    List.rev (Queue.fold
        (fun acc v -> match String_map.find_opt module_to_filepath v with
          | Some file -> (file ^ namespace ^ suffix_object_files) :: acc
          | None -> Bsb_exception.missing_object_file v
          )
        []
        tasks)
  in
  
  let all_object_files = match namespace with
    | None -> all_object_files
    | Some namespace -> (namespace ^ suffix_object_files) :: all_object_files 
  in
  
  let warning_command = if String.length warnings > 0 then
    "-w" :: warnings :: []
  else [] in 
  let warning_command = if String.length warn_error > 0 then
    "-warn-error" :: warn_error :: warning_command
  else warning_command in 
    
  if all_object_files <> [] then
    let includes = List.fold_left (fun acc dir -> "-I" :: dir :: acc) [] includes in
    (* If there are no ocamlfind packages then let's not use ocamlfind, let's use the opt compiler instead.
       This is for mainly because we'd like to offer a "sandboxed" experience for those who want it.
       So if you don't care about opam dependencies you can solely rely on Bucklescript and npm, no need 
       to install ocamlfind. *)
    if ocamlfind_packages = [] then
      let compiler_extension = if Ext_sys.is_windows_or_cygwin then ".opt.exe" else ".opt" in
      let ocaml_dir = Bsb_build_util.get_ocaml_dir cwd in
      let compiler = ocaml_dir // "bin" // compiler ^ compiler_extension in

      let list_of_args = (compiler :: "-a" :: "-g"
        :: (if bs_super_errors then ["-bs-super-errors"] else []) )
        @ warning_command
        @ flags
        @ "-o" :: (build_artifacts_dir // Bsb_config.lib_bs // nested // Literals.library_file ^ suffix_library_files) :: includes 
        @ all_object_files in
      
      if verbose then
        print_endline("Bsb_helper pack command:\n" ^ (String.concat "  " list_of_args) ^ "\n");
        
      Unix.execvp
        compiler
          (Array.of_list list_of_args)
    else begin
      (* @CrossPlatform This might work on windows since we're using the Unix module which claims to
         have a windows implementation... We should double check this. *)
      let list_of_args = ("ocamlfind" :: compiler :: "-a" :: "-g" :: ocamlfind_packages) 
      @ ((if bs_super_errors then ["-passopt"; "-bs-super-errors"] else []))
      @ warning_command
      @ flags
      @  ("-o" :: (build_artifacts_dir // Bsb_config.lib_bs // nested // Literals.library_file ^ suffix_library_files) :: includes @ all_object_files) in
      
      if verbose then
        print_endline("Bsb_helper pack command:\n" ^ (String.concat "  " list_of_args) ^ "\n");
      
      Unix.execvp
        "ocamlfind"
          (Array.of_list list_of_args)
    end
  else
    Bsb_exception.no_files_to_pack suffix_object_files
#end
