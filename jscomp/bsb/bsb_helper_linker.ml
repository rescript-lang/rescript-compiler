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
type link_t = LinkBytecode of string | LinkNative of string

let (//) = Ext_path.combine

let link link_byte_or_native 
  ~main_module
  ~batch_files
  ~clibs
  ~flags
  ~includes
  ~ocamlfind_packages
  ~bs_super_errors
  ~namespace
  ~ocaml_dependencies
  ~warnings 
  ~warn_error
  ~verbose
  ~build_artifacts_dir
  cwd =
  let ocaml_dir = Bsb_build_util.get_ocaml_dir cwd in
  let suffix_object_files, suffix_library_files, compiler, add_custom, output_file, nested = begin match link_byte_or_native with
  | LinkBytecode output_file -> Literals.suffix_cmo, Literals.suffix_cma , "ocamlc"  , true, output_file, "bytecode"
  | LinkNative output_file   -> Literals.suffix_cmx, Literals.suffix_cmxa, "ocamlopt", false, output_file, "native"
  end in
  (* Map used to track the path to the files as the dependency_graph that we're going to read from the mlast file only contains module names *)
  let module_to_filepath = List.fold_left
    (fun m v ->
      String_map.add m
      (Ext_modulename.module_name_of_file_if_any v)
      (Ext_path.chop_extension_if_any v)
      )
    String_map.empty
    batch_files in
  let dependency_graph = List.fold_left
    (fun m file ->
      String_map.add m
        (Ext_modulename.module_name_of_file_if_any file)
        (Bsb_helper_extract.read_dependency_graph_from_mlast_file ((Ext_path.chop_extension file) ^ Literals.suffix_mlast))
        )
    String_map.empty
    batch_files in
  let tasks = Bsb_helper_dep_graph.simple_collect_from_main dependency_graph main_module in
  let namespace = match namespace with 
    | None -> ""
    | Some namespace -> "-" ^ namespace
  in
  let list_of_object_files = Queue.fold
    (fun acc v -> match String_map.find_opt module_to_filepath v with
      | Some file -> (file ^ namespace ^ suffix_object_files) :: acc
      | None -> Bsb_exception.missing_object_file v
      )
    []
    tasks in
  if list_of_object_files <> [] then begin
    let library_files = List.fold_left
      (fun acc dir ->
        (Ext_path.combine dir (Literals.library_file ^ suffix_library_files)) :: acc)
      [] includes in
      (* We pass is_js:true here because that'll lead us to `bs-platform/lib/ocaml` which contains the JS artifacts, 
         and for belt the artifacts are under `native` or `byte` there.  *)
    let artifacts_dir = (Bsb_build_util.get_ocaml_lib_dir ~is_js:true cwd) // nested in
    let library_files = (artifacts_dir // (Literals.library_file ^ suffix_library_files)) :: library_files in
    let clibs = artifacts_dir // "stubs.o" :: clibs in
    (* This list will be reversed so we append the otherlibs object files at the end, and they'll end at the beginning. *)
    
    let suffix = begin match link_byte_or_native with
      | LinkBytecode _ -> Literals.suffix_cma
      | LinkNative _   -> Literals.suffix_cmxa
    end in

    let ocaml_dependencies = if ocamlfind_packages = [] then 
      List.fold_left (fun acc v -> 
        match v with
        | "compiler-libs" -> 
          ((ocaml_dir // "lib" // "ocaml" // "compiler-libs" // "ocamlcommon") ^ suffix) :: acc
        | "threads" -> 
          "-thread" :: (ocaml_dir // "lib" // "ocaml" // "threads" // "threads" ^ suffix) :: acc
        | v -> (ocaml_dir // "lib" // "ocaml" // v ^ suffix) :: acc
      ) [] ocaml_dependencies 
    else begin 
      List.fold_left (fun acc v -> 
        match v with
        | "compiler-libs" -> 
          "-package" :: "compiler-libs.common" :: acc
        | "threads" -> 
          "-thread" :: "-package" :: "threads" :: acc
        | "nums" -> 
          "-package" :: "num" :: acc
        | v -> "-package" :: v :: acc
      ) [] ocaml_dependencies
    end in 

    let clibs = if add_custom && clibs <> [] then
      "-custom" :: clibs
    else
      clibs
    in
    let warning_command = if String.length warnings > 0 then
      "-w" :: warnings :: []
    else [] in 
    let warning_command = if String.length warn_error > 0 then
      "-warn-error" :: warn_error :: warning_command
    else warning_command in 

    let static_libraries = Bsb_build_util.get_static_libraries 
      ~build_artifacts_dir
      ~clibs
      ~nested
      () in
    let clibs = static_libraries @ clibs in

    let all_object_files = ocaml_dependencies @ library_files @ List.rev (list_of_object_files) @ clibs in
    (* If there are no ocamlfind packages then let's not use ocamlfind, let's use the opt compiler instead.
       This is for mainly because we'd like to offer a "sandboxed" experience for those who want it.
       So if you don't care about opam dependencies you can solely rely on Bucklescript and npm, no need 
       to install ocamlfind. 
     *)
    if ocamlfind_packages = [] then
      let compiler_extension = if Ext_sys.is_windows_or_cygwin then ".opt.exe" else ".opt" in
      let compiler = ocaml_dir // compiler ^ compiler_extension in
      let list_of_args = (compiler :: "-g"
        :: (if bs_super_errors then ["-bs-super-errors"] else [])) 
        @ warning_command
        @ flags
        (* We filter out -thread because that'll lead to a linker warning like 
          "ld: warning: directory not found for option '-L/path/of/machine/where/artifacts/where/compiled" 
        *)
        @ "-o" :: output_file :: (List.filter (fun thing -> thing <> "-thread") all_object_files) in
      
      if verbose then
        print_endline("Bsb_helper link command:\n" ^ (String.concat "  " list_of_args) ^ "\n");
        
      Unix.execvp
        compiler
        (Array.of_list (list_of_args))
    else begin
      (* @CrossPlatform This might work on windows since we're using the Unix module which claims to
         have a windows implementation... We should double check this. *)
      let list_of_args = "ocamlfind" :: compiler 
        :: (if bs_super_errors then ["-passopt"; "-bs-super-errors"] else []) 
        @ ("-linkpkg" :: ocamlfind_packages)
        @ warning_command
        @ flags
        @ ("-g" :: "-o" :: output_file :: all_object_files) in
      
      if verbose then
        print_endline("Bsb_helper link command:\n" ^ (String.concat "  " list_of_args) ^ "\n");
        
      Unix.execvp
        "ocamlfind"
        (Array.of_list (list_of_args))
    end
  end else
    Bsb_exception.no_files_to_link suffix_object_files main_module
#end
