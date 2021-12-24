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

type link_t = LinkBytecode of string | LinkNative of string

(* let ( // ) = Ext_path.combine *)

(* The linker is called with object files (.cmo / .cmx) which will be namespaced and we're using
   those names to read-in the mlast files which are not namespaced. So we strip the namespace
   before reading them in. *)
let module_of_filename filename =
  let str = Ext_filename.chop_extension_maybe filename in
  match (String.rindex str '-') with
  | exception Not_found -> str
  | len -> String.sub str 0 len

let link link_byte_or_native ~main_module ~batch_files ~includes ~ocaml_dependencies ~namespace ~warnings ~warn_error ~verbose ~cwd =
  let suffix_object_files, suffix_library_files, compiler, output_file = begin match link_byte_or_native with
  | LinkBytecode output_file -> Literals.suffix_cmo, Literals.suffix_cma , "ocamlc"  , output_file
  | LinkNative output_file   -> Literals.suffix_cmx, Literals.suffix_cmxa, "ocamlopt", output_file
  end in
  (* Map used to track the path to the files as the dependency_graph that we're going to read from the mlast file only contains module names *)
  let module_to_filepath = Ext_list.fold_left batch_files String_map.empty
    (fun m v ->
      String_map.add m
      (Ext_filename.module_name (module_of_filename v))
      (Ext_filename.chop_extension_maybe v)
      )
  in
  let dependency_graph = Ext_list.fold_left batch_files String_map.empty
    (fun m file ->
      let module_name = module_of_filename file in
      let suffix = if Sys.file_exists (module_name ^ Literals.suffix_mlast) then Literals.suffix_mlast
        else Literals.suffix_reast in
      String_map.add m
        (Ext_filename.module_name module_name)
        (Bsb_helper_extract.read_dependency_graph_from_mlast_file (module_name ^ suffix))
        )
  in
  let ocaml_dependencies =
    List.fold_left (fun acc v ->
      match v with
      | "threads" ->
      "-thread" :: ((Ext_path.combine (Ext_path.combine (Ext_path.combine (Ext_path.combine Bsb_global_paths.ocaml_dir "lib") "ocaml") "threads") "threads") ^ suffix_library_files) :: acc
      | v -> (Ext_path.combine (Ext_path.combine (Ext_path.combine (Bsb_global_paths.ocaml_dir "lib") "ocaml") v) ^ suffix_library_files) :: acc
      ) [] ocaml_dependencies in
  let warning_command = if String.length warnings > 0 then
  "-w" :: warnings :: []
     else [] in
  let warning_command = if String.length warn_error > 0 then
  "-warn-error" :: warn_error :: warning_command
  else warning_command in

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
    let library_files = Ext_list.fold_left includes []
      (fun acc dir ->
        (Ext_path.combine dir (Literals.library_file ^ suffix_library_files)) :: acc)
    in
    (* This list will be reversed so we append the otherlibs object files at the end, and they'll end at the beginning. *)
    let otherlibs = Bsb_helper_dep_graph.get_otherlibs_dependencies dependency_graph suffix_library_files in
    let all_object_files = ocaml_dependencies @ library_files @ List.rev (list_of_object_files @ otherlibs) in
    let compiler_extension = if Ext_sys.is_windows_or_cygwin then ".opt.exe" else ".opt" in
    let local_compiler = (Ext_path.combine (Ext_path.combine Bsb_global_paths.ocaml_dir "bin") compiler) ^ compiler_extension in
    let super_errors = if false then ["-bs-super-errors"] else [] in
    let list_of_args = (local_compiler :: "-g" ::
      warning_command) @ super_errors @ "-o" :: output_file :: all_object_files in
    if verbose then
      print_endline("Bsb_helper link command:\n" ^ (String.concat "  " list_of_args) ^ "\n");

    Unix.execvp local_compiler (Array.of_list (list_of_args))
  end else
    failwith @@ "No " ^ suffix_object_files ^ " to link. Hint: is the main module in the entries array right?"
