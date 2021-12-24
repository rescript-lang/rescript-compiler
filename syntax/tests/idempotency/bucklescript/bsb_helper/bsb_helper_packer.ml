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

type pack_t = PackBytecode | PackNative

(* let ( // ) = Ext_path.combine *)

(* The packer is called with object files (.cmo / .cmx) which will be namespaced and we're using
   those names to read-in the mlast files which are not namespaced. So we strip the namespace
   before reading them in. *)
let module_of_filename filename =
  let str = Ext_filename.chop_extension_maybe filename in
  match (String.rindex str '-') with
  | exception Not_found -> str
  | len -> String.sub str 0 len

let pack pack_byte_or_native ~batch_files ~includes ~namespace ~warnings ~warn_error ~verbose ~cwd =
  let suffix_object_files, suffix_library_files, compiler, nested = begin match pack_byte_or_native with
  | PackBytecode -> Literals.suffix_cmo, Literals.suffix_cma , "ocamlc", "bytecode"
  | PackNative   -> Literals.suffix_cmx, Literals.suffix_cmxa, "ocamlopt", "native"
  end in
  let module_to_filepath = Ext_list.fold_left batch_files  String_map.empty
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
  let domain =
    String_map.fold dependency_graph String_set.empty
      (fun k _ acc -> String_set.add acc k)
      in
  let sorted_tasks = Bsb_helper_dep_graph.sort_files_by_dependencies ~domain dependency_graph in
  let all_object_files = Queue.fold
    (fun acc v -> match String_map.find_opt module_to_filepath v with
      | Some file -> (file ^ suffix_object_files) :: acc
      | None -> failwith @@ "build.ninja is missing the file '" ^ v ^ "' that was used in the project. Try force-regenerating but this shouldn't happen."
      )
    []
    sorted_tasks in
  let warning_command = if String.length warnings > 0 then
    "-w" :: warnings :: []
  else [] in
  let warning_command = if String.length warn_error > 0 then
    "-warn-error" :: warn_error :: warning_command
  else warning_command in

  (* This list will be reversed so we append the otherlibs object files at the end, and they'll end at the beginning. *)
  if all_object_files <> [] then
    let includes = Ext_list.fold_left includes [] (fun acc dir -> "-I" :: dir :: acc)  in
    let otherlibs = Bsb_helper_dep_graph.get_otherlibs_dependencies dependency_graph suffix_library_files in
    let all_object_files = match namespace with
      | None -> all_object_files
      | Some namespace -> (namespace ^ suffix_object_files) :: all_object_files
    in
    let all_object_files = List.rev (all_object_files @ otherlibs) in
    let compiler_extension = if Ext_sys.is_windows_or_cygwin then ".opt.exe" else ".opt" in
    let local_compiler = (Ext_path.combine (Ext_path.combine Bsb_global_paths.ocaml_dir "bin") compiler) ^ compiler_extension in

    let super_errors = if false then ["-bs-super-errors"] else [] in
    let list_of_args = (local_compiler :: "-a" :: "-g" ::
      warning_command) @ super_errors @ "-o" :: ((Ext_path.combine cwd Literals.library_file) ^ suffix_library_files) :: includes
      @ all_object_files in

    if verbose then
      print_endline("Bsb_helper pack command:\n" ^ (String.concat "  " list_of_args) ^ "\n");

    Unix.execvp
      local_compiler
        (Array.of_list list_of_args)
  else
    Bsb_exception.no_files_to_pack suffix_object_files
