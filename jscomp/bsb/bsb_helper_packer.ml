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

let pack pack_byte_or_native ~batch_files ~includes ~cwd =
  let ocaml_dir = Bsb_build_util.get_ocaml_dir cwd in
  let suffix_object_files, suffix_library_files, compiler, custom_flag = begin match pack_byte_or_native with
  | PackBytecode -> Literals.suffix_cmo, Literals.suffix_cma , Ext_filename.combine ocaml_dir "ocamlc.opt", true
  | PackNative   -> Literals.suffix_cmx, Literals.suffix_cmxa, Ext_filename.combine ocaml_dir "ocamlopt.opt", false
  end in
  let module_to_filepath = List.fold_left
    (fun m v ->
      String_map.add
      (Ext_filename.module_name_of_file_if_any v)
      (Ext_filename.chop_extension_if_any v)
      m)
    String_map.empty
    batch_files in
  let dependency_graph = List.fold_left
    (fun m file ->
      String_map.add
        (Ext_filename.module_name_of_file_if_any file)
        (Bsb_helper_extract.read_dependency_graph_from_mlast_file ((Ext_filename.chop_extension file) ^ Literals.suffix_mlast))
        m)
    String_map.empty
    batch_files in
  let domain =
    String_map.fold
      (fun k _ acc -> String_set.add k acc)
      dependency_graph String_set.empty in
  let sorted_tasks = Bsb_helper_dep_graph.sort_files_by_dependencies ~domain dependency_graph in
  let all_object_files = List.rev (Queue.fold
    (fun acc v -> match String_map.find_opt v module_to_filepath with
      | Some file -> (file ^ suffix_object_files) :: acc
      | None -> failwith @@ "build.ninja is missing the file '" ^ v ^ "' that was used in the project. Try force-regenerating but this shouldn't happen."
      )
    []
    sorted_tasks) in
  if all_object_files <> [] then
    let includes = List.fold_left (fun acc dir -> "-I" :: dir :: acc) [] includes in
    Unix.execvp
      compiler
        (Array.of_list (compiler :: "-a" :: "-o" :: (Literals.library_file ^ suffix_library_files) :: includes @ all_object_files))
  else
    failwith @@ "No " ^ suffix_object_files ^ " to pack into a lib."
