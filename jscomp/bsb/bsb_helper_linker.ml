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

let link link_byte_or_native ~main_module ~batch_files ~includes =
  let suffix_object_files, suffix_library_files, compiler, output_file = begin match link_byte_or_native with
  | LinkBytecode output_file -> Literals.suffix_cmo, Literals.suffix_cma , "ocamlc.opt"  , output_file
  | LinkNative output_file   -> Literals.suffix_cmx, Literals.suffix_cmxa, "ocamlopt.opt", output_file
  end in
  (* Map used to track the path to the files as the dependency_graph that we're going to read from the mlast file only contains module names *)
  let module_to_filepath = List.fold_left
    (fun m v ->
      String_map.add
      (Ext_modulename.module_name_of_file_if_any v)
      (Ext_path.chop_extension_if_any v)
      m)
    String_map.empty
    batch_files in
  let dependency_graph = List.fold_left
    (fun m file ->
      String_map.add
        (Ext_modulename.module_name_of_file_if_any file)
        (Bsb_helper_extract.read_dependency_graph_from_mlast_file ((Ext_path.chop_extension file) ^ Literals.suffix_mlast))
        m)
    String_map.empty
    batch_files in
  let tasks = Bsb_helper_dep_graph.simple_collect_from_main dependency_graph main_module in
  let list_of_object_files = Queue.fold
    (fun acc v -> match String_map.find_opt v module_to_filepath with
      | Some file -> (file ^ suffix_object_files) :: acc
      | None -> failwith @@ "build.ninja is missing the file '" ^ v ^ "' that was used in the project. Try force-regenerating but this shouldn't happen."
      )
    []
    tasks in
  if list_of_object_files <> [] then begin
    let library_files = List.fold_left
      (fun acc dir ->
        (Ext_path.combine dir (Literals.library_file ^ suffix_library_files)) :: acc)
      [] includes in
    (* This list will be reversed so we append the otherlibs object files at the end, and they'll end at the beginning. *)
    let otherlibs = Bsb_helper_dep_graph.get_otherlibs_dependencies dependency_graph suffix_library_files in
    let all_object_files = List.rev (list_of_object_files @ otherlibs) in
    Unix.execvp
      compiler
      (Array.of_list (compiler :: "-o" :: output_file :: library_files @ all_object_files))
  end else
    failwith @@ "No " ^ suffix_object_files ^ " to link. Hint: is the main module in the entries array right?"
