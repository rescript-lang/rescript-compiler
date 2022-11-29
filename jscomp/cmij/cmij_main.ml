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

type mode = Native | Playground of string list
(* 3rd party libraries folders paths *)

let get_files ext dir =
  Ext_array.filter_map (Sys.readdir dir) (fun x ->
      if Ext_string.ends_with x ext then Some (Filename.concat dir x) else None)
  |> Array.to_list

let from_cmj ~mode (files : string list) (output_file : string) : unit =
  let files =
    let cmp = Ext_filename.module_name in
    files
    |> List.sort (fun filea fileb ->
           Ext_string_array.cmp (cmp filea) (cmp fileb))
    |> Array.of_list
  in
  let module_names = Ext_array.map files Ext_filename.module_name in
  let module_data =
    Ext_array.map files (fun file ->
        let content : Js_cmj_format.t = Js_cmj_format.from_file file in
        let () =
          match mode with
          | Native -> (
              match content with
              | { case = Little; package_spec }
                when package_spec = Js_packages_info.runtime_package_specs ->
                  ()
              (*TODO: assert its suffixes*)
              | _ ->
                  Format.fprintf Format.err_formatter "@[%s: @[%a@]@]@." file
                    Js_packages_info.dump_packages_info content.package_spec;
                  assert false)
          | Playground _ -> ()
        in
        (* prerr_endline (Ext_obj.dump content.package_spec); *)
        let { Js_cmj_format.values; pure } = content in
        Cmij_cache.marshal_cmj_data { values; pure })
  in
  let cmj_cache = { Cmij_cache.module_names; module_data } in
  let v = open_out_bin output_file in
  Ext_pervasives.finally v ~clean:close_out (fun f ->
      Marshal.to_channel f cmj_cache [])

let from_cmi files output_file =
  let files =
    let cmp = Ext_filename.module_name in
    files
    |> List.sort (fun filea fileb ->
           Ext_string_array.cmp (cmp filea) (cmp fileb))
    |> Array.of_list
  in
  let module_names = Ext_array.map files Ext_filename.module_name in
  let module_data =
    Ext_array.map files (fun file ->
        let module_name = Ext_filename.module_name file in
        let cmi = Cmi_format.read_cmi file in
        assert (cmi.cmi_name = module_name);
        Cmij_cache.marshal_cmi_data cmi)
  in
  let cmi_cache = { Cmij_cache.module_names; module_data } in
  let v = open_out_bin output_file in
  Ext_pervasives.finally v ~clean:close_out (fun f ->
      Marshal.to_channel f cmi_cache [])

let stdlib = "stdlib-406"
let ( // ) = Filename.concat
let ( |~ ) = Ext_string.contain_substring

(* Assume cmij is run in the jscomp dir. *)
let jscomp_dir = Sys.getcwd ()
let lib_dir = jscomp_dir // ".." // "lib"
let cmi_target_file = lib_dir // "cmi_cache.bin"
let cmj_target_file = lib_dir // "cmj_cache.bin"

let mode =
  match Sys.argv with
  | [| _; "-playground"; folders |] ->
      Playground
        (Ext_list.filter (String.split_on_char ',' folders) (fun s -> s <> ""))
  | _ -> Native

let () =
  let third_party_cmj_files =
    match mode with
    | Native -> []
    | Playground folders ->
        List.fold_left
          (fun acc folder -> acc @ get_files Literals.suffix_cmj folder)
          [] folders
  in
  let cmj_files =
    get_files Literals.suffix_cmj (jscomp_dir // stdlib)
    @ get_files Literals.suffix_cmj (jscomp_dir // "others")
    @ third_party_cmj_files
  in
  from_cmj ~mode cmj_files cmj_target_file;
  let third_party_cmi_files =
    match mode with
    | Native -> []
    | Playground folders ->
        List.fold_left
          (fun acc folder -> acc @ get_files Literals.suffix_cmi folder)
          [] folders
  in
  let cmi_files =
    let files =
      get_files Literals.suffix_cmi (jscomp_dir // stdlib)
      @ get_files Literals.suffix_cmi (jscomp_dir // "others")
      @ third_party_cmi_files
    in
    Ext_list.filter files (fun x ->
        x |~ "js_OO" || x |~ "camlinternal" || not (x |~ "internal"))
  in
  from_cmi cmi_files cmi_target_file
