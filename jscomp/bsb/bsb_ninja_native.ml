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


module Rules = Bsb_rule 

type compile_target_t = Native | Bytecode

let output_build = Bsb_ninja.output_build

let (//) = Ext_filename.combine

let (++) (us : Bsb_ninja.info) (vs : Bsb_ninja.info) =
  if us == Bsb_ninja.zero then vs else
  if vs == Bsb_ninja.zero then us
  else
    {
      all_config_deps  = us.all_config_deps @ vs.all_config_deps
    ;
      (* all_installs = us.all_installs @ vs.all_installs *)
    }

let install_file (file : string) files_to_install =
  String_hash_set.add  files_to_install (Ext_filename.chop_extension_if_any file )

let handle_file_group oc
  ~compile_target
  ~package_specs
  ~js_post_build_cmd
  (files_to_install : String_hash_set.t)
  acc
  (group: Bsb_build_ui.file_group) : Bsb_ninja.info =
  let handle_module_info  oc  module_name
      ( module_info : Binary_cache.module_info)
      info  =
    let installable =
      match group.public with
      | Export_all -> true
      | Export_none -> false
      | Export_set set ->  String_set.mem module_name set in
    let emit_build (kind : [`Ml | `Mll | `Re | `Mli | `Rei ])  file_input : Bsb_ninja.info =
      let filename_sans_extension = Filename.chop_extension file_input in
      let input = Bsb_config.proj_rel file_input in
      let output_file_sans_extension = filename_sans_extension in
      let output_ml = output_file_sans_extension ^ Literals.suffix_ml in
      let output_mlast = output_file_sans_extension  ^ Literals.suffix_mlast in
      let output_mlastd = output_file_sans_extension ^ Literals.suffix_mlastd in
      let output_mliast = output_file_sans_extension ^ Literals.suffix_mliast in
      let output_mliastd = output_file_sans_extension ^ Literals.suffix_mliastd in
      let output_cmi = output_file_sans_extension ^ Literals.suffix_cmi in
      let output_cmj_or_cmo =
        match compile_target with
        | Bytecode -> output_file_sans_extension ^ Literals.suffix_cmo
        | Native   -> output_file_sans_extension ^ Literals.suffix_cmx
      in
      let output_js =
        String_set.fold (fun s acc ->
          Bsb_config.package_output ~format:s (output_file_sans_extension ^ Literals.suffix_js)
          :: acc
          ) package_specs []
      in
      (* let output_mldeps = output_file_sans_extension ^ Literals.suffix_mldeps in  *)
      (* let output_mlideps = output_file_sans_extension ^ Literals.suffix_mlideps in  *)
      let shadows =
        ( "bs_package_flags",
          `Append
            (String_set.fold (fun s acc ->
                 Ext_string.inter2 acc (Bsb_config.package_flag ~format:s (Filename.dirname output_cmi))

               ) package_specs Ext_string.empty)
        ) ::
        (if group.dir_index = 0 then [] else
           [
             "bs_package_includes", `Append "$bs_package_dev_includes"
             ;
             ("bsc_extra_includes",
              `Overwrite
                ("${" ^ Bsb_build_util.string_of_bsb_dev_include group.dir_index ^ "}")
             )
           ]
        )
      in
      if kind = `Mll then
        output_build oc
          ~output:output_ml
          ~input
          ~rule: Rules.build_ml_from_mll ;
      begin match kind with
        | `Mll
        | `Ml
        | `Re ->
          let input, rule = 
            if kind = `Re then
              input, Rules.build_ast_and_deps_from_reason_impl_simple
            else if kind = `Mll then
              output_ml, Rules.build_ast_and_deps_simple
            else
              input, Rules.build_ast_and_deps_simple
          in
          begin
            output_build oc
              ~output:output_mlast
              (* ~implicit_outputs:[output_mldeps] *)
              ~input
              ~rule;
            let bin_deps_rule = begin match compile_target with
            | Bytecode -> Rules.build_bin_deps_bytecode
            | Native   -> Rules.build_bin_deps_native
            end in
            output_build
              oc
              ~output:output_mlastd
              ~input:output_mlast
              ~rule:bin_deps_rule
              ?shadows:(if group.dir_index = 0 then None
                else Some [Bsb_build_schemas.bsb_dir_group, `Overwrite (string_of_int group.dir_index)])
            ;
            let rule_name = begin match compile_target with
            | Bytecode -> Rules.build_cmo_cmi_bytecode
            | Native   -> Rules.build_cmx_cmi_native
            end in
            let cm_outputs, deps =
              if module_info.mli = Mli_empty then
                [output_cmi], []
              else    
                [], [output_cmi]
            in
            let shadows =
              match js_post_build_cmd with
              | None -> shadows
              | Some cmd ->
                ("postbuild",
                 `Overwrite ("&& " ^ cmd ^ Ext_string.single_space ^ String.concat Ext_string.single_space output_js)) :: shadows
            in
            output_build oc
              ~output:output_cmj_or_cmo
              ~shadows
              ~implicit_outputs:cm_outputs
              ~input:output_mlast
              ~implicit_deps:deps
              ~rule:rule_name ;
            if installable then begin install_file file_input files_to_install end;
            {all_config_deps = [output_mlastd]; 
            (* all_installs = [output_cmi];   *)
            }

          end
        | `Mli
        | `Rei ->
          let rule =
            if kind = `Mli then Rules.build_ast_and_deps_simple
            else Rules.build_ast_and_deps_from_reason_intf_simple
          in
          output_build oc
            ~output:output_mliast
            (* ~implicit_outputs:[output_mlideps] *)
            ~input
            ~rule;
          output_build oc
            ~output:output_mliastd
            ~input:output_mliast
            ~rule:Rules.build_bin_deps
            ?shadows:(if group.dir_index = 0 then None
                      else Some [Bsb_build_schemas.bsb_dir_group, `Overwrite (string_of_int group.dir_index)])
          ;
          let rule = begin match compile_target with
            | Bytecode -> Rules.build_cmi_bytecode
            | Native   -> Rules.build_cmi_native
            end in
          output_build oc
            ~shadows
            ~output:output_cmi
            ~input:output_mliast
            (* ~implicit_deps:[output_mliastd] *)
            ~rule;
          if installable then begin install_file file_input files_to_install end ;
          {
            all_config_deps = [output_mliastd];
            (* all_installs = [output_cmi]; *)
          }

      end
    in
    begin match module_info.ml with
      | Ml input -> emit_build `Ml input
      | Re input -> emit_build `Re input
      | Ml_empty -> Bsb_ninja.zero
    end ++
    begin match module_info.mli with
      | Mli mli_file  ->
        emit_build `Mli mli_file
      | Rei rei_file ->
        emit_build `Rei rei_file
      | Mli_empty -> Bsb_ninja.zero
    end ++
    begin match module_info.mll with
      | Some mll_file ->
        begin match module_info.ml with
          | Ml_empty -> emit_build `Mll mll_file
          | Ml input | Re input ->
            failwith ("both "^ mll_file ^ " and " ^ input ^ " are found in source listings" )
        end
      | None -> Bsb_ninja.zero
    end ++ info

  in
  String_map.fold (fun  k v  acc ->
      handle_module_info  oc k v acc
    ) group.sources  acc

let link oc ret ~root_project_entry ~file_groups ~static_libraries =
  let output, rule_name, suffix_cmo_or_cmx, main_module_name =
    begin match root_project_entry with
    | Bsb_config_types.JsTarget main_module_name       -> assert false
    | Bsb_config_types.BytecodeTarget main_module_name -> 
      (String.lowercase main_module_name) ^ ".byte"  , Rules.linking_bytecode, Literals.suffix_cmo, main_module_name
    | Bsb_config_types.NativeTarget main_module_name   -> 
      (String.lowercase main_module_name) ^ ".native", Rules.linking_native  , Literals.suffix_cmx, main_module_name
  end in
  let (all_mlast_files, all_cmo_or_cmx_files, all_cmi_files) =
    List.fold_left (fun acc group -> 
      String_map.fold (fun _ (v : Binary_cache.module_info) (all_mlast_files, all_cmo_or_cmx_files, all_cmi_files) -> 
        let mlname = match v.ml with
          | Ml input | Re input -> Some (Ext_filename.chop_extension_if_any input)
          | Ml_empty -> None
        in
        let mliname = match v.mli with
          | Mli input | Rei input -> Some (Ext_filename.chop_extension_if_any input)
          | Mli_empty -> None in
        begin match (mlname, mliname) with
        | None, None -> failwith "Got a source file without an ml or mli file. This should not happen."
        | Some name, Some _ ->
          ((name ^ Literals.suffix_mlast) :: all_mlast_files,
           (name ^ suffix_cmo_or_cmx)     :: all_cmo_or_cmx_files,
           (name ^ Literals.suffix_cmi)   :: all_cmi_files)
        | Some name, None ->
          ((name ^ Literals.suffix_mlast) :: all_mlast_files,
           (name ^ suffix_cmo_or_cmx)     :: all_cmo_or_cmx_files,
           (name ^ Literals.suffix_cmi)   :: all_cmi_files)
        | None, Some name ->
          (all_mlast_files,
           all_cmo_or_cmx_files,
           (name ^ Literals.suffix_cmi)   :: all_cmi_files)
        end    
      ) group.Bsb_build_ui.sources acc) 
    ([], [], [])
    file_groups in
  let shadows = [(
    "main_module",
    `Overwrite main_module_name
  ); ("static_libraries", `Overwrite (Bsb_build_util.flag_concat "-add-clib" static_libraries))] in
  output_build oc
    ~output
    ~input:""
    ~inputs:all_mlast_files
    ~implicit_deps:(all_cmi_files @ all_cmo_or_cmx_files)
    ~shadows
    ~rule:rule_name;
  ret
    
let pack oc ret  ~root_project_entry ~file_groups =
  let output_cma_or_cmxa, rule_name, suffix_cmo_or_cmx =
    begin match root_project_entry with
    (* These cases could benefit from a better error message. *)
    | Bsb_config_types.JsTarget _       -> assert false
    | Bsb_config_types.BytecodeTarget _ -> Literals.library_file ^ Literals.suffix_cma , Rules.build_cma_library , Literals.suffix_cmo
    | Bsb_config_types.NativeTarget _   -> Literals.library_file ^ Literals.suffix_cmxa, Rules.build_cmxa_library, Literals.suffix_cmx
  end in
  (* TODO(sansouci): we pack all source files of the dependency, but we could just pack the
     files that are used by the main project. *)
  let all_cmo_or_cmx_files, all_cmi_files =
    List.fold_left (fun acc group -> 
      String_map.fold (fun _ (v : Binary_cache.module_info) (all_cmo_or_cmx_files, all_cmi_files) -> 
        let mlname = match v.ml with
          | Ml input | Re input -> Some (Ext_filename.chop_extension_if_any input)
          | Ml_empty -> None
        in
        let mliname = match v.mli with
          | Mli input | Rei input -> Some (Ext_filename.chop_extension_if_any input)
          | Mli_empty -> None in
        match (mlname, mliname) with
        | None, None -> failwith "Got a source file without an ml or mli file. This should not happen."
        | Some name, Some _ ->
          ((name ^ suffix_cmo_or_cmx  ) :: all_cmo_or_cmx_files,
           (name ^ Literals.suffix_cmi) :: all_cmi_files)
        | Some name , None ->
          ((name ^ suffix_cmo_or_cmx  ) :: all_cmo_or_cmx_files,
           (name ^ Literals.suffix_cmi) :: all_cmi_files)
        | None, Some name ->
          (all_cmo_or_cmx_files, (name ^ Literals.suffix_cmi) :: all_cmi_files)
    ) group.Bsb_build_ui.sources acc) 
    ([], [])
    file_groups in
  (* In the case that a library is just an interface file, we don't do anything *)
  if List.length all_cmo_or_cmx_files > 0 then begin
    output_build oc
      ~output:output_cma_or_cmxa
      ~input:""
      ~inputs:all_cmo_or_cmx_files
      ~implicit_deps:all_cmi_files
      ~rule:rule_name ;
    ret ++ ({all_config_deps = []; 
    (* all_installs = [output_cma_or_cmxa];  *)
  })
  end else ret

let handle_file_groups oc
  ~root_project_entry
  ~compile_target
  ~is_top_level
  ~package_specs
  ~js_post_build_cmd
  ~files_to_install
  ~static_libraries
  (file_groups  :  Bsb_build_ui.file_group list) st =
  let ret = List.fold_left (
    handle_file_group oc 
      ~compile_target
      ~package_specs
      ~js_post_build_cmd 
      files_to_install
  ) st file_groups in
  if is_top_level then
    link oc ret ~root_project_entry ~file_groups ~static_libraries
  else
    pack oc ret ~root_project_entry ~file_groups
