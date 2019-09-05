(* Copyright (C) 2017 Authors of BuckleScript
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

type compile_target_t = Native | Bytecode

let (//) = Ext_path.combine






let handle_generators oc 
    (group : Bsb_file_groups.file_group) 
    custom_rules =   
  let map_to_source_dir = 
    (fun x -> Bsb_config.proj_rel (group.dir //x )) in  
  Ext_list.iter group.generators (fun {output; input; command} -> 
      (*TODO: add a loc for better error message *)
      match Map_string.find_opt custom_rules command with 
      | None -> Ext_fmt.failwithf ~loc:__LOC__ "custom rule %s used but  not defined" command
      | Some rule -> 
        Bsb_ninja_targets.output_build oc 
          ~outputs:(Ext_list.map  output  map_to_source_dir)
          ~inputs:(Ext_list.map input map_to_source_dir) 
          ~rule
    )


let make_common_shadows
    package_specs
    dirname
    dir_index
  : Bsb_ninja_targets.shadow list
  =

    { key = Bsb_ninja_global_vars.g_pkg_flg;
      op =
        Append
          (Bsb_package_specs.package_flag_of_package_specs
             package_specs dirname
          )
    } ::
    (if Bsb_dir_index.is_lib_dir dir_index  then [] else
       [
        { key =  Bsb_ninja_global_vars.g_dev_incls;
          op = OverwriteVar (Bsb_dir_index.string_of_bsb_dev_include dir_index);
        }
       ]
    )



let emit_module_build
    (rules : Bsb_ninja_rule.builtin)
    (package_specs : Bsb_package_specs.t)
    (group_dir_index : Bsb_dir_index.t)
    oc
    ~bs_suffix
    js_post_build_cmd
    ~root_project_dir
    ~compile_target
    namespace
    (module_info : Bsb_db.module_info)
  : unit =
  let has_intf_file = module_info.info = Ml_mli in 
  let is_re = module_info.is_re in 
  let filename_sans_extension = module_info.name_sans_extension in 
  let is_dev = not (Bsb_dir_index.is_lib_dir group_dir_index) in
  let input_impl = 
    Bsb_config.proj_rel 
      (filename_sans_extension ^ if is_re then  Literals.suffix_re else  Literals.suffix_ml  ) in
  let input_intf =      
    Bsb_config.proj_rel 
      (filename_sans_extension ^ if is_re then  Literals.suffix_rei else  Literals.suffix_mli) in
  let output_mlast = 
    filename_sans_extension  ^ if is_re then Literals.suffix_reast else Literals.suffix_mlast in
  let output_mliast = 
    filename_sans_extension  ^ if is_re then Literals.suffix_reiast else Literals.suffix_mliast in
  let output_mlast_simple = filename_sans_extension  ^ Literals.suffix_mlast_simple in
  let output_mliast_simple = filename_sans_extension  ^ Literals.suffix_mliast_simple in
  let output_d = filename_sans_extension ^ Literals.suffix_d in
  let output_filename_sans_extension =
      Ext_namespace_encode.make ?ns:namespace filename_sans_extension
  in
  let output_cmi =  output_filename_sans_extension ^ Literals.suffix_cmi in
  let output_cmx_or_cmo =
     match compile_target with
     | Bytecode -> output_filename_sans_extension ^ Literals.suffix_cmo
     | Native   -> output_filename_sans_extension ^ Literals.suffix_cmx
   in
  let common_shadows =
    make_common_shadows package_specs
      (Filename.dirname output_cmi)
      group_dir_index in
  let ast_rule =     
    if is_re then 
      rules.build_ast_from_re
    else
      rules.build_ast in 
  Bsb_ninja_targets.output_build oc
    ~outputs:[output_mlast_simple]
    ~inputs:[input_impl]
    ~implicit_outputs:[output_mlast]
    ~rule:ast_rule;
  if has_intf_file then begin
    Bsb_ninja_targets.output_build oc
      ~outputs:[output_mliast_simple]
      ~implicit_outputs:[output_mliast]
      (* TODO: we can get rid of absloute path if we fixed the location to be
          [lib/bs], better for testing?
      *)
      ~inputs:[input_intf]
      ~rule:ast_rule
    ;
    let rule = begin match compile_target with
    | Bytecode -> rules.build_cmi_bytecode
    | Native   -> rules.build_cmi_native
    end in
    Bsb_ninja_targets.output_build oc
      ~outputs:[output_cmi]
      ~shadows:common_shadows
      ~order_only_deps:[output_d]
      ~inputs:[output_mliast_simple]
      ~rule
    ;
  end;
  Bsb_ninja_targets.output_build
    oc
    ~outputs:[output_d]
    ~inputs:(if has_intf_file then [output_mlast;output_mliast] else [output_mlast])
    ~rule:rules.build_bin_deps
    ?shadows:(if Bsb_dir_index.is_lib_dir group_dir_index then None
              else Some [{Bsb_ninja_targets.key = Bsb_build_schemas.bsb_dir_group ;
                          op =
                            Overwrite (string_of_int (group_dir_index :> int)) }])
  ;
  let shadows = if is_dev then
    { Bsb_ninja_targets.key = Bsb_ninja_global_vars.dev_includes;
      op =
        Append "$g_dev_incls $g_dpkg_incls"
    } :: common_shadows else common_shadows in
  let rule = begin match compile_target with
   | Bytecode -> rules.build_cmo_cmi_bytecode
   | Native   -> rules.build_cmx_cmi_native
   end in
  let cm_outputs, implicit_deps =
    if has_intf_file then
      []  , [output_cmi]
    else
      [output_cmi], []
  in
  Bsb_ninja_targets.output_build oc
    ~outputs:[output_cmx_or_cmo]
    ~shadows
    ~implicit_outputs:cm_outputs
    ~inputs:[output_mlast_simple]
    ~implicit_deps
    ~order_only_deps:[output_d]
    ~rule





let handle_file_group
    oc
    ~bs_suffix
    ~(rules : Bsb_ninja_rule.builtin)
    ~package_specs
    ~js_post_build_cmd
    ~root_project_dir
    ~compile_target
    (files_to_install : Hash_set_string.t)
    (namespace  : string option)
    (group: Bsb_file_groups.file_group )
  : unit =

  handle_generators oc group rules.customs ;
  let installable =
    match group.public with
    | Export_all -> fun _ -> true
    | Export_none -> fun _ -> false
    | Export_set set -> 
      fun module_name ->
      Set_string.mem set module_name in
  Map_string.iter group.sources   (fun  module_name module_info   ->
      if installable module_name then 
        Hash_set_string.add files_to_install 
          module_info.name_sans_extension;
      emit_module_build  rules
        package_specs
        group.dir_index
        oc
        ~bs_suffix
        ~root_project_dir
        ~compile_target
        js_post_build_cmd
        namespace module_info
    )


let link oc
  ~entries
  ~backend
  ~file_groups
  ~namespace
  ~dependency_info:(dependency_info : Bsb_dependency_info.t)
  ~rules:(rules : Bsb_ninja_rule.builtin)
  ~root_project_dir =
  let buildable_entries = List.filter (fun entry -> match (backend, entry) with
  | Bsb_config_types.Js, Bsb_config_types.JsTarget _
  | Bsb_config_types.Native, Bsb_config_types.NativeTarget _
  | Bsb_config_types.Bytecode, Bsb_config_types.BytecodeTarget _ -> true
  | _, _ -> false
  ) entries in
  List.iter (fun entry ->
    (* TODO: Maybe we should symlink this instead of outputting the the exe in the root directory

        Ben — August 6th 2019 (It's a special day)
     *)
    let output, rule_name, library_file_name, suffix_cmo_or_cmx, main_module_name =
      begin match entry with
      | Bsb_config_types.JsTarget _ -> assert false
      | Bsb_config_types.BytecodeTarget main_module_name ->
        root_project_dir // (Ext_string.lowercase_ascii main_module_name) ^ ".exe",
        rules.linking_bytecode,
        Literals.library_file ^ Literals.suffix_cma,
        Literals.suffix_cmo,
        main_module_name
      | Bsb_config_types.NativeTarget main_module_name ->
        root_project_dir // (Ext_string.lowercase_ascii main_module_name) ^ ".exe",
        rules.linking_native,
        Literals.library_file ^ Literals.suffix_cmxa,
        Literals.suffix_cmx,
        main_module_name
      end in
    let (all_mlast_files, all_cmo_or_cmx_files, all_cmi_files) =
      List.fold_left (fun acc (group : Bsb_file_groups.file_group) ->
        Map_string.fold group.sources acc (fun _ (v : Bsb_db.module_info) (all_mlast_files, all_cmo_or_cmx_files, all_cmi_files) ->
          let input = v.name_sans_extension in
          let namespacedName = Ext_namespace_encode.make ?ns:namespace input in
          begin match v.info with
            | Ml
            | Ml_mli ->
              ((input ^ Literals.suffix_mlast_simple)   :: all_mlast_files,
                (namespacedName ^ suffix_cmo_or_cmx)   :: all_cmo_or_cmx_files,
                (namespacedName ^ Literals.suffix_cmi) :: all_cmi_files)
            | Mli -> 
              (all_mlast_files,
                all_cmo_or_cmx_files,
                (namespacedName ^ Literals.suffix_cmi) :: all_cmi_files)
          end
        )
      ) ([], [], []) file_groups
    in
    let shadows = [{
        Bsb_ninja_targets.key = "main_module";
        op = Bsb_ninja_targets.Overwrite main_module_name
      }] in
      let external_deps_lib =
          List.map
            (fun dep -> (Bytes.unsafe_to_string (Bytes.escaped (Bytes.unsafe_of_string dep))) // library_file_name)
            dependency_info.all_external_deps
      in
      Bsb_ninja_targets.output_build oc
        ~outputs:[output]
        ~inputs:all_mlast_files
        ~implicit_deps:(external_deps_lib @ (List.map (fun str -> Bytes.unsafe_to_string (Bytes.escaped (Bytes.unsafe_of_string str))) (all_cmi_files @ all_cmo_or_cmx_files)))
        ~shadows
        ~rule:rule_name;
  ) buildable_entries


  let pack oc ~entries ~backend ~file_groups ~namespace ~rules:(rules : Bsb_ninja_rule.builtin) =
     let output_cma_or_cmxa, rule_name, suffix_cmo_or_cmx =
       begin match backend with
       (* These cases could benefit from a better error message. *)
       | Bsb_config_types.Js       -> assert false
       | Bsb_config_types.Bytecode ->
         Literals.library_file ^ Literals.suffix_cma ,
         rules.build_cma_library ,
         Literals.suffix_cmo
       | Bsb_config_types.Native   ->
         Literals.library_file ^ Literals.suffix_cmxa,
         rules.build_cmxa_library,
         Literals.suffix_cmx
     end in
     let all_cmo_or_cmx_files, all_cmi_files =
       List.fold_left (fun acc (group : Bsb_file_groups.file_group) ->
         Map_string.fold group.Bsb_file_groups.sources acc (fun _ (v : Bsb_db.module_info) (all_cmo_or_cmx_files, all_cmi_files) ->
             let input = v.name_sans_extension in
             let name = Ext_namespace_encode.make ?ns:namespace input in
              begin match v.info with
                | Ml
                | Ml_mli ->
                  ((name ^ suffix_cmo_or_cmx)     :: all_cmo_or_cmx_files,
                  (name ^ Literals.suffix_cmi)   :: all_cmi_files)
                 | Mli -> 
                  (all_cmo_or_cmx_files,
                  (name ^ Literals.suffix_cmi)   :: all_cmi_files)
              end
         )
       )
       ([], [])
       file_groups in
     (* In the case that a library is just an interface file, we don't do anything *)
     if List.length all_cmo_or_cmx_files > 0 then begin
       Bsb_ninja_targets.output_build oc
         ~outputs:[output_cma_or_cmxa]
         ~inputs:all_cmo_or_cmx_files
         ~implicit_deps:all_cmi_files
         ~rule:rule_name;
     end

let handle_file_groups
    oc
    ~package_specs
    ~bs_suffix
    ~js_post_build_cmd
    ~files_to_install
    ~rules
    ~toplevel
    ~compile_target
    ~backend
    ~dependency_info
    ~root_project_dir
    ~config:(config: Bsb_config_types.t)
    (file_groups  :  Bsb_file_groups.file_groups)
    namespace   =
  Ext_list.iter file_groups
    (handle_file_group
       oc
       ~bs_suffix
       ~package_specs
       ~rules
       ~js_post_build_cmd
       ~compile_target
       ~root_project_dir
       files_to_install
       namespace
    );
  if toplevel then
    link oc
      ~entries:config.entries
      ~backend
      ~file_groups
      ~namespace
      ~dependency_info
      ~rules
      ~root_project_dir
  else
    pack oc ~entries:config.entries ~backend ~file_groups ~namespace ~rules


#end
