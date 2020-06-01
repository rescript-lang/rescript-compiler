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
          op = OverwriteVar (Bsb_dir_index.bsc_dev_includes );          
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
    namespace
    (module_info : Bsb_db.module_info)
  =    
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
  let output_d = filename_sans_extension ^ Literals.suffix_d in
  let output_filename_sans_extension =  
      Ext_namespace_encode.make ?ns:namespace filename_sans_extension
  in 
  let output_cmi =  output_filename_sans_extension ^ Literals.suffix_cmi in
  let output_cmj =  output_filename_sans_extension ^ Literals.suffix_cmj in
  let output_js =
    Bsb_package_specs.get_list_of_output_js package_specs bs_suffix output_filename_sans_extension in 
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
    ~outputs:[output_mlast]
    ~inputs:[input_impl]
    ~rule:ast_rule;
  Bsb_ninja_targets.output_build
    oc
    ~outputs:[output_d]
    ~inputs:(if has_intf_file then [output_mlast;output_mliast] else [output_mlast] )
    ~rule:rules.build_bin_deps
    ?shadows:(if is_dev then
                Some [{Bsb_ninja_targets.key = Bsb_build_schemas.bsb_dir_group ; 
                       op = 
                         Overwrite (string_of_int (group_dir_index :> int)) }] 
              else None)
  ;  
  if has_intf_file then begin           
    Bsb_ninja_targets.output_build oc
      ~outputs:[output_mliast]
      (* TODO: we can get rid of absloute path if we fixed the location to be 
          [lib/bs], better for testing?
      *)
      ~inputs:[input_intf]
      ~rule:ast_rule
    ;
    Bsb_ninja_targets.output_build oc
      ~outputs:[output_cmi]
      ~shadows:common_shadows
      ~order_only_deps:[output_d]
      ~inputs:[output_mliast]
      ~rule:(if is_dev then rules.ml_cmi_dev else rules.ml_cmi)
    ;
  end;

  let shadows =
    match js_post_build_cmd with
    | None -> common_shadows
    | Some cmd ->
      {key = Bsb_ninja_global_vars.postbuild;
       op = Overwrite ("&& " ^ cmd ^ Ext_string.single_space ^ String.concat Ext_string.single_space output_js)} 
      :: common_shadows
  in
  let rule =
    if has_intf_file then 
      (if  is_dev then rules.ml_cmj_js_dev
       else rules.ml_cmj_js)
    else  
      (if is_dev then rules.ml_cmj_cmi_js_dev 
       else rules.ml_cmj_cmi_js
      )
  in
  Bsb_ninja_targets.output_build oc
    ~outputs:[output_cmj]
    ~shadows
    ~implicit_outputs:  
      (if has_intf_file then output_js else output_cmi::output_js )
    ~inputs:[output_mlast]
    ~implicit_deps:(if has_intf_file then [output_cmi] else [] )
    ~order_only_deps:[output_d]
    ~rule
  (* ;
  {output_cmj; output_cmi} *)






let handle_files_per_dir
    oc 
    ~bs_suffix
    ~(rules : Bsb_ninja_rule.builtin)
    ~package_specs 
    ~js_post_build_cmd  
    ~(files_to_install : Hash_set_string.t) 
    ~(namespace  : string option)
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
        js_post_build_cmd      
        namespace module_info
    )

    (* ; 
    Bsb_ninja_targets.phony
    oc ~order_only_deps:[] ~inputs:[] ~output:group.dir *)

    (* pseuduo targets per directory *)
