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

let (//) = Ext_path.combine

(* we need copy package.json into [_build] since it does affect build output
   it is a bad idea to copy package.json which requires to copy js files
*)




let bsc_exe = "bsc.exe"
let bsb_helper_exe = "bsb_helper.exe"
let dash_i = "-I"


let output_reason_config 
  (has_reason_files : bool)
  (refmt : Bsb_config_types.refmt) 
  (bsc_dir : string)
  (refmt_flags : string) 
  (oc : out_channel) : unit =   
  if has_reason_files then 
    Bsb_ninja_util.output_kvs
      [|
        Bsb_ninja_global_vars.refmt, 
          (match refmt with 
          | Refmt_none -> 
            Bsb_log.warn "@{<warning>Warning:@} refmt version missing. Please set it explicitly, since we may change the default in the future.@.";
            bsc_dir // Bsb_default.refmt_none
          | Refmt_v3 -> 
            bsc_dir // Bsb_default.refmt_v3
          | Refmt_custom x -> x );        
        Bsb_ninja_global_vars.refmt_flags, refmt_flags;
      |] oc 

let get_bsc_flags 
    (not_dev : bool)     
    (bsc_flags : string list)
  : string =       
  String.concat Ext_string.single_space 
    (if not_dev then "-bs-quiet" :: bsc_flags else bsc_flags)


let emit_bsc_lib_includes 
    (bs_dependencies : Bsb_config_types.dependencies)
  (source_dirs : string list) 
  (external_includes) 
  (namespace : _ option)
  (oc : out_channel): unit = 
  (* TODO: bsc_flags contain stdlib path which is in the latter position currently *)
  let all_includes source_dirs  = 
    source_dirs @
    Ext_list.map bs_dependencies (fun x -> x.package_install_path) @ 
    (
      (* for external includes, if it is absolute path, leave it as is 
         for relative path './xx', we need '../.././x' since we are in 
         [lib/bs], [build] is different from merlin though
      *)
      Ext_list.map
        external_includes

        (fun x -> if Filename.is_relative x then Bsb_config.rev_lib_bs_prefix  x else x) 
    )
  in 
  Bsb_ninja_util.output_kv
    Bsb_build_schemas.g_lib_incls 
    (Bsb_build_util.include_dirs 
       (all_includes 
          (if namespace = None then source_dirs 
           else Filename.current_dir_name :: source_dirs
           (*working dir is [lib/bs] we include this path to have namespace mapping*)
          )))  oc 


let output_ninja_and_namespace_map
    ~cwd 
    ~bsc_dir
    ~not_dev           
    ({
      bs_suffix;
      package_name;
      external_includes;
      bsc_flags ; 
      pp_file;
      ppx_files ;
      ppx_checked_files;
      bs_dependencies;
      bs_dev_dependencies;
      refmt;
      refmt_flags;
      js_post_build_cmd;
      package_specs;
      bs_file_groups;
      files_to_install;
      built_in_dependency;
      reason_react_jsx;
      generators ;
      namespace ; 
      warning;
      gentype_config; 
    } : Bsb_config_types.t) : unit 
  =
  let rules : Bsb_ninja_rule.builtin = 
      Bsb_ninja_rule.make_custom_rules 
      ~has_gentype:(gentype_config <> None)
      ~has_postbuild:(js_post_build_cmd <> None)
      ~has_ppx:(ppx_files <> [])
      ~has_pp:(pp_file <> None)
      ~has_builtin:(built_in_dependency <> None)
      ~reason_react_jsx
      ~bs_suffix
      generators in 
  
  
  let cwd_lib_bs = cwd // Bsb_config.lib_bs in 
  let ppx_flags = Bsb_build_util.ppx_flags ppx_files in
  let refmt_flags = String.concat Ext_string.single_space refmt_flags in
  let oc = open_out_bin (cwd_lib_bs // Literals.build_ninja) in          
  let has_reason_files = ref false in 
  let g_pkg_flg , g_ns_flg = 
    match namespace with
    | None -> 
      Ext_string.inter2 "-bs-package-name" package_name, Ext_string.empty
    | Some s -> 
      Ext_string.inter4 
        "-bs-package-name" package_name 
        "-bs-ns" s
      ,
      Ext_string.inter2 "-bs-ns" s in  
  let () = 
    Ext_option.iter pp_file (fun flag ->
        Bsb_ninja_util.output_kv Bsb_ninja_global_vars.pp_flags
          (Bsb_build_util.pp_flag flag) oc 
      );
    Ext_option.iter gentype_config (fun x -> 
        (* resolved earlier *)
        Bsb_ninja_util.output_kv Bsb_ninja_global_vars.gentypeconfig
          ("-bs-gentype " ^ x.path) oc
      );
    Ext_option.iter built_in_dependency (fun x -> 
      Bsb_ninja_util.output_kv Bsb_ninja_global_vars.g_stdlib_incl
      (Ext_filename.maybe_quote x.package_install_path) oc 
    )  
    ;  
    (*
    TODO: 
    see https://github.com/ninja-build/ninja/issues/1375
    *)
    (match ppx_checked_files with
     | first_ppx_checked_file :: _ -> 
       Bsb_ninja_util.output_kv Bsb_ninja_global_vars.ppx_checked_files 
         first_ppx_checked_file oc
     | [] -> ())
    ;

    Bsb_ninja_util.output_kvs
      [|
        Bsb_ninja_global_vars.g_pkg_flg, g_pkg_flg ; 
        Bsb_ninja_global_vars.src_root_dir, cwd (* TODO: need check its integrity -- allow relocate or not? *);
        (* The path to [bsc.exe] independent of config  *)
        Bsb_ninja_global_vars.bsc, (Ext_filename.maybe_quote (bsc_dir // bsc_exe));
        (* The path to [bsb_heler.exe] *)
        Bsb_ninja_global_vars.bsdep, (Ext_filename.maybe_quote (bsc_dir // bsb_helper_exe)) ;
        Bsb_ninja_global_vars.warnings, Bsb_warning.opt_warning_to_string not_dev warning ;
        Bsb_ninja_global_vars.bsc_flags, (get_bsc_flags not_dev  bsc_flags) ;
        Bsb_ninja_global_vars.ppx_flags, ppx_flags;

        Bsb_ninja_global_vars.g_dpkg_incls, 
        (Bsb_build_util.include_dirs_by
           bs_dev_dependencies
           (fun x -> x.package_install_path));  
        Bsb_ninja_global_vars.g_ns , g_ns_flg ; 
        Bsb_build_schemas.bsb_dir_group, "0"  (*TODO: avoid name conflict in the future *)
      |] oc 
  in        
  let  bs_groups, bsc_lib_dirs, static_resources =
    let number_of_dev_groups = Bsb_dir_index.get_current_number_of_dev_groups () in
    if number_of_dev_groups = 0 then
      let bs_group, source_dirs,static_resources  =
        Ext_list.fold_left bs_file_groups (String_map.empty,[],[]) 
          (fun (acc, dirs,acc_resources) ({sources ; dir; resources } as x)   
            ->
            Bsb_db_util.merge  acc  sources ,  
            (if Bsb_file_groups.is_empty x then dirs else  dir::dirs) , 
            ( if resources = [] then acc_resources
              else Ext_list.map_append resources acc_resources (fun x -> dir // x ) )
          )  in
      Bsb_db_util.sanity_check bs_group;
      has_reason_files := !has_reason_files || Bsb_db.has_reason_files bs_group ;     
      [|bs_group|], source_dirs, static_resources
    else
      let bs_groups = Array.init  (number_of_dev_groups + 1 ) (fun i -> String_map.empty) in
      let source_dirs = Array.init (number_of_dev_groups + 1 ) (fun i -> []) in
      let static_resources =
        Ext_list.fold_left bs_file_groups [] (fun (acc_resources : string list) {sources; dir; resources; dir_index} 
           ->
            let dir_index = (dir_index :> int) in 
            bs_groups.(dir_index) <- Bsb_db_util.merge bs_groups.(dir_index) sources ;
            source_dirs.(dir_index) <- dir :: source_dirs.(dir_index);
            Ext_list.map_append resources  acc_resources (fun x -> dir//x) 
          ) in
      let lib = bs_groups.((Bsb_dir_index.lib_dir_index :> int)) in               
      Bsb_db_util.sanity_check lib;
      has_reason_files :=  !has_reason_files || Bsb_db.has_reason_files lib ;
      for i = 1 to number_of_dev_groups  do
        let c = bs_groups.(i) in
        Bsb_db_util.sanity_check c;
        has_reason_files :=  !has_reason_files || Bsb_db.has_reason_files c ;
        String_map.iter c 
          (fun k a -> 
            if String_map.mem lib k  then 
              Bsb_db_util.conflict_module_info k a (String_map.find_exn lib k)            
            ) ;
        Bsb_ninja_util.output_kv 
          (Bsb_dir_index.(string_of_bsb_dev_include (of_int i)))
          (Bsb_build_util.include_dirs source_dirs.(i)) oc
      done  ;
      bs_groups,source_dirs.((Bsb_dir_index.lib_dir_index:>int)), static_resources
  in

  output_reason_config !has_reason_files  refmt bsc_dir refmt_flags oc;
  Bsb_db_encode.write_build_cache ~dir:cwd_lib_bs bs_groups ;
  emit_bsc_lib_includes bs_dependencies bsc_lib_dirs external_includes namespace oc;
  Ext_list.iter static_resources (fun output -> 
      Bsb_ninja_util.output_build
        oc
        ~output
        ~input:(Bsb_config.proj_rel output)
        ~rule:rules.copy_resources);
  (** Generate build statement for each file *)        
  Bsb_ninja_file_groups.handle_file_groups oc  
    ~has_checked_ppx:(ppx_checked_files <> [])
    ~bs_suffix     
    ~rules
    ~js_post_build_cmd 
    ~package_specs 
    ~files_to_install
    bs_file_groups 
    namespace
    ;
  if static_resources <> [] then
    Bsb_ninja_util.phony
      oc
      ~order_only_deps:static_resources 
      ~inputs:[]
      ~output:Literals.build_ninja ;
  Ext_option.iter  namespace (fun ns -> 
      let namespace_dir =     
        cwd // Bsb_config.lib_bs  in
      Bsb_namespace_map_gen.output 
        ~dir:namespace_dir ns
        bs_file_groups; 
      Bsb_ninja_util.output_build oc 
        ~output:(ns ^ Literals.suffix_cmi)
        ~input:(ns ^ Literals.suffix_mlmap)
        ~rule:rules.build_package
    );
  close_out oc
