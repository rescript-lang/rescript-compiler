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




(* let dash_i = "-I" *)



let get_bsc_flags 
    (bsc_flags : string list)
  : string =       
  String.concat Ext_string.single_space bsc_flags



let emit_bsc_lib_includes 
    (bs_dependencies : Bsb_config_types.dependencies)
  (source_dirs : string list) 
  (external_includes) 
  (namespace : _ option): string = 
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

  (Bsb_build_util.include_dirs 
     (all_includes 
        (if namespace = None then source_dirs 
         else Filename.current_dir_name :: source_dirs
         (*working dir is [lib/bs] we include this path to have namespace mapping*)
        )))


let output_static_resources 
    (static_resources : string list) 
    copy_rule 
    oc
  = 
  Ext_list.iter static_resources (fun output -> 
      Bsb_ninja_targets.output_build
        oc
        ~outputs:[output]
        ~inputs:[Bsb_config.proj_rel output]
        ~rule:copy_rule);
  if static_resources <> [] then
    Bsb_ninja_targets.phony
      oc
      ~order_only_deps:static_resources 
      ~inputs:[]
      ~output:Literals.build_ninja         


let output_ninja_and_namespace_map
    ~per_proj_dir 
    ~toplevel           
    ({
      package_name;
      external_includes;
      bsc_flags ; 
      pp_file;
      ppx_files ;

      bs_dependencies;
      bs_dev_dependencies;
      refmt;
      js_post_build_cmd;
      package_specs;
      file_groups = { files = bs_file_groups};
      files_to_install;
      built_in_dependency;
      reason_react_jsx;
      generators ;
      namespace ; 
      warning;
      gentype_config; 

    } : Bsb_config_types.t) : unit 
  =
  let lib_artifacts_dir = Bsb_config.lib_bs in
  let cwd_lib_bs = per_proj_dir // lib_artifacts_dir in 
  let ppx_flags = Bsb_build_util.ppx_flags ppx_files in
  let oc = open_out_bin (cwd_lib_bs // Literals.build_ninja) in          
  let warnings = Bsb_warning.to_bsb_string ~toplevel warning in
  let bsc_flags = (get_bsc_flags bsc_flags) in 
  let bsc_path = (Ext_filename.maybe_quote Bsb_global_paths.vendor_bsc) in      
  let bs_dep = (Ext_filename.maybe_quote Bsb_global_paths.vendor_bsdep) in 
  let dpkg_incls  =  (Bsb_build_util.include_dirs_by
                        bs_dev_dependencies
                        (fun x -> x.package_install_path)) in 
  let () = 
    Ext_option.iter pp_file (fun flag ->
        Bsb_ninja_targets.output_kv Bsb_ninja_global_vars.pp_flags
          (Bsb_build_util.pp_flag flag) oc 
      );
    Ext_option.iter gentype_config (fun x -> 
        (* resolved earlier *)
        Bsb_ninja_targets.output_kv Bsb_ninja_global_vars.gentypeconfig
          ("-bs-gentype " ^ x.path) oc
      );    
    Bsb_ninja_targets.output_kv      
      Bsb_ninja_global_vars.src_root_dir per_proj_dir                 
      oc 
  in          
  let bs_groups : Bsb_db.t = {lib = Map_string.empty; dev = Map_string.empty} in
  let source_dirs : string list Bsb_db.cat = {lib = []; dev = []} in
  let static_resources =
    Ext_list.fold_left 
      bs_file_groups 
      [] (
      fun 
        (acc_resources : string list) 
        {sources; dir; resources; dev_index} 
        ->
          if dev_index then begin
            bs_groups.dev <- Bsb_db_util.merge bs_groups.dev sources ;
            source_dirs.dev <- dir :: source_dirs.dev;  
          end else begin 
            bs_groups.lib <- Bsb_db_util.merge bs_groups.lib sources ;
            source_dirs.lib <- dir :: source_dirs.lib
          end;
          Ext_list.map_append resources  acc_resources (fun x -> dir//x) 
    ) in
  let lib = bs_groups.lib in 
  let dev = bs_groups.dev in 
  Bsb_db_util.sanity_check lib;
  Bsb_db_util.sanity_check dev;
  Map_string.iter dev 
    (fun k a -> 
       if Map_string.mem lib k  then 
         raise (Bsb_db_util.conflict_module_info k a (Map_string.find_exn lib k))
    ) ;
  if source_dirs.dev <> [] then
    Bsb_ninja_targets.output_kv 
      Bsb_ninja_global_vars.g_dev_incls
      (Bsb_build_util.include_dirs source_dirs.dev) oc
  ;
  let digest = Bsb_db_encode.write_build_cache ~dir:cwd_lib_bs bs_groups in
  let lib_incls = emit_bsc_lib_includes bs_dependencies source_dirs.lib external_includes namespace in
  let rules : Bsb_ninja_rule.builtin = 
      Bsb_ninja_rule.make_custom_rules 
      ~refmt
      ~has_gentype:(gentype_config <> None)
      ~has_postbuild:js_post_build_cmd 
      ~has_pp:(pp_file <> None)
      ~has_builtin:(built_in_dependency <> None)
      ~reason_react_jsx
      ~package_specs
      ~namespace
      ~digest
      ~package_name
      ~bsc:bsc_path
      ~warnings
      ~bs_dep
      ~ppx_flags
      ~bsc_flags
      ~dpkg_incls
      ~lib_incls
      generators in   

  output_static_resources static_resources rules.copy_resources oc ;
  (** Generate build statement for each file *)        
  Ext_list.iter bs_file_groups 
    (fun files_per_dir ->
       Bsb_ninja_file_groups.handle_files_per_dir oc  
         ~rules
         ~package_specs 
         ~files_to_install    
         ~namespace files_per_dir)
  ;

  Ext_option.iter  namespace (fun ns -> 
      let namespace_dir =     
        per_proj_dir // lib_artifacts_dir  in
      Bsb_namespace_map_gen.output 
        ~dir:namespace_dir ns
        bs_file_groups; 
      Bsb_ninja_targets.output_build oc 
        ~outputs:[ns ^ Literals.suffix_cmi]
        ~inputs:[ns ^ Literals.suffix_mlmap]
        ~rule:rules.build_package
    );
  close_out oc
