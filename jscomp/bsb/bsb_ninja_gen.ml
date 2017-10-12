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

let merge_module_info_map acc sources : Bsb_build_cache.t =
  String_map.merge (fun modname k1 k2 ->
      match k1 , k2 with
      | None , None ->
        assert false
      | Some a, Some b  ->
        failwith ("Conflict files found: " ^ modname ^ " in "
                  ^ Bsb_build_cache.dir_of_module_info a ^ " and " ^ Bsb_build_cache.dir_of_module_info b
                  ^ ". File names need to be unique in a project.")
      | Some v, None  -> Some v
      | None, Some v ->  Some v
    ) acc  sources 


let bsc_exe = "bsc.exe"
let bsb_helper_exe = "bsb_helper.exe"
let dash_i = "-I"
let refmt_exe = "refmt.exe"
let dash_ppx = "-ppx"



let output_ninja_and_namespace_map
    ~cwd 
    ~bsc_dir
    ~no_dev           
    ({
      bs_suffix;
      package_name;
      external_includes;
      bsc_flags ; 
      ppx_flags;
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
    } : Bsb_config_types.t)
  =
  let custom_rules = Bsb_rule.reset generators in 
  let bsc = bsc_dir // bsc_exe in   (* The path to [bsc.exe] independent of config  *)
  let bsdep = bsc_dir // bsb_helper_exe in (* The path to [bsb_heler.exe] *)
  (* let builddir = Bsb_config.lib_bs in  *)
  let ppx_flags = Bsb_build_util.flag_concat dash_ppx ppx_flags in
  let bsc_flags =  String.concat Ext_string.single_space bsc_flags in
  let refmt_flags = String.concat Ext_string.single_space refmt_flags in
  let oc = open_out_bin (cwd // Bsb_config.lib_bs // Literals.build_ninja) in
  let bs_package_includes = 
    Bsb_build_util.flag_concat dash_i @@ Ext_list.map 
      (fun (x : Bsb_config_types.dependency) -> x.package_install_path) bs_dependencies
  in
  let bs_package_dev_includes = 
    Bsb_build_util.flag_concat dash_i @@ Ext_list.map 
      (fun (x : Bsb_config_types.dependency) -> x.package_install_path) bs_dev_dependencies
  in  

  begin
    let () =
      let bs_package_flags , namespace_flag = 
        match namespace with
        | None -> 
          Ext_string.inter2 "-bs-package-name" package_name, Ext_string.empty
        | Some s -> 
          Ext_string.inter2 "-bs-package-map" package_name ,
          Ext_string.inter2 "-ns" s  
      in  
      let bsc_flags = 
        let result = 
          Ext_string.inter2  Literals.dash_nostdlib @@
          match built_in_dependency with 
          | None -> bsc_flags   
          | Some {package_install_path} -> 
            Ext_string.inter3 dash_i (Filename.quote package_install_path) bsc_flags
        in 
        if bs_suffix then Ext_string.inter2 "-bs-suffix" result else result
      in 
      let reason_react_jsx_flag = 
        match reason_react_jsx with 
        | None -> Ext_string.empty          
        | Some  s -> 
          Ext_string.inter2 "-ppx" s 
      in 
      let warnings = Bsb_warning.opt_warning_to_string no_dev warning in

      Bsb_ninja_util.output_kvs
        [|
          Bsb_ninja_global_vars.bs_package_flags, bs_package_flags ; 
          Bsb_ninja_global_vars.src_root_dir, cwd (* TODO: need check its integrity -- allow relocate or not? *);
          Bsb_ninja_global_vars.bsc, bsc ;
          Bsb_ninja_global_vars.bsdep, bsdep;
          Bsb_ninja_global_vars.warnings, warnings;
          Bsb_ninja_global_vars.bsc_flags, bsc_flags ;
          Bsb_ninja_global_vars.ppx_flags, ppx_flags;
          Bsb_ninja_global_vars.bs_package_includes, bs_package_includes;
          Bsb_ninja_global_vars.bs_package_dev_includes, bs_package_dev_includes;  
          Bsb_ninja_global_vars.refmt, (match refmt with None -> bsc_dir // refmt_exe | Some x -> x) ;
          Bsb_ninja_global_vars.reason_react_jsx, reason_react_jsx_flag
          ; (* make it configurable in the future *)
          Bsb_ninja_global_vars.refmt_flags, refmt_flags;
          Bsb_ninja_global_vars.namespace , namespace_flag ; 

          (** TODO: could be removed by adding a flag
              [-bs-ns react]
          *)
          Bsb_build_schemas.bsb_dir_group, "0"  (*TODO: avoid name conflict in the future *)
        |] oc ;
    in
    let all_includes acc  = 
      match external_includes with 
      | [] -> acc 
      | _ ->  
        (* for external includes, if it is absolute path, leave it as is 
           for relative path './xx', we need '../.././x' since we are in 
           [lib/bs], [build] is different from merlin though
        *)
        Ext_list.map_append 
          (fun x -> if Filename.is_relative x then Bsb_config.rev_lib_bs_prefix  x else x) 
          external_includes
          acc 

    in 
    let  static_resources =
      let number_of_dev_groups = Bsb_dir_index.get_current_number_of_dev_groups () in
      if number_of_dev_groups = 0 then
        let bs_group, source_dirs,static_resources  =
          List.fold_left (fun (acc, dirs,acc_resources) ({Bsb_parse_sources.sources ; dir; resources }) ->
              merge_module_info_map  acc  sources ,  
              dir::dirs , 
              Ext_list.map_append (fun x -> dir // x ) resources  acc_resources
            ) (String_map.empty,[],[]) bs_file_groups in
        Bsb_build_cache.sanity_check bs_group;    
        Bsb_build_cache.write_build_cache 
          ~dir:(cwd // Bsb_config.lib_bs) [|bs_group|] ;
        Bsb_ninja_util.output_kv
          Bsb_build_schemas.bsc_lib_includes 
          (Bsb_build_util.flag_concat dash_i @@ 
           (all_includes 
              (if namespace = None then source_dirs else Filename.current_dir_name :: source_dirs) ))  oc ;
        static_resources
      else
        let bs_groups = Array.init  (number_of_dev_groups + 1 ) (fun i -> String_map.empty) in
        let source_dirs = Array.init (number_of_dev_groups + 1 ) (fun i -> []) in
        let static_resources =
          List.fold_left (fun acc_resources  ({Bsb_parse_sources.sources; dir; resources; dir_index})  ->
              let dir_index = (dir_index :> int) in 
              bs_groups.(dir_index) <- merge_module_info_map bs_groups.(dir_index) sources ;
              source_dirs.(dir_index) <- dir :: source_dirs.(dir_index);
              Ext_list.map_append (fun x -> dir//x) resources  resources
            ) [] bs_file_groups in
        (* Make sure [sources] does not have files in [lib] we have to check later *)

        Bsb_ninja_util.output_kv
          Bsb_build_schemas.bsc_lib_includes 
          (Bsb_build_util.flag_concat dash_i @@
           (all_includes 
              (if namespace = None then source_dirs.(0) 
               else Filename.current_dir_name::source_dirs.(0)))) oc ;
        let lib = bs_groups.((Bsb_dir_index.lib_dir_index :> int)) in               
        Bsb_build_cache.sanity_check lib;
        for i = 1 to number_of_dev_groups  do
          let c = bs_groups.(i) in
          Bsb_build_cache.sanity_check c ;
          String_map.iter (fun k _ -> if String_map.mem k lib then failwith ("conflict files found:" ^ k)) c ;
          Bsb_ninja_util.output_kv (Bsb_dir_index.(string_of_bsb_dev_include (of_int i)))
            (Bsb_build_util.flag_concat "-I" @@ source_dirs.(i)) oc
        done  ;
        Bsb_build_cache.write_build_cache 
          ~dir:(cwd // Bsb_config.lib_bs) bs_groups ;
        static_resources;
    in
    let all_info =      
      Bsb_ninja_file_groups.handle_file_groups oc       
        ~custom_rules
        ~js_post_build_cmd  ~package_specs ~files_to_install
        bs_file_groups 
        namespace
        Bsb_ninja_file_groups.zero 
    in
    let all_info =
      match namespace with 
      | None -> all_info
      | Some ns -> 
        (* let dir = 
           Bsb_parse_sources.find_first_lib_dir bs_file_groups in   *)
        let namespace_dir =     
          cwd // Bsb_config.lib_bs  in
        (* Bsb_build_util.mkp namespace_dir ;    *)
        Bsb_pkg_map_gen.output ~dir:namespace_dir ns
          bs_file_groups
        ; 
        Bsb_ninja_util.output_build oc 
          (* This is what is actually needed 
             We may fix that issue when compiling a package 
             ns.ml explicitly does not need that package
          *)
          ~output:(ns ^ Literals.suffix_cmi)
          ~input:(ns ^ Literals.suffix_mlmap)
          ~rule:Bsb_rule.build_package;
        (ns ^ Literals.suffix_cmi) :: all_info
    in
    let () =
      List.iter (fun x -> Bsb_ninja_util.output_build oc
                    ~output:x
                    ~input:(Bsb_config.proj_rel x)
                    ~rule:Bsb_rule.copy_resources) static_resources in
    Bsb_ninja_util.phony oc ~order_only_deps:(static_resources @ all_info)
      ~inputs:[]
      ~output:Literals.build_ninja ;
    close_out oc;
  end
