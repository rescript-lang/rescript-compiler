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

let (//) = Ext_filename.combine

(* we need copy package.json into [_build] since it does affect build output
   it is a bad idea to copy package.json which requires to copy js files
*)

let merge_module_info_map acc sources =
  String_map.merge (fun modname k1 k2 ->
      match k1 , k2 with
      | None , None ->
        assert false
      | Some a, Some b  ->
        failwith ("Conflict files found: " ^ modname ^ " in "
                  ^ Binary_cache.dir_of_module_info a ^ " and " ^ Binary_cache.dir_of_module_info b
                  ^ ". File names need to be unique in a project.")
      | Some v, None  -> Some v
      | None, Some v ->  Some v
    ) acc  sources

let bsc_exe = "bsc.exe"
let bsb_helper_exe = "bsb_helper.exe"
let dash_i = "-I"
let refmt_exe = "refmt.exe"
let dash_ppx = "-ppx"

let ninja_required_version = "ninja_required_version = 1.5.1 \n"


let output_ninja
    ~cwd 
    ~bsc_dir
    ~no_dev
    {
    Bsb_config_types.package_name;
    ocamllex;
    external_includes;
    bsc_flags ; 
    warnings;
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
    reason_react_jsx
    }
  =
  let () = Bsb_rule.reset () in 
  let bsc = bsc_dir // bsc_exe in   (* The path to [bsc.exe] independent of config  *)
  let bsb_helper = bsc_dir // bsb_helper_exe in (* The path to [bsb_heler.exe] *)
  (* let builddir = Bsb_config.lib_bs in  *)
  let ppx_flags = Bsb_build_util.flag_concat dash_ppx ppx_flags in
  let bsc_flags =  String.concat Ext_string.single_space bsc_flags in
  let refmt_flags = String.concat Ext_string.single_space refmt_flags in
  let oc = open_out_bin (cwd // Bsb_config.lib_bs // Literals.build_ninja) in
  begin
    let () =
      output_string oc ninja_required_version ;
      output_string oc "bs_package_flags = ";
      output_string oc ("-bs-package-name "  ^ package_name);
      output_string oc "\n";
      let bsc_flags = 
        Ext_string.inter2  Literals.dash_nostdlib @@
        match built_in_dependency with 
        | None -> bsc_flags   
        | Some {package_install_path} -> 
          Ext_string.inter3 dash_i (Filename.quote package_install_path) bsc_flags
  
      in 
      let reason_react_jsx_flag = 
        match reason_react_jsx with 
        | None -> Ext_string.empty          
        | Some  s -> 
          "-ppx " ^ s         
      in 
      Bsb_ninja.output_kvs
        [|
          "src_root_dir", cwd (* TODO: need check its integrity -- allow relocate or not? *);
          "bsc", bsc ;
          "bsb_helper", bsb_helper;
          "ocamllex", ocamllex;
          "bsc_flags", bsc_flags ;
          "ppx_flags", ppx_flags;
          "bs_package_includes", (Bsb_build_util.flag_concat dash_i @@ List.map (fun x -> x.Bsb_config_types.package_install_path) bs_dependencies);
          "bs_package_dev_includes", (Bsb_build_util.flag_concat dash_i @@ List.map (fun x -> x.Bsb_config_types.package_install_path) bs_dev_dependencies);  
          "refmt", (match refmt with None -> bsc_dir // refmt_exe | Some x -> x) ;
          "reason_react_jsx", reason_react_jsx_flag
             ; (* make it configurable in the future *)
          "refmt_flags", refmt_flags;
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
          Ext_list.map_acc acc 
          (fun x -> if Filename.is_relative x then Bsb_config.rev_lib_bs_prefix  x else x) 
          external_includes
    in 
    let  static_resources =
      let number_of_dev_groups = Bsb_build_ui.get_current_number_of_dev_groups () in
      if number_of_dev_groups = 0 then
        let bs_groups, source_dirs,static_resources  =
          List.fold_left (fun (acc, dirs,acc_resources) ({Bsb_build_ui.sources ; dir; resources }) ->
              merge_module_info_map  acc  sources ,  dir::dirs , (List.map (fun x -> dir // x ) resources) @ acc_resources
            ) (String_map.empty,[],[]) bs_file_groups in
        Binary_cache.write_build_cache (cwd // Bsb_config.lib_bs // Binary_cache.bsbuild_cache) [|bs_groups|] ;
        Bsb_ninja.output_kv
          Bsb_build_schemas.bsc_lib_includes (Bsb_build_util.flag_concat dash_i @@ 
          (all_includes source_dirs  ))  oc ;
        static_resources
      else
        let bs_groups = Array.init  (number_of_dev_groups + 1 ) (fun i -> String_map.empty) in
        let source_dirs = Array.init (number_of_dev_groups + 1 ) (fun i -> []) in
        let static_resources =
          List.fold_left (fun acc_resources  ({Bsb_build_ui.sources; dir; resources; dir_index})  ->
              bs_groups.(dir_index) <- merge_module_info_map bs_groups.(dir_index) sources ;
              source_dirs.(dir_index) <- dir :: source_dirs.(dir_index);
              (List.map (fun x -> dir//x) resources) @ resources
            ) [] bs_file_groups in
        (* Make sure [sources] does not have files in [lib] we have to check later *)
        let lib = bs_groups.(0) in
        Bsb_ninja.output_kv
          Bsb_build_schemas.bsc_lib_includes (Bsb_build_util.flag_concat dash_i @@
           (all_includes source_dirs.(0))) oc ;
        for i = 1 to number_of_dev_groups  do
          let c = bs_groups.(i) in
          String_map.iter (fun k _ -> if String_map.mem k lib then failwith ("conflict files found:" ^ k)) c ;
          Bsb_ninja.output_kv (Bsb_build_util.string_of_bsb_dev_include i)
            (Bsb_build_util.flag_concat "-I" @@ source_dirs.(i)) oc
        done  ;
        Binary_cache.write_build_cache (cwd // Bsb_config.lib_bs // Binary_cache.bsbuild_cache) bs_groups ;
        static_resources;
    in
    let all_info =
      Bsb_ninja.handle_file_groups oc
        ~js_post_build_cmd  ~package_specs ~files_to_install bs_file_groups Bsb_ninja.zero  in
    let () =
      List.iter (fun x -> Bsb_ninja.output_build oc
                    ~output:x
                    ~input:(Bsb_config.proj_rel x)
                    ~rule:Bsb_rule.copy_resources) static_resources in
    Bsb_ninja.phony oc ~order_only_deps:(static_resources @ all_info.all_config_deps)
      ~inputs:[]
      ~output:Literals.build_ninja ;
    close_out oc;
  end
