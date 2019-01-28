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

let bsdeps = ".bsdeps"

let bsppx_exe = "bsppx.exe"

let (//) = Ext_path.combine

#if BS_NATIVE then 
(* @Speed Manually walk the external dep graph. Optimize this. 
                
  We can't really serialize that tree inside a file and avoid checking
  each dep one by one because that cache could go stale if one modifies
  something inside a dep directly. We could use such a cache for when
  the compiler's not invoked with `-make-world` but it's unclear if it's 
  worth doing.
  
  One way that might make this faster is to avoid parsing all of the bsconfig
  and just grab what we need. 
  
  Another way, which might be generally beneficial, is to generate
  a faster representation of bsconfig that's more liner. If we put the 
  most used information at the top and mashalled, it would be the "fastest"
  thing to do (that I can think of off the top of my head).
  
     Ben - August 9th 2017
*)
let collect_dependency_info ~cwd ~bsc_dir ~backend ~nested ~config:(config : Bsb_config_types.t) = 
  let dependency_info : Bsb_dependency_info.t = {
    all_external_deps = [];
    all_ocamlfind_dependencies = [];
    all_ocaml_dependencies = Depend.StringSet.empty;
    all_clibs = [];
    all_c_linker_flags = [];
    all_toplevel_ppxes = String_map.empty;
  } in

  let all_ppxes = ref String_map.empty in

  Bsb_build_util.walk_all_deps cwd
    (fun {top; cwd} ->
      if top then begin
        dependency_info.all_toplevel_ppxes <- List.fold_left (fun all_toplevel_ppxes ({package_name} : Bsb_config_types.dependency) ->
          match String_map.find_opt !all_ppxes (Bsb_pkg_types.to_string package_name) with
          | None -> all_toplevel_ppxes
          | Some v -> String_map.add all_toplevel_ppxes (Bsb_pkg_types.to_string package_name) v
        ) dependency_info.all_toplevel_ppxes config.bs_dependencies;
       end else begin 
        let build_artifacts_dir = Bsb_build_util.get_build_artifacts_location cwd in
        (* @Speed We don't need to read the full config, just the right fields.
           Then again we also should cache this info so we don't have to crawl anything. *)
        let inner_config = 
          Bsb_config_parse.interpret_json 
            ~override_package_specs:(Some config.package_specs)
            ~bsc_dir
            ~generate_watch_metadata:false
            ~not_dev:true
            cwd in
        all_ppxes := List.fold_left Bsb_config_types.(fun all_ppxes e -> 
          match e with
          | {kind = Ppx} -> 
              String_map.update inner_config.Bsb_config_types.package_name (function
                | None -> Some [e]
                | Some l -> Some (e :: l)
              ) all_ppxes
          | _ -> all_ppxes
         ) !all_ppxes inner_config.Bsb_config_types.entries;
        begin match backend with 
        | Bsb_config_types.Js ->  assert false
        | Bsb_config_types.Bytecode 
          when List.mem Bsb_config_types.Bytecode Bsb_config_types.(inner_config.allowed_build_kinds) -> 
            dependency_info.all_external_deps <- (build_artifacts_dir // Bsb_config.lib_ocaml // Literals.bytecode) :: dependency_info.all_external_deps;
            dependency_info.all_c_linker_flags <- Bsb_config_types.(inner_config.c_linker_flags) @ dependency_info.all_c_linker_flags;
            dependency_info.all_clibs <- Bsb_config_types.(inner_config.static_libraries) @ dependency_info.all_clibs;
            dependency_info.all_ocamlfind_dependencies <- Bsb_config_types.(inner_config.ocamlfind_dependencies) @ dependency_info.all_ocamlfind_dependencies;
            dependency_info.all_ocaml_dependencies <- List.fold_left (fun acc v -> Depend.StringSet.add v acc) dependency_info.all_ocaml_dependencies Bsb_config_types.(inner_config.ocaml_dependencies);
            
        | Bsb_config_types.Native 
          when List.mem Bsb_config_types.Native Bsb_config_types.(inner_config.allowed_build_kinds) -> 
            dependency_info.all_external_deps <- (build_artifacts_dir // Bsb_config.lib_ocaml // Literals.native) :: dependency_info.all_external_deps;
            dependency_info.all_c_linker_flags <- Bsb_config_types.(inner_config.c_linker_flags) @ dependency_info.all_c_linker_flags;
            dependency_info.all_clibs <- Bsb_config_types.(inner_config.static_libraries) @ dependency_info.all_clibs;
            dependency_info.all_ocamlfind_dependencies <- Bsb_config_types.(inner_config.ocamlfind_dependencies) @ dependency_info.all_ocamlfind_dependencies;
            dependency_info.all_ocaml_dependencies <- List.fold_left (fun acc v -> Depend.StringSet.add v acc) dependency_info.all_ocaml_dependencies Bsb_config_types.(inner_config.ocaml_dependencies);
            
        | _ -> ()
        end;
        if List.mem backend Bsb_config_types.(inner_config.allowed_build_kinds)
           && Bsb_config_types.(inner_config.build_script) <> None then begin
          let static_libraries = Bsb_build_util.get_static_libraries 
            ~build_artifacts_dir 
            ~clibs:dependency_info.all_clibs
            ~nested
            ~package_name:inner_config.package_name 
            () in
          dependency_info.all_clibs <- static_libraries @ dependency_info.all_clibs;
        end
       end
    );
  dependency_info.all_external_deps <- List.rev dependency_info.all_external_deps;
  dependency_info.all_ocamlfind_dependencies <- List.rev dependency_info.all_ocamlfind_dependencies;
  dependency_info
#end

(** Regenerate ninja file by need based on [.bsdeps]
    return None if we dont need regenerate
    otherwise return Some info
*)
let regenerate_ninja 
    ~not_dev 
#if BS_NATIVE then
    ?dependency_info
    ~is_top_level
    ~root_project_dir
    ~build_library
    ~backend
    ~main_config:(config : Bsb_config_types.t)
    ~ocaml_dir
    ~forced cwd bsc_dir
  : bool =
#else
    ~override_package_specs
    ~generate_watch_metadata 
    ~forced cwd bsc_dir
  : _ option =
#end
#if BS_NATIVE then
  let build_artifacts_dir = Bsb_build_util.get_build_artifacts_location cwd in
  let output_deps = build_artifacts_dir // Bsb_config.lib_bs // bsdeps in
  let check_result  =
    Bsb_ninja_check.check 
      ~cwd  
      ~forced ~file:output_deps backend build_library in
#else
  let output_deps = cwd // Bsb_config.lib_bs // bsdeps in
  let check_result  =
    Bsb_ninja_check.check 
      ~cwd  
      ~forced ~file:output_deps in
#end
  let () = 
    Bsb_log.info
      "@{<info>BSB check@} build spec : %a @." Bsb_ninja_check.pp_check_result check_result in 
  begin match check_result  with 
    | Good ->
#if BS_NATIVE then
      false  (* Fast path, no need regenerate ninja *)
#else
      None  (* Fast path, no need regenerate ninja *)
#end
    | Bsb_forced 
    | Bsb_bsc_version_mismatch 
    | Bsb_file_not_exist 
    | Bsb_source_directory_changed  
#if BS_NATIVE then
    | Bsb_different_cmdline_arg
#end
    | Other _ -> 
#if BS_NATIVE then
      let nested = begin match backend with
        | Bsb_config_types.Js       -> "js"
        | Bsb_config_types.Native   -> "native"
        | Bsb_config_types.Bytecode -> "bytecode"
      end in
#end
      if check_result = Bsb_bsc_version_mismatch then begin 
        Bsb_log.info "@{<info>Different compiler version@}: clean current repo";

#if BS_NATIVE then
        Bsb_clean.clean_self bsc_dir build_artifacts_dir; 
#else
        Bsb_clean.clean_self bsc_dir cwd; 
#end
      end ; 

      (* Generate the nested folder before anything else... *)
#if BS_NATIVE then
      Bsb_build_util.mkp (build_artifacts_dir // Bsb_config.lib_bs // nested);
#else
      Bsb_build_util.mkp (cwd // Bsb_config.lib_bs); 
#end
#if BS_NATIVE = false then
      let config = 
        Bsb_config_parse.interpret_json 
          ~override_package_specs
          ~bsc_dir
          ~generate_watch_metadata
          ~not_dev
          cwd in 
#end
      begin 
        Bsb_merlin_gen.merlin_file_gen ~cwd
#if BS_NATIVE then
          ~backend
#end
          (bsc_dir // bsppx_exe) config;       
#if BS_NATIVE then  
        let dependency_info = match dependency_info with 
        | None -> 
          (* We check `is_top_level` to decide whether we need to walk the dependencies' graph or not.
             If we are building a dep, we use the top level project's entry.
             
             If we're aiming at building JS, we do NOT walk the external dep graph.
             
             If we're aiming at building Native or Bytecode, we do walk the external 
             dep graph and build a topologically sorted list of all of them. *)
          begin match backend with
          | Bsb_config_types.Js -> 
            Bsb_dependency_info.{
              all_external_deps = [];
              all_ocamlfind_dependencies = [];
              all_ocaml_dependencies = Depend.StringSet.empty;
              all_clibs = [];
              all_c_linker_flags = [];
              all_toplevel_ppxes = String_map.empty;
            }
          | Bsb_config_types.Bytecode
          | Bsb_config_types.Native ->
            if not is_top_level then 
              Bsb_dependency_info.{
                all_external_deps = [];
                all_ocamlfind_dependencies = [];
                all_ocaml_dependencies = Depend.StringSet.empty;
                all_clibs = [];
                all_c_linker_flags = [];
                all_toplevel_ppxes = String_map.empty;
              }
            else begin
              collect_dependency_info ~cwd ~backend ~nested ~bsc_dir ~config
            end
          end
        | Some all_deps -> all_deps in
#end
        Bsb_ninja_gen.output_ninja_and_namespace_map 
#if BS_NATIVE then
          ~dependency_info 
          ~ocaml_dir 
          ~root_project_dir 
          ~is_top_level 
          ~backend 
          ~main_bs_super_errors:true
          ~build_library
#end
          ~cwd ~bsc_dir ~not_dev config ;         
        (* PR2184: we still need record empty dir 
            since it may add files in the future *)  
        Bsb_ninja_check.record ~cwd ~file:output_deps 
#if BS_NATIVE then
          backend
          build_library
#end
        (Literals.bsconfig_json::config.globbed_dirs) ;

#if BS_NATIVE then
        true
#else
        Some config 
#end
      end 
  end

