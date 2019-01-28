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

let config_file_bak = "bsconfig.json.bak"
let get_list_string = Bsb_build_util.get_list_string
let (//) = Ext_path.combine
let current_package : Bsb_pkg_types.t = Global Bs_version.package_name
let resolve_package cwd  package_name = 
  let x =  Bsb_pkg.resolve_bs_package ~cwd package_name  in
  {
    Bsb_config_types.package_name ;
    package_install_path = x // Bsb_config.lib_ocaml
  }


(* Key is the path *)
let (|?)  m (key, cb) =
  m  |> Ext_json.test key cb

let parse_entries name (field : Ext_json_types.t array) =
  Ext_array.to_list_map (function
      | Ext_json_types.Obj {map} as entry ->
        let backend = ref [] in
        let main = ref None in
        let output_name = ref None in
        let kind = ref Bsb_config_types.Library in
        
        let _ = map
                |? (Bsb_build_schemas.backend, `Str (fun x -> backend := [x]))
                |? (Bsb_build_schemas.backend, `Arr (fun s -> backend := Bsb_build_util.get_list_string s))
                |? (Bsb_build_schemas.main_module, `Str (fun x -> main := Some x))
                |? (Bsb_build_schemas.output_name, `Str (fun x -> output_name := Some x))
                  (* Only accept ppx for now, until I can figure out how we can let the user link in specific entries from their project. 
                     If a project has 2 bytecode entries which are libraries, right now they'll conflict because we create the same lib.cma for both.
                     And the user has no way to specify sub-dependencies.
                  *)
                |? (Bsb_build_schemas.type_, `Str (fun x -> 
                if x = Literals.ppx then
                    kind := Bsb_config_types.Ppx
                else 
                  Bsb_exception.config_error entry "Field 'kind' not recognized. Should be empty or 'ppx'" ))
                |? (Bsb_build_schemas.kind, `Str (fun x -> 
                  if (x = Literals.native 
                       || x = Literals.bytecode 
                       || x = Literals.js) then begin
                    Bsb_log.warn "@{<warn>Warning@} package %s: 'kind' field in 'entries' is deprecated and will be removed in the next release. Please use 'backend'.@." name;
                    backend := [x];
                  end else 
                    Bsb_exception.config_error entry "Field 'kind' not recognized. Please use 'backend'." 
                  ))
        in
        
        let backend = if !backend = [] then ["js"] else !backend in
          
        let main_module_name = begin match !main with
          (* This is technically optional when compiling to js *)
          | None when backend = [Literals.js] -> "Index"
          | None -> 
            Bsb_exception.config_error entry "Missing field 'main-module'. That field is required its value needs to be the main module for the target"
          | Some main_module_name -> main_module_name
        end in
        
        if !kind = Bsb_config_types.Ppx && backend <> [Literals.native] then
          Bsb_exception.config_error entry "Ppx can only be compiled to native for now. Set `backend` to `native`.";
        
        let backend = List.map Bsb_config_types.(function
          | "js" -> 
            if !kind = Ppx then
              Bsb_exception.config_error entry "Ppx can't be compiled to JS for now. Please set the backend to `native`.";
              
            JsTarget
          | "bytecode" -> BytecodeTarget
          | "native" -> NativeTarget
          | _ -> Bsb_exception.config_error entry "Missing field 'backend'. That field is required and its value be 'js', 'native' or 'bytecode'"
        ) backend in
        
        Some {Bsb_config_types.kind = !kind; main_module_name; output_name = !output_name; backend}  
      | entry -> Bsb_exception.config_error entry "Unrecognized object inside array 'entries' field.") 
    field

#if BS_NATIVE then
let entries_from_bsconfig () = 
  let json = Ext_json_parse.parse_json_from_file Literals.bsconfig_json in
  match json with
    | Obj {map} ->
      let entries = ref Bsb_default.main_entries in
      let name = ref "[unnamed]" in
      map 
        |? (Bsb_build_schemas.name, `Str (fun s -> name := s))
        |? (Bsb_build_schemas.entries, `Arr (fun s -> entries := parse_entries !name s)) |> ignore;
      !entries
    | _ -> assert false

(* Shawdows the previous resolve_package *)
let resolve_package cwd  package_name = 
  let x =  Bsb_pkg.resolve_bs_package ~cwd package_name  in
  {
    Bsb_config_types.package_name ;
    package_install_path = (Bsb_build_util.get_build_artifacts_location x) // Bsb_config.lib_ocaml
  }

let parse_allowed_build_kinds map =
  let open Ext_json_types in
  match String_map.find_opt map Bsb_build_schemas.allowed_build_kinds with
  | Some (Arr {loc_start; content = s }) ->
    List.map (fun (s : string) ->
      match s with 
      | "js"       -> Bsb_config_types.Js
      | "native"   -> Bsb_config_types.Native
      | "bytecode" -> Bsb_config_types.Bytecode
      | str -> Bsb_exception.errorf ~loc:loc_start "'allowed-build-kinds' field expects one of, or an array of: 'js', 'bytecode' or 'native'. Found '%s'" str
    ) (Bsb_build_util.get_list_string s) 
  | Some (Str {str = "js"} )       -> [Bsb_config_types.Js]
  | Some (Str {str = "native"} )   -> [Bsb_config_types.Native]
  | Some (Str {str = "bytecode"} ) -> [Bsb_config_types.Bytecode]
  | Some (Str {str; loc} ) -> Bsb_exception.errorf ~loc:loc "'allowed-build-kinds' field expects one of, or an array of: 'js', 'bytecode' or 'native'. Found '%s'" str
  | Some x -> Bsb_exception.config_error x "'allowed-build-kinds' field expects one of, or an array of: 'js', 'bytecode' or 'native'"
  | None -> Bsb_default.allowed_build_kinds

let replace_sep_if_necessary path =
  if Ext_sys.is_windows_or_cygwin then
    let cp = Bytes.of_string path in
    for i = 0 to (Bytes.length cp - 1) do
      if Bytes.get cp i = '/' then
        Bytes.set cp i '\\';
    done;
    Bytes.to_string cp
  else path

#end

let package_specs_from_bsconfig () = 
  let json = Ext_json_parse.parse_json_from_file Literals.bsconfig_json in
  begin match json with
    | Obj {map} ->
      begin 
        match String_map.find_opt map  Bsb_build_schemas.package_specs with 
        | Some x ->
          Bsb_package_specs.from_json x
        | None -> 
          Bsb_package_specs.default_package_specs
      end
    | _ -> assert false
  end





(*TODO: it is a little mess that [cwd] and [project dir] are shared*)


let extract_package_name_and_namespace
    loc (map : Ext_json_types.t String_map.t) : string * string option =   
  let package_name = 
    match String_map.find_opt map Bsb_build_schemas.name with 

    | Some (Str { str = "_" })
      -> 
      Bsb_exception.errorf ~loc "_ is a reserved package name"
    | Some (Str {str = name }) -> 
      name 
    | Some _ | None -> 
      Bsb_exception.errorf ~loc
        "field name  as string is required"
  in 
  let namespace = 
    match String_map.find_opt map Bsb_build_schemas.namespace with 
    | None 
    | Some (False _) 
      -> None 
    | Some (True _) -> 
      Some (Ext_namespace.namespace_of_package_name package_name)
    | Some (Str {str}) -> 
      (*TODO : check the validity of namespace *)
      Some (Ext_namespace.namespace_of_package_name str)        
    | Some x ->
      Bsb_exception.errorf ~loc:(Ext_json.loc_of x)
      "namespace field expects string or boolean"
  in 
  package_name, namespace
(** ATT: make sure such function is re-entrant. 
    With a given [cwd] it works anywhere*)
let interpret_json 
    ~override_package_specs
    ~bsc_dir 
    ~generate_watch_metadata
    ~not_dev 
    cwd  

  : Bsb_config_types.t =

  let reason_react_jsx = ref None in 
  let config_json = cwd // Literals.bsconfig_json in
  let refmt_flags = ref Bsb_default.refmt_flags in
  let bs_external_includes = ref [] in 
  (** we should not resolve it too early,
      since it is external configuration, no {!Bsb_build_util.convert_and_resolve_path}
  *)
  let bsc_flags = ref Bsb_default.bsc_flags in  
  let ppx_flags = ref [] in 

#if BS_NATIVE then
  let build_script = ref None in
  let static_libraries = ref [] in
  let c_linker_flags = ref [] in
  let ocamlfind_dependencies = ref [] in
  let ocaml_flags = ref Bsb_default.ocaml_flags in
  let ocaml_linker_flags = ref Bsb_default.ocaml_linker_flags in
  let ocaml_dependencies= ref Bsb_default.ocaml_dependencies in 
#end
  let js_post_build_cmd = ref None in 
  let built_in_package = ref None in
  let generate_merlin = ref true in 
  let generators = ref String_map.empty in 

  (* When we plan to add more deps here,
     Make sure check it is consistent that for nested deps, we have a 
     quck check by just re-parsing deps 
     Make sure it works with [-make-world] [-clean-world]
  *)
  let bs_dependencies = ref [] in 
  let bs_dev_dependencies = ref [] in
  (* Setting ninja is a bit complex
     1. if [build.ninja] does use [ninja] we need set a variable
     2. we need store it so that we can call ninja correctly
  *)
  let cut_generators = ref false in 
  let config_json_chan = open_in_bin config_json  in
  let global_data = 
    Ext_json_parse.parse_json_from_chan 
      config_json config_json_chan  in
  match global_data with
  | Obj { map ; loc } ->
    let package_name, namespace = 
      extract_package_name_and_namespace loc  map in 
    let refmt =   
      match String_map.find_opt map Bsb_build_schemas.refmt with 
      | Some (Flo {flo} as config) -> 
        begin match flo with 
        | "3" -> Bsb_config_types.Refmt_v3
        | _ -> Bsb_exception.config_error config "expect version 3 only"
        end
      | Some (Str {str}) 
        -> 
        Refmt_custom
        (Bsb_build_util.resolve_bsb_magic_file 
          ~cwd ~desc:Bsb_build_schemas.refmt str)
      | Some config  -> 
        Bsb_exception.config_error config "expect version 2 or 3"
      | None ->
        Refmt_none
        

    in 
    let bs_suffix = 
          match String_map.find_opt map Bsb_build_schemas.suffix with 
          | None -> false  
          | Some (Str {str} as config ) -> 
            if str = Literals.suffix_js then false 
            else if str = Literals.suffix_bs_js then true
            else Bsb_exception.config_error config 
              "expect .bs.js or .js string here"
          | Some config -> 
            Bsb_exception.config_error config 
              "expect .bs.js or .js string here"
    in   
    (* The default situation is empty *)
    (match String_map.find_opt map Bsb_build_schemas.use_stdlib with      
     | Some (False _) -> 
       ()
     | None 
     | Some _ ->
        begin
          let stdlib_path = 
              Bsb_pkg.resolve_bs_package ~cwd current_package in 
          let json_spec = 
              Ext_json_parse.parse_json_from_file 
              (Filename.concat stdlib_path Literals.package_json) in 
          match json_spec with 
          | Obj {map}  -> 
            (match String_map.find_exn map Bsb_build_schemas.version with 
            | Str {str } -> 
              if str <> Bs_version.version then 
              (
                Format.fprintf Format.err_formatter
                "@{<error>bs-platform version mismatch@} Running bsb @{<info>%s@} (%s) vs vendored @{<info>%s@} (%s)@."
                    Bs_version.version
                    (Filename.dirname (Filename.dirname Sys.executable_name))
                    str
                    stdlib_path 
                ;
              exit 2)
                
            | _ -> assert false);
            built_in_package := Some {
              Bsb_config_types.package_name = current_package;
              package_install_path = stdlib_path // Bsb_config.lib_ocaml;
            }
             
          | _ -> assert false 
          
        end
    ) ;
    let package_specs =     
      match String_map.find_opt map Bsb_build_schemas.package_specs with 
      | Some x ->
        Bsb_package_specs.from_json x 
      | None ->  Bsb_package_specs.default_package_specs 
    in
    let pp_flags : string option = 
      match String_map.find_opt map Bsb_build_schemas.pp_flags with 
      | Some (Str {str = p }) ->
        if p = "" then failwith "invalid pp, empty string found"
        else 
          Some (Bsb_build_util.resolve_bsb_magic_file ~cwd ~desc:Bsb_build_schemas.pp_flags p)
      | Some x ->    
        Bsb_exception.errorf ~loc:(Ext_json.loc_of x) "pp-flags expected a string"
      | None ->  
        None      
    in 
    map
    |? (Bsb_build_schemas.reason, `Obj begin fun m -> 
        match String_map.find_opt m Bsb_build_schemas.react_jsx with 
        | Some (Flo{loc; flo}) -> 
          begin match flo with 
            | "2" -> 
              reason_react_jsx := 
                Some (Filename.quote 
                        (Filename.concat bsc_dir Literals.reactjs_jsx_ppx_2_exe) )
            | "3" -> 
              Bsb_exception.errorf ~loc "JSX version 3 is deprecated, please downgrade to 1.x for version 3"
            | _ -> Bsb_exception.errorf ~loc "Unsupported jsx version %s" flo
          end        
        | Some x -> Bsb_exception.config_error x 
                      "Unexpected input (expect a version number) for jsx, note boolean is no longer allowed"
        | None -> ()
      end)

    |? (Bsb_build_schemas.generate_merlin, `Bool (fun b ->
        generate_merlin := b
      ))

    |? (Bsb_build_schemas.js_post_build, `Obj begin fun m ->
        m |? (Bsb_build_schemas.cmd , `Str (fun s -> 
            js_post_build_cmd := Some (Bsb_build_util.resolve_bsb_magic_file ~cwd ~desc:Bsb_build_schemas.js_post_build s)

          )
          )
        |> ignore
      end)

    |? (Bsb_build_schemas.bs_dependencies, `Arr (fun s -> bs_dependencies :=  Ext_list.map (Bsb_build_util.get_list_string s) (fun s -> resolve_package cwd (Bsb_pkg_types.string_as_package s))))
    |? (Bsb_build_schemas.bs_dev_dependencies,
        `Arr (fun s ->
            if not  not_dev then 
              bs_dev_dependencies
              :=  Ext_list.map (Bsb_build_util.get_list_string s) (fun s -> resolve_package cwd (Bsb_pkg_types.string_as_package s)))
       )

    (* More design *)
    |? (Bsb_build_schemas.bs_external_includes, `Arr (fun s -> bs_external_includes := get_list_string s))
    |? (Bsb_build_schemas.bsc_flags, `Arr (fun s -> bsc_flags := Bsb_build_util.get_list_string_acc s !bsc_flags))
    |? (Bsb_build_schemas.ppx_flags, `Arr (fun s -> 
        ppx_flags := Ext_list.map (get_list_string s) (fun p ->
            if p = "" then failwith "invalid ppx, empty string found"
            else Bsb_build_util.resolve_bsb_magic_file ~cwd ~desc:Bsb_build_schemas.ppx_flags p
          )
      ))

    |? (Bsb_build_schemas.cut_generators, `Bool (fun b -> cut_generators := b))
    |? (Bsb_build_schemas.generators, `Arr (fun s ->
        generators :=
          Array.fold_left (fun acc json -> 
              match (json : Ext_json_types.t) with 
              | Obj {map = m ; loc}  -> 
                begin match String_map.find_opt  m Bsb_build_schemas.name,
                            String_map.find_opt  m Bsb_build_schemas.command with 
                | Some (Str {str = name}), Some ( Str {str = command}) -> 
                  String_map.add acc name command 
                | _, _ -> 
                  Bsb_exception.errorf ~loc {| generators exepect format like { "name" : "cppo",  "command"  : "cppo $in -o $out"} |}
                end
              | _ -> acc ) String_map.empty  s  ))
    |? (Bsb_build_schemas.refmt_flags, `Arr (fun s -> refmt_flags := get_list_string s))
#if BS_NATIVE then
    |? (Bsb_build_schemas.static_libraries, `Arr (fun s -> static_libraries := (List.map (fun v -> cwd // (replace_sep_if_necessary v)) (get_list_string s))))
    |? (Bsb_build_schemas.c_linker_flags, `Arr (fun s -> c_linker_flags := (List.fold_left (fun acc v -> "-ccopt" :: v :: acc) [] (List.rev (get_list_string s))) @ !c_linker_flags))
    |? (Bsb_build_schemas.build_script, `Str (fun s -> build_script := Some (replace_sep_if_necessary s)))
    |? (Bsb_build_schemas.ocamlfind_dependencies, `Arr (fun s -> ocamlfind_dependencies := get_list_string s))
    |? (Bsb_build_schemas.ocaml_flags, `Arr (fun s -> ocaml_flags := !ocaml_flags @ (get_list_string s)))
    |? (Bsb_build_schemas.ocaml_linker_flags, `Arr (fun s -> ocaml_linker_flags := !ocaml_linker_flags @ (get_list_string s)))
    |? (Bsb_build_schemas.ocaml_dependencies, `Arr (fun s -> ocaml_dependencies := (get_list_string s)))
#end
    |> ignore ;
    begin match String_map.find_opt map Bsb_build_schemas.sources with 
      | Some x -> 
        let res = Bsb_parse_sources.scan
            ~not_dev
            ~root: cwd
            ~cut_generators: !cut_generators
            ~clean_staled_bs_js:bs_suffix
            ~namespace
            x in 
#if BS_NATIVE then
        let allowed_build_kinds = parse_allowed_build_kinds map in
        let build_script = begin match !build_script with 
          | Some bs -> 
            let is_ml_or_re = Filename.check_suffix bs ".re" || Filename.check_suffix bs ".ml" in
            if Sys.file_exists (cwd // bs) && is_ml_or_re then 
              Some (cwd // bs, true)
            else begin
              Bsb_log.warn "@{<warn>Warning@} package %s: field 'build-script' has changed. It should be a path to an ml/re file instead of a shell command.@." package_name;
              Some (bs, false)
            end
          | None -> None
        end in
        
        if generate_watch_metadata then
          Bsb_watcher_gen.generate_sourcedirs_meta (Bsb_build_util.get_build_artifacts_location cwd) res ;
#else
        if generate_watch_metadata then
          Bsb_watcher_gen.generate_sourcedirs_meta cwd res ;     
#end
        begin match List.sort Ext_file_pp.interval_compare  res.intervals with
          | [] -> ()
          | queue ->
            let file_size = in_channel_length config_json_chan in
            let output_file = (cwd //config_file_bak) in 
            let oc = open_out_bin output_file in
            let () =
              Ext_file_pp.process_wholes
                queue file_size config_json_chan oc in
            close_out oc ;
            close_in config_json_chan ;
            Unix.unlink config_json;
            Unix.rename output_file config_json
        end;
        let warning : Bsb_warning.t option  = 
          match String_map.find_opt map Bsb_build_schemas.warnings with 
          | None -> None 
          | Some (Obj {map }) -> Bsb_warning.from_map map 
          | Some config -> Bsb_exception.config_error config "expect an object"
        in 

        let entries  = 
          match String_map.find_opt map Bsb_build_schemas.entries with
          | None -> Bsb_default.main_entries
          | Some (Arr {content}) -> parse_entries package_name content
          | Some config -> Bsb_exception.config_error config "`entries` should be an array"
        in 
        {
          bs_suffix ;
          package_name ;
          namespace ;    
          warning = warning;
          external_includes = !bs_external_includes;
          bsc_flags = !bsc_flags ;
          ppx_flags = !ppx_flags ;
          pp_flags = pp_flags ;          
          bs_dependencies = !bs_dependencies;
          bs_dev_dependencies = !bs_dev_dependencies;
          refmt;
          refmt_flags = !refmt_flags ;
          js_post_build_cmd =  !js_post_build_cmd ;
          package_specs = 
            (match override_package_specs with 
             | None ->  package_specs
             | Some x -> x );
          globbed_dirs = res.globbed_dirs; 
          bs_file_groups = res.files; 
          files_to_install = String_hash_set.create 96;
          built_in_dependency = !built_in_package;
          generate_merlin = !generate_merlin ;
          reason_react_jsx = !reason_react_jsx ;  
          entries = entries;
          generators = !generators ; 
#if BS_NATIVE then
          static_libraries = !static_libraries;
          c_linker_flags = !c_linker_flags;
          build_script = build_script;
          allowed_build_kinds = allowed_build_kinds;
          ocamlfind_dependencies = !ocamlfind_dependencies;
          ocaml_flags = !ocaml_flags;
          ocaml_linker_flags = !ocaml_linker_flags;
          ocaml_dependencies = !ocaml_dependencies;
#end
          cut_generators = !cut_generators
        }
      | None -> failwith "no sources specified, please checkout the schema for more details"
    end
  | _ -> failwith "bsconfig.json expect a json object {}"
