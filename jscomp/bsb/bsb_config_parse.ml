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

let parse_entries (field : Ext_json_types.t array) =
  Ext_array.to_list_map (function
      | Ext_json_types.Obj {map} ->
        (* kind defaults to bytecode *)
        let kind = ref "js" in
        let main = ref None in
        let _ = map
                |? (Bsb_build_schemas.kind, `Str (fun x -> kind := x))
                |? (Bsb_build_schemas.main, `Str (fun x -> main := Some x))
        in
        let path = begin match !main with
          (* This is technically optional when compiling to js *)
          | None when !kind = Literals.js ->
            "Index"
          | None -> 
            failwith "Missing field 'main'. That field is required its value needs to be the main module for the target"
          | Some path -> path
        end in
        if !kind = Literals.native then
          Some (Bsb_config_types.NativeTarget path)
        else if !kind = Literals.bytecode then
          Some (Bsb_config_types.BytecodeTarget path)
        else if !kind = Literals.js then
          Some (Bsb_config_types.JsTarget path)
        else
          failwith "Missing field 'kind'. That field is required and its value be 'js', 'native' or 'bytecode'"
      | _ -> failwith "Unrecognized object inside array 'entries' field.") 
    field



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

  let g_react : Bsb_config_types.g_react option ref = ref None in 
  let config_json = cwd // Literals.bsconfig_json in
  let g_re_flag = ref Bsb_default.g_re_flag in
  let bs_external_includes = ref [] in 
  (** we should not resolve it too early,
      since it is external configuration, no {!Bsb_build_util.convert_and_resolve_path}
  *)
  let bsc_flags = ref Bsb_default.bsc_flags in  
  let ppx_files : string list ref = ref [] in 
  let ppx_checked_files : string list ref = ref [] in 
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
  let entries = ref Bsb_default.main_entries in
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
        (fst (Bsb_build_util.resolve_bsb_magic_file 
          ~cwd ~desc:Bsb_build_schemas.refmt str))
      | Some config  -> 
        Bsb_exception.config_error config "expect version 2 or 3"
      | None ->
        Refmt_none        
    in 
    let gentype_config : Bsb_config_types.gentype_config option  = 
      match String_map.find_opt map Bsb_build_schemas.gentypeconfig with 
      | None -> None
      | Some (Obj {map = obj}) -> 
        Some { path = 
          match String_map.find_opt obj Bsb_build_schemas.path with
          | None -> 
            fst @@ Bsb_build_util.resolve_bsb_magic_file
            ~cwd ~desc:"gentype.exe"
            "gentype/gentype.exe"
          | Some (Str {str}) ->  
            fst @@ Bsb_build_util.resolve_bsb_magic_file
            ~cwd ~desc:"gentype.exe" str 
          | Some config -> 
            Bsb_exception.config_error config
              "path expect to be a string"
        }
        
      | Some config -> 
        Bsb_exception.config_error 
          config "gentypeconfig expect an object"
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
          Some (fst @@ Bsb_build_util.resolve_bsb_magic_file ~cwd ~desc:Bsb_build_schemas.pp_flags p)
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
              g_react := Some Jsx_v2
            | "3" -> 
              g_react := Some Jsx_v3
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
            js_post_build_cmd := Some (fst @@ Bsb_build_util.resolve_bsb_magic_file ~cwd ~desc:Bsb_build_schemas.js_post_build s)

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
    |? (Bsb_build_schemas.g_ppx_flag, `Arr (fun s -> 
        let args = get_list_string s in 
        let a,b = Ext_list.map_split_opt  args (fun p ->
            if p = "" then failwith "invalid ppx, empty string found"
            else 
              let file, checked = 
                Bsb_build_util.resolve_bsb_magic_file ~cwd ~desc:Bsb_build_schemas.g_ppx_flag p 
              in 
              let some_file = Some file in 
              some_file, if checked then some_file else None
          ) in 
        ppx_files := a ;  
        ppx_checked_files := b    
      ))

    |? (Bsb_build_schemas.cut_generators, `Bool (fun b -> cut_generators := b))
    |? (Bsb_build_schemas.generators, `Arr (fun s ->
        generators :=
          Ext_array.fold_left s String_map.empty (fun acc json -> 
              match json with 
              | Obj {map = m ; loc}  -> 
                begin match String_map.find_opt  m Bsb_build_schemas.name,
                            String_map.find_opt  m Bsb_build_schemas.command with 
                | Some (Str {str = name}), Some ( Str {str = command}) -> 
                  String_map.add acc name command 
                | _, _ -> 
                  Bsb_exception.errorf ~loc {| generators exepect format like { "name" : "cppo",  "command"  : "cppo $in -o $out"} |}
                end
              | _ -> acc ) ))
    |? (Bsb_build_schemas.g_re_flag, `Arr (fun s -> g_re_flag := get_list_string s))
    |? (Bsb_build_schemas.entries, `Arr (fun s -> entries := parse_entries s))
    |> ignore ;
    begin match String_map.find_opt map Bsb_build_schemas.sources with 
      | Some x -> 
        let ignored_dirs : String_set.t =   
          match String_map.find_opt map Bsb_build_schemas.ignored_dirs with 
          | None -> String_set.empty
          | Some (Arr {content}) -> 
            String_set.of_list (Bsb_build_util.get_list_string content)
          | Some config -> 
            Bsb_exception.config_error config "expect an array of string"
        in
        let res = Bsb_parse_sources.scan
            ~ignored_dirs
            ~not_dev
            ~root: cwd
            ~cut_generators: !cut_generators
            ~clean_staled_bs_js:bs_suffix
            ~namespace
            x in 
        if generate_watch_metadata then
          Bsb_watcher_gen.generate_sourcedirs_meta cwd res ;     
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

        {
          gentype_config;
          bs_suffix ;
          package_name ;
          namespace ;    
          warning = warning;
          external_includes = !bs_external_includes;
          bsc_flags = !bsc_flags ;
          ppx_files = !ppx_files ;
          ppx_checked_files = !ppx_checked_files;
          pp_file = pp_flags ;          
          bs_dependencies = !bs_dependencies;
          bs_dev_dependencies = !bs_dev_dependencies;
          refmt;
          g_re_flag = !g_re_flag ;
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
          g_react = !g_react ;  
          entries = !entries;
          generators = !generators ; 
          cut_generators = !cut_generators
        }
      | None -> failwith "no sources specified, please checkout the schema for more details"
    end
  | _ -> failwith "bsconfig.json expect a json object {}"
