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


(* let get_list_string = Bsb_build_util.get_list_string *)
let (//) = Ext_path.combine
let current_package : Bsb_pkg_types.t = Global Bs_version.package_name
let resolve_package cwd  package_name = 
  let x =  Bsb_pkg.resolve_bs_package ~cwd package_name  in
  {
    Bsb_config_types.package_name ;
    package_install_path = x // !Bsb_global_backend.lib_ocaml_dir
  }

type json_map = Ext_json_types.t Map_string.t
(* Key is the path *)
let (|?)  m (key, cb) =
  m  |> Ext_json.test key cb


#if BS_NATIVE then  
let extract_main_entries (map :json_map) =  

  let extract_entries (field : Ext_json_types.t array) =
    Ext_array.to_list_map (function
        | Ext_json_types.Obj {map} ->
          (* kind defaults to bytecode *)
          let kind = ref "js" in
          let main = ref None in
          let _ = map
                  |? (Bsb_build_schemas.backend, `Str (fun x -> kind := x))
                  |? (Bsb_build_schemas.main_module, `Str (fun x -> main := Some x))
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
      field in
  let entries = ref Bsb_default.main_entries in
  begin match Map_string.find_opt map Bsb_build_schemas.entries with
    | Some (Arr {content = s}) -> entries := extract_entries s
    | _ -> ()
  end;
  if not !Bsb_global_backend.backend_is_set then 
    begin match !entries with
      | []
      | (Bsb_config_types.JsTarget _) :: _       -> Bsb_global_backend.set_backend Bsb_config_types.Js
      | (Bsb_config_types.NativeTarget _) :: _   -> Bsb_global_backend.set_backend Bsb_config_types.Native
      | (Bsb_config_types.BytecodeTarget _) :: _ -> Bsb_global_backend.set_backend Bsb_config_types.Bytecode
    end;
  !entries
#else 
let extract_main_entries (_ :json_map) = []  
#end


let package_specs_from_bsconfig () = 
  let json = Ext_json_parse.parse_json_from_file Literals.bsconfig_json in
  begin match json with
    | Obj {map} ->
      begin 
        match Map_string.find_opt map  Bsb_build_schemas.package_specs with 
        | Some x ->
          Bsb_package_specs.from_json x
        | None -> 
          Bsb_package_specs.default_package_specs
      end
    | _ -> assert false
  end





(*TODO: it is a little mess that [cwd] and [project dir] are shared*)


let extract_package_name_and_namespace
    (map : json_map) : string * string option =   
  let package_name = 
    match Map_string.find_opt map Bsb_build_schemas.name with 

    | Some (Str { str = "_" } as config)
      -> 
      Bsb_exception.config_error config "_ is a reserved package name"
    | Some (Str {str = name }) -> 
      name 
    | Some config -> 
      Bsb_exception.config_error config 
        "name expect a string field"  
    | None -> 
      Bsb_exception.invalid_spec
        "field name is required"
  in 
  let namespace = 
    match Map_string.find_opt map Bsb_build_schemas.namespace with 
    | None 
    | Some (False _) 
      -> None 
    | Some (True _) -> 
      Some (Ext_namespace.namespace_of_package_name package_name)
    | Some (Str {str}) -> 
      (*TODO : check the validity of namespace *)
      Some (Ext_namespace.namespace_of_package_name str)        
    | Some x ->
      Bsb_exception.config_error x 
      "namespace field expects string or boolean"
  in 
  package_name, namespace


(**
    There are two things to check:
    - the running bsb and vendoring bsb is the same
    - the running bsb need delete stale build artifacts
      (kinda check npm upgrade)
*)
let check_version_exit (map : json_map) stdlib_path =   
  match Map_string.find_exn map Bsb_build_schemas.version with 
  | Str {str } -> 
    if str <> Bs_version.version then 
      begin
        Format.fprintf Format.err_formatter
          "@{<error>bs-platform version mismatch@} Running bsb @{<info>%s@} (%s) vs vendored @{<info>%s@} (%s)@."
          Bs_version.version
          (Filename.dirname (Filename.dirname Sys.executable_name))
          str
          stdlib_path 
        ;
        exit 2
      end
  | _ -> assert false

let check_stdlib (map : json_map) cwd (*built_in_package*) =  
  match Map_string.find_opt map Bsb_build_schemas.use_stdlib with      
  | Some (False _) -> None    
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
        check_version_exit map stdlib_path;
        Some {
            Bsb_config_types.package_name = current_package;
            package_install_path = stdlib_path // !Bsb_global_backend.lib_ocaml_dir;
          }

      | _ -> assert false 

    end
let extract_bs_suffix_exn (map : json_map) =  
  match Map_string.find_opt map Bsb_build_schemas.suffix with 
  | None -> false  
  | Some (Str {str} as config ) -> 
    if str = Literals.suffix_js then false 
    else if str = Literals.suffix_bs_js then true
    else Bsb_exception.config_error config 
        "expect .bs.js or .js string here"
  | Some config -> 
    Bsb_exception.config_error config 
      "expect .bs.js or .js string here"

let extract_gentype_config (map : json_map) cwd 
  : Bsb_config_types.gentype_config option = 
  match Map_string.find_opt map Bsb_build_schemas.gentypeconfig with 
  | None -> None
  | Some (Obj {map = obj}) -> 
    Some { path = 
             match Map_string.find_opt obj Bsb_build_schemas.path with
             | None -> 
               (Bsb_build_util.resolve_bsb_magic_file
                 ~cwd ~desc:"gentype.exe"
                 "gentype/gentype.exe").path
             | Some (Str {str}) ->  
               (Bsb_build_util.resolve_bsb_magic_file
                 ~cwd ~desc:"gentype.exe" str).path 
             | Some config -> 
               Bsb_exception.config_error config
                 "path expect to be a string"
         }

  | Some config -> 
    Bsb_exception.config_error 
      config "gentypeconfig expect an object"  

let extract_refmt (map : json_map) cwd : Bsb_config_types.refmt =      
  match Map_string.find_opt map Bsb_build_schemas.refmt with 
  | Some (Flo {flo} as config) -> 
    begin match flo with 
      | "3" -> None
      | _ -> Bsb_exception.config_error config "expect version 3 only"
    end
  | Some (Str {str}) 
    -> 
    Some
      (Bsb_build_util.resolve_bsb_magic_file 
              ~cwd ~desc:Bsb_build_schemas.refmt str).path
  | Some config  -> 
    Bsb_exception.config_error config "expect version 2 or 3"
  | None ->
    None

let extract_string (map : json_map) (field : string) cb = 
  match Map_string.find_opt map field with 
  | None -> None 
  | Some (Str{str}) -> cb str 
  | Some config -> 
    Bsb_exception.config_error config (field ^ " expect a string" )
  
let extract_boolean (map : json_map) (field : string) (default : bool) : bool = 
  match Map_string.find_opt map field with 
  | None -> default 
  | Some (True _ ) -> true
  | Some (False _) -> false 
  | Some config -> 
    Bsb_exception.config_error config (field ^ " expect a boolean" )
  
let extract_reason_react_jsx (map : json_map) = 
  let default : Bsb_config_types.reason_react_jsx option ref = ref None in 
  map
  |? (Bsb_build_schemas.reason, `Obj begin fun m -> 
      match Map_string.find_opt m Bsb_build_schemas.react_jsx with 
      | Some (Flo{loc; flo}) -> 
        begin match flo with 
          | "3" -> 
            default := Some Jsx_v3
          | _ -> Bsb_exception.errorf ~loc "Unsupported jsx version %s" flo
        end        
      | Some x -> Bsb_exception.config_error x 
                    "Unexpected input (expect a version number) for jsx, note boolean is no longer allowed"
      | None -> ()
    end)
  |> ignore;
  !default

let extract_warning (map : json_map) = 
  match Map_string.find_opt map Bsb_build_schemas.warnings with 
  | None -> Bsb_warning.use_default 
  | Some (Obj {map }) -> Bsb_warning.from_map map 
  | Some config -> Bsb_exception.config_error config "expect an object"

let extract_ignored_dirs (map : json_map) =   
  match Map_string.find_opt map Bsb_build_schemas.ignored_dirs with 
  | None -> Set_string.empty
  | Some (Arr {content}) -> 
    Set_string.of_list (Bsb_build_util.get_list_string content)
  | Some config -> 
    Bsb_exception.config_error config "expect an array of string"  

let extract_generators (map : json_map) = 
  let generators = ref Map_string.empty in 
  (match Map_string.find_opt map Bsb_build_schemas.generators with 
   | None -> ()
   | Some (Arr {content = s}) -> 
     generators :=
       Ext_array.fold_left s Map_string.empty (fun acc json -> 
           match json with 
           | Obj {map = m ; loc}  -> 
             begin match Map_string.find_opt  m Bsb_build_schemas.name,
                         Map_string.find_opt  m Bsb_build_schemas.command with 
             | Some (Str {str = name}), Some ( Str {str = command}) -> 
               Map_string.add acc name command 
             | _, _ -> 
               Bsb_exception.errorf ~loc {| generators exepect format like { "name" : "cppo",  "command"  : "cppo $in -o $out"} |}
             end
           | _ -> acc )
   | Some config ->
     Bsb_exception.config_error config (Bsb_build_schemas.generators ^ " expect an array field")       
  );
  !generators
  

let extract_dependencies (map : json_map) cwd (field : string )
  : Bsb_config_types.dependencies =   
  match Map_string.find_opt map field with 
  | None -> []
  | Some (Arr ({content = s})) -> 
    Ext_list.map (Bsb_build_util.get_list_string s) (fun s -> resolve_package cwd (Bsb_pkg_types.string_as_package s))
  | Some config -> 
    Bsb_exception.config_error config 
      (field ^ " expect an array")
  
(* return an empty array if not found *)     
let extract_string_list (map : json_map) (field : string) : string list = 
  match Map_string.find_opt map field with 
  | None -> []
  | Some (Arr {content = s}) -> 
    Bsb_build_util.get_list_string s 
  | Some config ->   
    Bsb_exception.config_error config (field ^ " expect an array")

let extract_ppx 
  (map : json_map) 
  (field : string) 
  ~(cwd : string) : Bsb_config_types.ppx list =     
  match Map_string.find_opt map field with 
  | None -> []
  | Some (Arr {content }) -> 
    let resolve s = 
      if s = "" then Bsb_exception.invalid_spec "invalid ppx, empty string found"
      else 
        (Bsb_build_util.resolve_bsb_magic_file ~cwd ~desc:Bsb_build_schemas.ppx_flags s).path in 
    Ext_array.to_list_f content (fun x -> 
      match x with 
      | Str x ->    
      
        {Bsb_config_types.name = 
          resolve x.str; 
          args = []}
      | Arr {content } -> 

          let xs = Bsb_build_util.get_list_string content in 
          (match xs with 
          | [] -> Bsb_exception.config_error x " empty array is not allowed"
          | name :: args -> 
            {Bsb_config_types.name = resolve name ; args}
          )
      | config -> Bsb_exception.config_error config 
        (field ^ "expect each item to be either string or array")
    )
  | Some config -> 
    Bsb_exception.config_error config (field ^ " expect an array")



let extract_js_post_build (map : json_map) cwd : string option = 
  let js_post_build_cmd = ref None in 
  map    
  |? (Bsb_build_schemas.js_post_build, `Obj begin fun m ->
      m |? (Bsb_build_schemas.cmd , `Str (fun s -> 
          js_post_build_cmd := Some (Bsb_build_util.resolve_bsb_magic_file ~cwd ~desc:Bsb_build_schemas.js_post_build s).path

        )
        )
      |> ignore
    end)

  |> ignore ;
  !js_post_build_cmd

(** ATT: make sure such function is re-entrant. 
    With a given [cwd] it works anywhere*)
let interpret_json 
    ~toplevel_package_specs
    ~per_proj_dir:(per_proj_dir:string)

  : Bsb_config_types.t =

  (** we should not resolve it too early,
      since it is external configuration, no {!Bsb_build_util.convert_and_resolve_path}
  *)
  
  
 
  
  (* When we plan to add more deps here,
     Make sure check it is consistent that for nested deps, we have a 
     quck check by just re-parsing deps 
     Make sure it works with [-make-world] [-clean-world]
  *)
  
  (* Setting ninja is a bit complex
     1. if [build.ninja] does use [ninja] we need set a variable
     2. we need store it so that we can call ninja correctly
  *)
  match  Ext_json_parse.parse_json_from_file (per_proj_dir // Literals.bsconfig_json) with
  | Obj { map } ->
    let package_name, namespace = 
      extract_package_name_and_namespace  map in 
    let refmt = extract_refmt map per_proj_dir in 
    let gentype_config  = extract_gentype_config map per_proj_dir in  
    let bs_suffix = extract_bs_suffix_exn map in   
    (* This line has to be before any calls to Bsb_global_backend.backend, because it'll read the entries 
         array from the bsconfig and set the backend_ref to the first entry, if any. *)
    let entries = extract_main_entries map in
    (* The default situation is empty *)
    let built_in_package = check_stdlib map per_proj_dir in
    let package_specs =     
      match Map_string.find_opt map Bsb_build_schemas.package_specs with 
      | Some x ->
        Bsb_package_specs.from_json x 
      | None ->  Bsb_package_specs.default_package_specs 
    in
    let pp_flags : string option = 
      extract_string map Bsb_build_schemas.pp_flags (fun p -> 
        if p = "" then 
          Bsb_exception.invalid_spec "invalid pp, empty string found"
        else 
          Some (Bsb_build_util.resolve_bsb_magic_file ~cwd:per_proj_dir ~desc:Bsb_build_schemas.pp_flags p).path
      ) in 
    let reason_react_jsx = extract_reason_react_jsx map in 
    let bs_dependencies = extract_dependencies map per_proj_dir Bsb_build_schemas.bs_dependencies in 
    let toplevel = toplevel_package_specs = None in 
    let bs_dev_dependencies = 
      if toplevel then 
        extract_dependencies map per_proj_dir Bsb_build_schemas.bs_dev_dependencies
      else [] in 
    begin match Map_string.find_opt map Bsb_build_schemas.sources with 
      | Some sources -> 
        let cut_generators = 
          extract_boolean map Bsb_build_schemas.cut_generators false in 
        let groups = Bsb_parse_sources.scan
            ~ignored_dirs:(extract_ignored_dirs map)
            ~toplevel
            ~root: per_proj_dir
            ~cut_generators
            ~bs_suffix
            ~namespace
            sources in         
        {
          gentype_config;
          bs_suffix ;
          package_name ;
          namespace ;    
          warning = extract_warning map;
          external_includes = extract_string_list map Bsb_build_schemas.bs_external_includes;
          bsc_flags = extract_string_list map Bsb_build_schemas.bsc_flags ;
          ppx_files = extract_ppx map ~cwd:per_proj_dir Bsb_build_schemas.ppx_flags;
          pp_file = pp_flags ;          
          bs_dependencies ;
          bs_dev_dependencies ;
          (*
            reference for quoting
             {[
               let tmpfile = Filename.temp_file "ocamlpp" "" in
               let comm = Printf.sprintf "%s %s > %s"
                   pp (Filename.quote sourcefile) tmpfile
               in
             ]}
          *)          
          refmt;
          js_post_build_cmd = (extract_js_post_build map per_proj_dir);
          package_specs = 
            (match toplevel_package_specs with 
             | None ->  package_specs
             | Some x -> x );          
          file_groups = groups; 
          files_to_install = Hash_set_string.create 96;
          built_in_dependency = built_in_package;
          generate_merlin = 
            extract_boolean map Bsb_build_schemas.generate_merlin true;
          reason_react_jsx  ;  
          entries;
          generators = extract_generators map ; 
          cut_generators ;
        }
      | None -> 
          Bsb_exception.invalid_spec
            "no sources specified in bsconfig.json"
    end
  | _ -> 
    Bsb_exception.invalid_spec "bsconfig.json expect a json object {}"
