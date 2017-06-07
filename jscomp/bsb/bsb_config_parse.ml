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
let (//) = Ext_filename.combine

let resolve_package cwd  package_name = 
  let x =  Bsb_pkg.resolve_bs_package ~cwd package_name  in
  {
    Bsb_config_types.package_name ;
    package_install_path = x // Bsb_config.lib_ocaml
  }


let get_package_specs_from_array arr =  
  arr
  |> get_list_string
  |> List.fold_left (fun acc x ->
      let v =
        if Bsb_config.supported_format x    then String_set.add x acc
        else
          failwith ("Unkonwn package spec" ^ x) in
      v
    ) String_set.empty 


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

let sourcedirs_meta = ".sourcedirs"
let merlin = ".merlin"
let merlin_header = "####{BSB GENERATED: NO EDIT"
let merlin_trailer = "####BSB GENERATED: NO EDIT}"
let merlin_trailer_length = String.length merlin_trailer


let package_specs_from_bsconfig () = 
  let json = Ext_json_parse.parse_json_from_file Literals.bsconfig_json in
  begin match json with
    | Obj {map} ->
      begin 
        match String_map.find_opt Bsb_build_schemas.package_specs map with 
        | Some (Arr s ) -> 
          get_package_specs_from_array s.content
        | Some _
        | None -> Bsb_default.package_specs
      end
    | _ -> assert false
  end



(** [new_content] should start end finish with newline *)
let revise_merlin merlin new_content =
  if Sys.file_exists merlin then
    let merlin_chan = open_in_bin merlin in
    let size = in_channel_length merlin_chan in
    let s = really_input_string merlin_chan size in
    let () =  close_in merlin_chan in

    let header =  Ext_string.find s ~sub:merlin_header  in
    let tail = Ext_string.find s ~sub:merlin_trailer in
    if header < 0  && tail < 0 then (* locked region not added yet *)
      let ochan = open_out_bin merlin in
      output_string ochan s ;
      output_string ochan "\n";
      output_string ochan merlin_header;
      Buffer.output_buffer ochan new_content;
      output_string ochan merlin_trailer ;
      output_string ochan "\n";
      close_out ochan
    else if header >=0 && tail >= 0  then
      (* there is one, hit it everytime,
         should be fixed point
      *)
      let ochan = open_out_bin merlin in
      output_string ochan (String.sub s 0 header) ;
      output_string ochan merlin_header;
      Buffer.output_buffer ochan new_content;
      output_string ochan merlin_trailer ;
      output_string ochan (Ext_string.tail_from s (tail +  merlin_trailer_length));
      close_out ochan
    else failwith ("the .merlin is corrupted, locked region by bsb is not consistent ")
  else
    let ochan = open_out_bin merlin in
    output_string ochan merlin_header ;
    Buffer.output_buffer ochan new_content;
    output_string ochan merlin_trailer ;
    output_string ochan "\n";
    close_out ochan

(* ATTENTION: order matters here, need resolve global properties before
   merlin generation
*)
let merlin_flg_ppx = "\nFLG -ppx " 
let merlin_s = "\nS "
let merlin_b = "\nB "


let merlin_flg = "\nFLG "
let merlin_file_gen ~cwd
    (built_in_ppx, _reactjs_jsx_ppx)
    ({bs_file_groups = res_files ; 
      generate_merlin;
      ppx_flags;
      bs_dependencies;
      bs_dev_dependencies;
      bsc_flags; 
      built_in_dependency;
      external_includes; 
      reason_react_jsx ; 
     } : Bsb_config_types.t)
  =
  if generate_merlin then begin     
    let buffer = Buffer.create 1024 in
    ppx_flags
    |> List.iter (fun x ->
        Buffer.add_string buffer (merlin_flg_ppx ^ x )
      );
    (match reason_react_jsx with
    | Some s -> 
      begin 
        Buffer.add_string buffer (merlin_flg_ppx ^ s)
      end
    | None -> ());
    Buffer.add_string buffer (merlin_flg_ppx  ^ built_in_ppx);
    (*
    (match external_includes with 
    | [] -> ()
    | _ -> 

      Buffer.add_string buffer (merlin_flg ^ Bsb_build_util.flag_concat "-I" external_includes
      ));
    *)
    external_includes 
    |> List.iter (fun path -> 
        Buffer.add_string buffer merlin_s ;
        Buffer.add_string buffer path ;
        Buffer.add_string buffer merlin_b;
        Buffer.add_string buffer path ;
      );      
    (match built_in_dependency with
     | None -> ()
     | Some package -> 
       let path = package.package_install_path in 
       Buffer.add_string buffer (merlin_s ^ path );
       Buffer.add_string buffer (merlin_b ^ path)                      
    );

    let bsc_string_flag = 
      merlin_flg ^ 
      String.concat Ext_string.single_space 
        (Literals.dash_nostdlib::bsc_flags)  in 
    Buffer.add_string buffer bsc_string_flag ;

    bs_dependencies 
    |> List.iter (fun package ->
        let path = package.Bsb_config_types.package_install_path in
        Buffer.add_string buffer merlin_s ;
        Buffer.add_string buffer path ;
        Buffer.add_string buffer merlin_b;
        Buffer.add_string buffer path ;
      );
    bs_dev_dependencies (**TODO: shall we generate .merlin for dev packages ?*)
    |> List.iter (fun package ->
        let path = package.Bsb_config_types.package_install_path in
        Buffer.add_string buffer merlin_s ;
        Buffer.add_string buffer path ;
        Buffer.add_string buffer merlin_b;
        Buffer.add_string buffer path ;
      );

    res_files |> List.iter (fun (x : Bsb_build_ui.file_group) -> 
        Buffer.add_string buffer merlin_s;
        Buffer.add_string buffer x.dir ;
        Buffer.add_string buffer merlin_b;
        Buffer.add_string buffer (Bsb_config.lib_bs//x.dir) ;
      ) ;
    Buffer.add_string buffer "\n";
    revise_merlin (cwd // merlin) buffer 
  end



(*TODO: it is a little mess that [cwd] and [project dir] are shared*)





let generate_sourcedirs_meta cwd (res : Bsb_build_ui.t) = 
  let ochan = open_out_bin (cwd // Bsb_config.lib_bs // sourcedirs_meta) in
  res.files |> List.iter
    (fun (x : Bsb_build_ui.file_group) ->
       output_string ochan x.dir; (* to [.sourcedirs] *)
       output_string ochan "\n" ;
    ) ;
  close_out ochan


(** ATT: make sure such function is re-entrant. 
    With a given [cwd] it works anywhere*)
let interpret_json 
    ~override_package_specs
    ~bsc_dir 
    ~generate_watch_metadata
    ~no_dev 
    cwd  

  : Bsb_config_types.t =
  
  let reason_react_jsx = ref None in 
  let config_json = (cwd // Literals.bsconfig_json) in
  let ocamllex = ref Bsb_default.ocamllex in 
  let refmt = ref None in
  let refmt_flags = ref Bsb_default.refmt_flags in
  let package_name = ref None in 
  let bs_external_includes = ref [] in 
  (** we should not resolve it too early,
      since it is external configuration, no {!Bsb_build_util.convert_and_resolve_path}
  *)
  let bsc_flags = ref Bsb_default.bsc_flags in  
  let warnings = ref Bsb_default.warnings in
  let ppx_flags = ref []in 

  let js_post_build_cmd = ref None in 
  let built_in_package = ref None in
  let generate_merlin = ref true in 
  let package_specs = ref (String_set.singleton Literals.commonjs) in 
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

  let config_json_chan = open_in_bin config_json  in
  let global_data = Ext_json_parse.parse_json_from_chan config_json_chan  in
  match global_data with
  | Obj { map} ->
    (* The default situation is empty *)
    (match String_map.find_opt Bsb_build_schemas.use_stdlib map with      
     | Some (False _) -> 
       ()
     | None 
     | Some _ ->
       built_in_package := Some (resolve_package cwd Bs_version.package_name);
    ) ;
    map
    |? (Bsb_build_schemas.reason, `Obj begin fun m -> 
      match String_map.find_opt Bsb_build_schemas.react_jsx m with 
      
      | Some (False _)
      | None -> ()
      | Some (Flo{loc; flo}) -> 
        begin match flo with 
        | "1" -> 
        reason_react_jsx := 
            Some (Filename.quote (Filename.concat bsc_dir Literals.reactjs_jsx_ppx_exe) )
        | "2" -> 
          reason_react_jsx := 
            Some (Filename.quote 
              (Filename.concat bsc_dir Literals.reactjs_jsx_ppx_2_exe) )
        | _ -> Bsb_exception.failf ~loc "Unsupported jsx version %s" flo
        end
      | Some (True _) -> 
        reason_react_jsx := 
            Some (Filename.quote (Filename.concat bsc_dir Literals.reactjs_jsx_ppx_exe) 
            )
      | Some x -> Bsb_exception.failf ~loc:(Ext_json.loc_of x) 
      "Unexpected input for jsx"
      end)
    |? (Bsb_build_schemas.generate_merlin, `Bool (fun b ->
        generate_merlin := b
      ))
    |? (Bsb_build_schemas.name, `Str (fun s -> package_name := Some s))
    |? (Bsb_build_schemas.package_specs, 
        `Arr (fun s -> package_specs := get_package_specs_from_array  s ))
    |? (Bsb_build_schemas.js_post_build, `Obj begin fun m ->
        m |? (Bsb_build_schemas.cmd , `Str (fun s -> 
            js_post_build_cmd := Some (Bsb_build_util.resolve_bsb_magic_file ~cwd ~desc:Bsb_build_schemas.js_post_build s)

          )
          )
        |> ignore
      end)
    |? (Bsb_build_schemas.ocamllex, `Str (fun s -> 
        ocamllex := Bsb_build_util.resolve_bsb_magic_file ~cwd ~desc:Bsb_build_schemas.ocamllex s ))

    |? (Bsb_build_schemas.bs_dependencies, `Arr (fun s -> bs_dependencies := Bsb_build_util.get_list_string s |> List.map (resolve_package cwd)))
    |? (Bsb_build_schemas.bs_dev_dependencies,
        `Arr (fun s ->
            if not  no_dev then 
              bs_dev_dependencies
              := Bsb_build_util.get_list_string s
                 |> List.map (resolve_package cwd))
       )

    (* More design *)
    |? (Bsb_build_schemas.bs_external_includes, `Arr (fun s -> bs_external_includes := get_list_string s))
    |? (Bsb_build_schemas.bsc_flags, `Arr (fun s -> bsc_flags := Bsb_build_util.get_list_string_acc s !bsc_flags))
    |? (Bsb_build_schemas.warnings, `Str (fun s -> warnings := s))
    |? (Bsb_build_schemas.ppx_flags, `Arr (fun s -> 
        ppx_flags := s |> get_list_string |> List.map (fun p ->
            if p = "" then failwith "invalid ppx, empty string found"
            else Bsb_build_util.resolve_bsb_magic_file ~cwd ~desc:Bsb_build_schemas.ppx_flags p
          )
      ))
    |? (Bsb_build_schemas.refmt, `Str (fun s -> 
        refmt := Some (Bsb_build_util.resolve_bsb_magic_file ~cwd ~desc:Bsb_build_schemas.refmt s) ))
    |? (Bsb_build_schemas.refmt_flags, `Arr (fun s -> refmt_flags := get_list_string s))
    |? (Bsb_build_schemas.entries, `Arr (fun s -> entries := parse_entries s))
    |> ignore ;
    begin match String_map.find_opt Bsb_build_schemas.sources map with 
      | Some x -> 
        let res = Bsb_build_ui.parsing_sources {no_dev; dir_index =
            Bsb_build_ui.lib_dir_index; cwd = Filename.current_dir_name; root = cwd}  x in 
        if generate_watch_metadata then
          generate_sourcedirs_meta cwd res ;     
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

        {
          Bsb_config_types.package_name =
            (match !package_name with
             | Some name -> name
             | None ->
               failwith "Error: Package name is required. Please specify a `name` in `bsconfig.json`"
            );

          ocamllex = !ocamllex ; 
          external_includes = !bs_external_includes;
          bsc_flags = !bsc_flags ;
          warnings = !warnings;
          ppx_flags = !ppx_flags ;
          bs_dependencies = !bs_dependencies;
          bs_dev_dependencies = !bs_dev_dependencies;
          refmt = !refmt ;
          refmt_flags = !refmt_flags ;
          js_post_build_cmd =  !js_post_build_cmd ;
          package_specs = 
            (match override_package_specs with None ->  !package_specs
                                             | Some x -> x );
          globbed_dirs = res.globbed_dirs; 
          bs_file_groups = res.files; 
          files_to_install = String_hash_set.create 96;
          built_in_dependency = !built_in_package;
          generate_merlin = !generate_merlin ;
          reason_react_jsx = !reason_react_jsx ;  
          entries = !entries
        }
      | None -> failwith "no sources specified, please checkout the schema for more details"
    end
  | _ -> failwith "bsconfig.json expect a json object {}"
