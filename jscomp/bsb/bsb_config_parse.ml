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



(* Key is the path *)
let (|?)  m (key, cb) =
  m  |> Ext_json.test key cb

let (//) = Ext_filename.combine

let sourcedirs_meta = ".sourcedirs"
let merlin = ".merlin"
let merlin_header = "####{BSB GENERATED: NO EDIT"
let merlin_trailer = "####BSB GENERATED: NO EDIT}"
let merlin_trailer_length = String.length merlin_trailer


let package_specs_from_json () = 
  let json = Ext_json.parse_json_from_file Literals.bsconfig_json in
  begin match json with
    | `Obj map ->
      map
      |? (Bsb_build_schemas.package_specs,
          `Arr Bsb_default.set_package_specs_from_array)
      |> ignore ;
      Bsb_default.get_package_specs ()
    | _ -> assert false
  end
(** [new_content] should start end finish with newline *)
let revise_merlin new_content =
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
let bsppx_exe = "bsppx.exe"

let merlin_flg = "\nFLG "
let merlin_file_gen 
    (built_in_ppx, reactjs_jsx_ppx)
    ({bs_file_groups = res_files ; 
      generate_merlin;
      ppx_flags;
      bs_dependencies;
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
    if reason_react_jsx then   
      begin 
        Buffer.add_string buffer (merlin_flg_ppx ^ reactjs_jsx_ppx)
      end;
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

    res_files |> List.iter (fun (x : Bsb_build_ui.file_group) -> 
        Buffer.add_string buffer merlin_s;
        Buffer.add_string buffer x.dir ;
        Buffer.add_string buffer merlin_b;
        Buffer.add_string buffer (Bsb_config.lib_bs//x.dir) ;
      ) ;
    Buffer.add_string buffer "\n";
    revise_merlin buffer 
  end



(*TODO: it is a little mess that [cwd] and [project dir] are shared*)





let generate_sourcedirs_meta (res : Bsb_build_ui.t) = 
  let builddir = Bsb_config.lib_bs in 
  let ochan = open_out_bin (builddir // sourcedirs_meta) in
  res.files |> List.iter
    (fun (x : Bsb_build_ui.file_group) ->
       output_string ochan x.dir; (* to [.sourcedirs] *)
       output_string ochan "\n" ;
    ) ;
  close_out ochan



let interpret_json 
    ~override_package_specs
    ~bsc_dir 
    cwd  

  : Bsb_config_types.t =
  let builddir = Bsb_config.lib_bs in
  let () = Bsb_build_util.mkp builddir in
  let update_queue = ref [] in
  let globbed_dirs = ref [] in
  let bs_file_groups = ref [] in 
  let reason_react_jsx = ref false in 

  let config_json_chan = open_in_bin Literals.bsconfig_json in
  let global_data = Ext_json.parse_json_from_chan config_json_chan  in

  let () =
    match global_data with
    | `Obj map ->
      let use_stdlib = 
        match String_map.find_opt Bsb_build_schemas.use_stdlib map with 
        | None -> true 
        | Some `False -> false 
        | Some _ -> true  in 
      Bsb_default.set_use_stdlib ~cwd use_stdlib ;

      map
      |? (Bsb_build_schemas.reason, `Obj begin fun m -> 
          m |? (Bsb_build_schemas.react_jsx, 
                `Bool (fun b -> reason_react_jsx := b))
            |> ignore 
        end)
      |? (Bsb_build_schemas.generate_merlin, `Bool (fun b ->
          Bsb_default.set_generate_merlin b
        ))
      |?  (Bsb_build_schemas.name, `Str Bsb_default.set_package_name)
      |? (Bsb_build_schemas.package_specs, `Arr Bsb_default.set_package_specs_from_array )
      |? (Bsb_build_schemas.js_post_build, `Obj begin fun m ->
          m |? (Bsb_build_schemas.cmd , `Str (Bsb_default.set_js_post_build_cmd ~cwd)
               )
          |> ignore
        end)
      |? (Bsb_build_schemas.ocamllex, `Str (Bsb_default.set_ocamllex ~cwd))
      |? (Bsb_build_schemas.ninja, `Str (Bsb_default.set_ninja ~cwd))
      |? (Bsb_build_schemas.bs_dependencies, `Arr (Bsb_default.set_bs_dependencies ~cwd))
      (* More design *)
      |? (Bsb_build_schemas.bs_external_includes, `Arr Bsb_default.set_bs_external_includes)
      |? (Bsb_build_schemas.bsc_flags, `Arr Bsb_default.set_bsc_flags)
      |? (Bsb_build_schemas.ppx_flags, `Arr (Bsb_default.set_ppx_flags ~cwd))
      |? (Bsb_build_schemas.refmt, `Str (Bsb_default.set_refmt ~cwd))
      |? (Bsb_build_schemas.refmt_flags, `Arr Bsb_default.set_refmt_flags)

      |? (Bsb_build_schemas.sources, `Id (fun x ->
          begin 
            let res = Bsb_build_ui.parsing_sources
                Bsb_build_ui.lib_dir_index
                Filename.current_dir_name x in 
            generate_sourcedirs_meta res ;     

            bs_file_groups := res.files ;
            update_queue := res.intervals;
            globbed_dirs := res.globbed_dirs;
          end

        ))
      |> ignore
    | _ -> ()
  in
  begin match List.sort Ext_file_pp.interval_compare  !update_queue with
    | [] -> ()
    | queue ->
      let file_size = in_channel_length config_json_chan in
      let oc = open_out_bin config_file_bak in
      let () =
        Ext_file_pp.process_wholes
          queue file_size config_json_chan oc in
      close_out oc ;
      close_in config_json_chan ;
      Unix.unlink Literals.bsconfig_json;
      Unix.rename config_file_bak Literals.bsconfig_json
  end;
  let config = 
    {
      Bsb_config_types.package_name = Bsb_default.get_package_name ();
      ocamllex = Bsb_default.get_ocamllex ();
      external_includes = Bsb_default.get_bs_external_includes ();
      bsc_flags = Bsb_default.get_bsc_flags ();
      ppx_flags = Bsb_default.get_ppx_flags ();
      bs_dependencies = Bsb_default.get_bs_dependencies ();
      refmt = Bsb_default.get_refmt ();
      refmt_flags = Bsb_default.get_refmt_flags ();
      js_post_build_cmd =  Bsb_default.get_js_post_build_cmd ();
      package_specs = 
        (match override_package_specs with None ->  Bsb_default.get_package_specs()
                                         | Some x -> x );
      globbed_dirs = !globbed_dirs; 
      bs_file_groups = !bs_file_groups; 
      files_to_install = String_hash_set.create 96;
      built_in_dependency = !Bsb_default.built_in_package;
      generate_merlin = Bsb_default.get_generate_merlin ();
      reason_react_jsx = !reason_react_jsx ;  
    } in 
  merlin_file_gen 
    (bsc_dir // bsppx_exe,
     bsc_dir // Literals.reactjs_jsx_ppx_exe
    ) config;
  config






