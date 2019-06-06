(* Copyright (C) 2017- Authors of BuckleScript
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

(** TODO: create the animation effect 
    logging installed files
*)
let install_targets cwd (config : Bsb_config_types.t option) =  
  let install ~destdir file = 
     Bsb_file.install_if_exists ~destdir file  |> ignore
  in
  let install_filename_sans_extension destdir namespace x = 
    let x = 
      match namespace with 
      | None -> x 
      | Some ns -> Ext_namespace.make ~ns x in 
    install ~destdir (cwd // x ^  Literals.suffix_ml) ;
    install ~destdir (cwd // x ^  Literals.suffix_re) ;
    install ~destdir (cwd // x ^ Literals.suffix_mli) ;
    install ~destdir (cwd // x ^  Literals.suffix_rei) ;
    install ~destdir (cwd // Bsb_config.lib_bs//x ^ Literals.suffix_cmi) ;
    install ~destdir (cwd // Bsb_config.lib_bs//x ^ Literals.suffix_cmj) ;
    install ~destdir (cwd // Bsb_config.lib_bs//x ^ Literals.suffix_cmt) ;
    install ~destdir (cwd // Bsb_config.lib_bs//x ^ Literals.suffix_cmti) ;

  in   
  Ext_option.iter config (fun {files_to_install; namespace; package_name} -> 
      let destdir = cwd // Bsb_config.lib_ocaml in (* lib is already there after building, so just mkdir [lib/ocaml] *)
      if not @@ Sys.file_exists destdir then begin Unix.mkdir destdir 0o777  end;
      begin
        Bsb_log.info "@{<info>Installing started@}@.";
        begin match namespace with 
          | None -> ()
          | Some x -> 
            install_filename_sans_extension destdir None  x
        end;
        String_hash_set.iter files_to_install (install_filename_sans_extension destdir namespace) ;
        Bsb_log.info "@{<info>Installing finished@} @.";
      end)



let build_bs_deps cwd (deps : Bsb_package_specs.t) =

  let bsc_dir = Bsb_build_util.get_bsc_dir ~cwd in
  let vendor_ninja = bsc_dir // "ninja.exe" in
  Bsb_build_util.walk_all_deps  cwd (fun {top; cwd} ->
      if not top then
        begin 
          let config_opt = Bsb_ninja_regen.regenerate_ninja ~not_dev:true
              ~generate_watch_metadata:false
              ~override_package_specs:(Some deps) 
              ~forced:true
              cwd bsc_dir  in (* set true to force regenrate ninja file so we have [config_opt]*)
          let command = 
            {Bsb_unix.cmd = vendor_ninja;
             cwd = cwd // Bsb_config.lib_bs;
             args  = [|vendor_ninja|]
            } in     
          let eid =
            Bsb_unix.run_command_execv
              command in 
          if eid <> 0 then   
            Bsb_unix.command_fatal_error command eid;
          (* When ninja is not regenerated, ninja will still do the build, 
             still need reinstall check
             Note that we can check if ninja print "no work to do", 
             then don't need reinstall more
          *)
          install_targets cwd config_opt;
        end
    )


let make_world_deps cwd (config : Bsb_config_types.t option) =
  Bsb_log.info "Making the dependency world!@.";
  let deps =
    match config with
    | None ->
      (* When this running bsb does not read bsconfig.json,
         we will read such json file to know which [package-specs]
         it wants
      *)
      Bsb_config_parse.package_specs_from_bsconfig ()
    | Some config -> config.package_specs in
  build_bs_deps cwd deps