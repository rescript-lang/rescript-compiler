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

(** Regenerate ninja file by need based on [.bsdeps]
    return None if we dont need regenerate
    otherwise return Some info
*)
let regenerate_ninja 
    ~not_dev 
    ~(override_package_specs : Bsb_package_specs.t option)
    ~generate_watch_metadata 
    ~forced cwd bsc_dir
  : Bsb_config_types.t option =
  let output_deps = cwd // Bsb_config.lib_bs // bsdeps in
  let check_result  =
    Bsb_ninja_check.check 
      ~cwd  
      ~forced ~file:output_deps in
  Bsb_log.info
    "@{<info>BSB check@} build spec : %a @." Bsb_ninja_check.pp_check_result check_result ;
  match check_result  with 
  | Good ->
    None  (* Fast path, no need regenerate ninja *)
  | Bsb_forced 
  | Bsb_bsc_version_mismatch 
  | Bsb_file_not_exist 
  | Bsb_source_directory_changed  
  | Other _ -> 
    if check_result = Bsb_bsc_version_mismatch then begin 
      Bsb_log.info "@{<info>Different compiler version@}: clean current repo";
      Bsb_clean.clean_self bsc_dir cwd; 
    end ; 
    Bsb_build_util.mkp (cwd // Bsb_config.lib_bs); 
    let config = 
      Bsb_config_parse.interpret_json 
        ~override_package_specs
        ~bsc_dir
        ~generate_watch_metadata
        ~not_dev
        cwd in 
    Bsb_merlin_gen.merlin_file_gen ~cwd
      (bsc_dir // bsppx_exe) config;       
    Bsb_ninja_gen.output_ninja_and_namespace_map 
      ~cwd ~bsc_dir ~not_dev config ;         
    (* PR2184: we still need record empty dir 
        since it may add files in the future *)  
    Bsb_ninja_check.record ~cwd ~file:output_deps 
      (Literals.bsconfig_json::config.globbed_dirs) ;
    Some config 


