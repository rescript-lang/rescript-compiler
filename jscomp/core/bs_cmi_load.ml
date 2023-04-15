(* Copyright (C) Hongbo Zhang, Authors of ReScript
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

#ifdef RELEASE 
(*true *)

let cmi_cache_path =
  let ( // ) = Filename.concat in
  Filename.dirname Sys.executable_name
  // Filename.parent_dir_name // "lib" // "cmi_cache.bin"
  
let load_cmi_cache () =
  let channel = open_in_bin cmi_cache_path in
  let cache: Cmij_cache.t = Marshal.from_channel channel in
  close_in channel;
  cache

let cmi_cache = lazy (load_cmi_cache ())

let load_cmi ~unit_name : Env.Persistent_signature.t option =
  match Config_util.find_opt (unit_name ^".cmi") with 
  | Some filename -> Some {filename; cmi = Cmi_format.read_cmi filename}
  | None ->
    if !Js_config.no_stdlib then None
    else
      let {Cmij_cache.module_names; module_data} = Lazy.force cmi_cache in
      match Ext_string_array.find_sorted module_names unit_name with
      | Some index ->
        if !Js_config.diagnose then
          Format.fprintf Format.err_formatter ">Cmi: %s@." unit_name;
        let cmi = Cmij_cache.unmarshal_cmi_data module_data.(index) in
        if !Js_config.diagnose then
          Format.fprintf Format.err_formatter "<Cmi: %s@." unit_name;
        Some {filename = Sys.executable_name ; 
              cmi }
      | None -> None

#else

let load_cmi ~unit_name : Env.Persistent_signature.t option =
  match Config_util.find_opt (unit_name ^".cmi") with 
  | Some filename -> Some {filename; cmi = Cmi_format.read_cmi filename}
  | None -> None 
#endif    