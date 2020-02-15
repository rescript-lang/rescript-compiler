(* Copyright (C) Authors of BuckleScript
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



let load_cmi ~unit_name : Env.Persistent_signature.t option =
  match Config_util.find_opt (unit_name ^".cmi") with 
  | Some filename -> Some {filename; cmi = Cmi_format.read_cmi filename}
  | None ->
    if !Js_config.no_stdlib then None
    else 
      match Ext_string_array.find_sorted_assoc Builtin_cmi_datasets.module_sets_cmi unit_name with
      | Some cmi ->
        if Js_config.get_diagnose () then
          Format.fprintf Format.err_formatter ">Cmi: %s@." unit_name;
        let lazy cmi = cmi in   
        if Js_config.get_diagnose () then
          Format.fprintf Format.err_formatter "<Cmi: %s@." unit_name;
        Some {filename = Sys.executable_name ; 
              cmi }
      | None -> None
