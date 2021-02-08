(* Copyright (C) Authors of ReScript
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

#if 
  BS_RELEASE_BUILD 
  (*true *)
then

let load_cmi ~unit_name : Env.Persistent_signature.t option =
  match Config_util.find_opt (unit_name ^".cmi") with 
  | Some filename -> Some {filename; cmi = Cmi_format.read_cmi filename}
  | None ->
    if !Js_config.no_stdlib then None
    else 
      match Ext_string_array.find_sorted Builtin_cmi_datasets.module_names unit_name with
      | Some cmi ->
        if Js_config.get_diagnose () then
          Format.fprintf Format.err_formatter ">Cmi: %s@." unit_name;
        let cmi : Cmi_format.cmi_infos = 
            Ext_marshal.from_string_uncheck
            Builtin_cmi_datasets.module_data.(cmi)  in   
        if Js_config.get_diagnose () then
          Format.fprintf Format.err_formatter "<Cmi: %s@." unit_name;
        Some {filename = Sys.executable_name ; 
              cmi }
      | None -> None

 let check () = ()
(*  
  Ext_array.iter 
    Builtin_cmi_datasets.module_sets_cmi
    (fun (name,l) ->
       prerr_endline (">checking " ^ name);
       let cmi = Lazy.force l in 
       (match cmi.cmi_crcs with 
        | (unit , Some digest) :: _  ->
          Format.fprintf Format.err_formatter "%s -> %s@." unit (Digest.to_hex digest)
        | _ -> ());
       prerr_endline ("<checking " ^ name);
    );
  Ext_array.iter 
    Builtin_cmj_datasets.module_sets
    (fun (name,l) ->
       prerr_endline (">checking " ^ name);
       let cmj = Lazy.force l in 
       Format.fprintf Format.err_formatter "%b@." cmj.pure;
       prerr_endline ("<checking " ^ name);
    )  *)
#else

let check () = ()
let load_cmi ~unit_name : Env.Persistent_signature.t option =
  match Config_util.find_opt (unit_name ^".cmi") with 
  | Some filename -> Some {filename; cmi = Cmi_format.read_cmi filename}
  | None -> None 
#end    