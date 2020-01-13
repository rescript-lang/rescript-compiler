(* Copyright (C) 2019 - Authors of BuckleScript
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

#if BS_NATIVE then
let cmdline_backend = ref None

(* If cmdline_backend is set we use it, otherwise we use the first entry's backend. *)
let backend = lazy
  begin match !cmdline_backend with
  | Some cmdline_backend -> cmdline_backend
  | None -> 
    let entries = Bsb_config_parse.entries_from_bsconfig Bsb_global_paths.cwd in 
    let new_cmdline_backend = begin match entries with
      | []                                       -> Bsb_config_types.Js
      | (Bsb_config_types.JsTarget _) :: _       -> Bsb_config_types.Js
      | (Bsb_config_types.NativeTarget _) :: _   -> Bsb_config_types.Native
      | (Bsb_config_types.BytecodeTarget _) :: _ -> Bsb_config_types.Bytecode
    end in
    cmdline_backend := Some (new_cmdline_backend);
    new_cmdline_backend
  end
#else
let backend = lazy Bsb_config_types.Js

(* No cost of using this variable below when compiled in JS mode. *)
let cmdline_backend = ref (Some Bsb_config_types.Js)
#end

let (//) = Ext_path.combine

let lib_artifacts_dir = lazy
  begin match Lazy.force backend with
  | Bsb_config_types.Js       -> Bsb_config.lib_bs
  | Bsb_config_types.Native   -> Bsb_config.lib_lit // "native"
  | Bsb_config_types.Bytecode -> Bsb_config.lib_lit // "bytecode"
  end
