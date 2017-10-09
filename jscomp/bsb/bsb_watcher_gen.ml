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

let sourcedirs_meta = ".sourcedirs.json"

let generate_sourcedirs_meta cwd (res : Bsb_parse_sources.t) = 
  let ochan = open_out_bin (cwd // Bsb_config.lib_bs // sourcedirs_meta) in
  let v = 
    Ext_json_noloc.(
      kvs [
        "dirs" ,
      arr (Ext_array.of_list_map ( fun (x : Bsb_parse_sources.file_group) -> 
      str x.dir 
      ) res.files ) ;
      "generated" ,
      arr @@ Array.of_list @@ List.fold_left (fun acc (x : Bsb_parse_sources.file_group) -> 
      Ext_list.flat_map_append 
      (fun x -> Ext_list.map str x.Bsb_parse_sources.output)   
      x.generators acc
      )  [] res.files 
      ]
     ) in 
  Ext_json_noloc.to_channel ochan v ;
  close_out ochan