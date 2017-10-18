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



let query_sources ({bs_file_groups} : Bsb_config_types.t) : Ext_json_noloc.t 
  = 
  bs_file_groups 
  |> Ext_array.of_list_map (fun (x : Bsb_parse_sources.file_group) -> 
    Ext_json_noloc.(
      kvs [
        "dir", str x.dir ;
        "sources" , 
        (String_map.keys x.sources)
        |> Ext_array.of_list_map str
        |> arr 
      ]
    )
  )
  |> Ext_json_noloc.arr 


let query_current_package_sources cwd bsc_dir = 
    let config_opt  = Bsb_ninja_regen.regenerate_ninja 
      ~not_dev:false
      ~override_package_specs:None
      ~generate_watch_metadata:true
      ~forced:true  cwd bsc_dir in 
    match config_opt with   
    | None -> None
     
    | Some config ->
      Some (query_sources config)


let query ~cwd ~bsc_dir str = 
  match str with 
  | "sources" -> 
    begin match query_current_package_sources cwd bsc_dir with 
    | None -> raise (Arg.Bad "internal error in query")
    | Some config -> 
      output_string stdout 
      (Printf.sprintf "QUERY-INFO-BEGIN(%s)\n" str);
      Ext_json_noloc.to_channel stdout 
      ( config );
      output_string stdout "\nQUERY-INFO-END\n";
    end
  | _ -> raise (Arg.Bad "Unsupported query")