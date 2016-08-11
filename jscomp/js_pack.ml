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






let get_files dir = 
  let arr = 
    Sys.readdir dir 
    |> Ext_array.filter_map 
        (fun  x -> if Ext_string.ends_with x Js_config.cmj_ext then Some (Filename.concat dir x) else None )
  in
  (* Sort to guarantee it works the same across OSes *)
  Array.sort (fun (x : string) y -> Pervasives.compare x y ) arr;
  Array.to_list arr

let from_cmj files output_file = 
  let raw_to_str f str = 
    Ext_pp.string f "\""   ;
    Ext_pp.string f (Ext_string.escaped str);
    Ext_pp.string f "\""
  in  
  let v = open_out_bin output_file in
  Ext_pervasives.finally v close_out (fun v ->   
      let f = Ext_pp.from_channel v in  
      let aux file = 
        let in_chan = open_in_bin file in
        let len = in_channel_length in_chan in
        let str = really_input_string in_chan len in
        begin
          prerr_endline (* can not embed corrupted data *)
            (Printf.sprintf "Begin Verifying %s" file);
          let _  = Js_cmj_format.from_string str in          
          prerr_endline "End";

          Ext_pp.paren_group f 1 (fun _ ->
          raw_to_str f (Filename.basename file) ;
          Ext_pp.string f ",";
          Ext_pp.string f "lazy";
          Ext_pp.space f ;
          Ext_pp.paren_group f 1 (fun _ ->
              Ext_pp.string f "Js_cmj_format.from_string " ;
              raw_to_str f str));
          Ext_pp.string f  ";";
          Ext_pp.newline f ;          
          close_in in_chan;        
        end
      in
      Ext_pp.string f "(* -*-mode:fundamental-*- *)"  ;
      Ext_pp.newline f ;
      Ext_pp.string f "let cmj_data_sets = String_map.of_list "    ;
      Ext_pp.bracket_vgroup f 1 (fun _ -> List.iter aux files))


let () = 
  from_cmj (get_files "stdlib" @ get_files "runtime") "js_cmj_datasets.ml";;  

