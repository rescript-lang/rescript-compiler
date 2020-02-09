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


 



let get_files ext dir = 
  let arr =     
    Ext_array.filter_map (Sys.readdir dir)
      (fun  x ->
         if Ext_string.ends_with x  ext 
         then Some (Filename.concat dir x) else None )
  in
  (* Sort to guarantee it works the same across OSes *)
  Array.sort (fun (x : string) y -> Pervasives.compare x y ) arr;
  Array.to_list arr

let from_cmj (files : string list) (output_file : string) = 
  let raw_to_str f str = 
    Ext_pp.string f "\""   ;
    Ext_pp.string f (String.escaped str);
    Ext_pp.string f "\""
  in  
  let v = open_out_bin output_file in
  Ext_pervasives.finally v ~clean:close_out (fun v ->   
      let f = Ext_pp.from_channel v in  
      let aux file =                 
        let str = Ext_io.load_file file in
        begin
#if 0 then           
          prerr_endline (* can not embed corrupted data *)
            (Printf.sprintf "Begin Verifying %s" file);
          let _  = Js_cmj_format.from_string str in          
          prerr_endline "End";
#end
          Ext_pp.paren_group f 1 (fun _ ->
          raw_to_str f (Filename.basename file) ;
          Ext_pp.string f ",";
          Ext_pp.string f "lazy";
          Ext_pp.space f ;
          Ext_pp.paren_group f 1 (fun _ ->
              Ext_pp.string f "Js_cmj_format.from_string " ;
              raw_to_str f str));
          Ext_pp.string f  ";";
          Ext_pp.newline f           
        end
      in

      Ext_pp.newline f ;
      Ext_pp.string f "let data_sets = let map = Map_string.of_list "    ;
      Ext_pp.bracket_vgroup f 1 (fun _ -> List.iter aux files);
      Ext_pp.string f " in ref map") 


(** the cache should be readable and also update *)
let _raw_to_str f str = 
  Ext_pp.string f "\""   ;
  Ext_pp.string f (String.escaped str);
  Ext_pp.string f "\""

let from_cmi (files : string list) (output_file : string) = 
  let files = List.sort (fun filea fileb  ->
     Ext_string_array.cmp (Ext_filename.module_name filea) (Ext_filename.module_name fileb)) files in 
  let v = open_out_bin output_file in
  Ext_pervasives.finally v ~clean:close_out (fun f ->         
      output_string f 
        (Printf.sprintf {|let module_sets = [|
%s
        |]|}
          (String.concat ";\n" (Ext_list.map files (fun x -> "\"" ^ Ext_filename.module_name x ^ "\"")))
         ) ;
      output_string f "\n";
      output_string f 
      (Printf.sprintf {|let module_sets_cmi : Cmi_format.cmi_infos Lazy.t array = [|
      %s
      |] 
      |} (String.concat ";\n" (Ext_list.map files (fun file -> 
        Printf.sprintf "lazy (Marshal.from_string %S 0)"
          (let content = (Cmi_format.read_cmi file) in 
            Marshal.to_string content []
          (* let header_len = (String.length Config.cmi_magic_number) in 
          String.sub content header_len (String.length content - header_len) *)
      )))
      )
      ))
      ;;

let stdlib = "stdlib-406"

let () = 
  from_cmj ( Ext_list.append (get_files Literals.suffix_cmj stdlib)
             (Ext_list.append (get_files Literals.suffix_cmj "runtime")
             (get_files Literals.suffix_cmj "others"))) 
    (Filename.concat "core" "js_cmj_datasets.ml")
  (* from_cmi ( Ext_list.append (get_files Literals.suffix_cmi stdlib)
               (Ext_list.append (get_files Literals.suffix_cmi "runtime")
                  (get_files Literals.suffix_cmi "others"))) 
    (Filename.concat "core" "js_cmi_datasets.ml") *)

