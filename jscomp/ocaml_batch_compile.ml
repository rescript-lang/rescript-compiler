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



let build_queue ppf queue (ast_table : _ Ast_extract.t String_map.t) =
  queue |> Queue.iter (fun modname -> 
      match String_map.find modname ast_table  with
      | {ast_info = Ml(source_file,ast, opref)}
        -> 
        Js_implementation.after_parsing_impl ppf source_file 
          opref ast 
      | {ast_info = Mli (source_file,ast,opref) ; }  
        ->
        Js_implementation.after_parsing_sig ppf source_file 
              opref ast 
      | {ast_info = Ml_mli(source_file1,impl,opref1,source_file2,intf,opref2)}
        -> 
        Js_implementation.after_parsing_sig ppf source_file1 opref1 intf ;
        Js_implementation.after_parsing_impl ppf source_file2 opref2 impl
      | exception Not_found -> assert false 
    )

let build_lazy_queue ppf queue (ast_table : _ Ast_extract.t String_map.t) =
  queue |> Queue.iter (fun modname -> 
      match String_map.find modname ast_table  with
      | {ast_info = Ml(source_file,lazy ast, opref)}
        -> 
        Js_implementation.after_parsing_impl ppf source_file 
          opref ast 
      | {ast_info = Mli (source_file,lazy ast,opref) ; }  
        ->
        Js_implementation.after_parsing_sig ppf source_file 
              opref ast 
      | {ast_info = Ml_mli(source_file1,lazy impl,opref1,source_file2,lazy intf,opref2)}
        -> 
        Js_implementation.after_parsing_sig ppf source_file1 opref1 intf ;
        Js_implementation.after_parsing_impl ppf source_file2 opref2 impl
      | exception Not_found -> assert false 
    )



module String_set = Depend.StringSet


let handle_main_file ppf main_file =
  let dirname = Filename.dirname main_file in
  let files =
    Sys.readdir dirname
    |> Ext_array.to_list_f
      (fun source_file ->
         if Ext_string.ends_with source_file ".ml" ||
            Ext_string.ends_with source_file ".mli" then
           Some (Filename.concat dirname source_file)
         else None
      ) in
  let ast_table =
    Ast_extract.build ppf files
      Ocaml_parse.lazy_parse_implementation
      Ocaml_parse.lazy_parse_interface in 

  let visited = Hashtbl.create 31 in
  let result = Queue.create () in  
  let next module_name =
    match String_map.find module_name ast_table with
    | exception _ -> String_set.empty
    | {ast_info = Ml (_, lazy impl, _)} ->
      Ast_extract.read_parse_and_extract Ml_kind impl
    | {ast_info = Mli (_, lazy intf,_)} ->
      Ast_extract.read_parse_and_extract Mli_kind intf
    | {ast_info = Ml_mli(_,lazy impl, _, _, lazy intf, _)}
      -> 
      String_set.union
        (Ast_extract.read_parse_and_extract Ml_kind impl)
        (Ast_extract.read_parse_and_extract Mli_kind intf)
  in
  let rec visit visiting path current =
    if String_set.mem current visiting  then
      Bs_exception.error (Bs_cyclic_depends (current::path))
    else
    if not (Hashtbl.mem visited current)
    && String_map.mem current ast_table then
      begin
        String_set.iter
          (visit
             (String_set.add current visiting)
             (current::path))
          (next current) ;
        Queue.push current result;
        Hashtbl.add visited current ();
      end in
  visit (String_set.empty) [] (Ext_filename.module_name_of_file main_file) ;
  if Js_config.get_diagnose () then
    Format.fprintf Format.err_formatter
      "Order: @[%a@]@."
      (Ext_format.pp_print_queue
         ~pp_sep:Format.pp_print_space
         Format.pp_print_string)
      result ;
  build_lazy_queue ppf result ast_table;
  if not (!Clflags.compile_only) then
    Sys.command
      ("node " ^ Filename.chop_extension main_file ^ ".js")
  else 0


let batch_compile ppf files main_file =
  Compenv.readenv ppf Before_compile; 
  Compmisc.init_path  false;
  if files <> [] then 
    begin
      let ast_table =
        Ast_extract.build ppf files
          Ocaml_parse.parse_implementation
          Ocaml_parse.parse_interface in
      build_queue ppf (Ast_extract.sort (fun x -> x ) (fun x -> x )ast_table) ast_table
    end        
  ;
  if String.length main_file <> 0 then
    handle_main_file ppf main_file
  else 0



                    
