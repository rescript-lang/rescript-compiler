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





let read_parse_and_extract ast extract_function : Depend.StringSet.t =
  Depend.free_structure_names := Depend.StringSet.empty;
  (let bound_vars = Depend.StringSet.empty in
  List.iter
    (fun modname  ->
      Depend.open_module bound_vars (Longident.Lident modname))
    (!Clflags.open_modules);
  extract_function bound_vars ast;
  !Depend.free_structure_names)




type ast = 
  | Ml of Parsetree.structure * string 
  | Mli of Parsetree.signature * string 

type  info = 
  { source_file : string ; 
    ast : ast
  }



let normalize tbl file  =
  let module_name = 
    String.capitalize 
      (Filename.chop_extension @@ Filename.basename file)  in
  Hashtbl.add tbl module_name file;
  (* could have both mli and ml *)
  module_name 

let merge tbl (files : (info * Depend.StringSet.t) list ) : 
  (string, Depend.StringSet.t) Hashtbl.t  
  =


  let domain = 
    Depend.StringSet.of_list 
      (List.map (fun ({ source_file },_)-> normalize tbl source_file) files) in
  let local_tbl = Hashtbl.create 31 in 
  List.iter 
    (fun  ({source_file = file; _}, deps) ->
       let modname = String.capitalize 
           (Filename.chop_extension @@ Filename.basename file) in
       match Hashtbl.find local_tbl modname with 
       | new_deps -> 
         Hashtbl.replace local_tbl modname 
           (Depend.StringSet.inter domain 
              (Depend.StringSet.union deps new_deps))
       | exception Not_found -> 
         Hashtbl.add local_tbl  modname (Depend.StringSet.inter deps domain)
    ) files  ;
  local_tbl

exception Cyclic_dependends of string list
  
let sort_files_by_dependencies tbl files
  =
  let h : (string, Depend.StringSet.t) Hashtbl.t = merge tbl files in
  let next current =
    Depend.StringSet.elements (Hashtbl.find h current) in    
  let worklist = 
    ref (Hashtbl.fold
           (fun key _  acc ->
              String_set.add key acc) h String_set.empty) in

  let result = Ext_list.create_ref_empty () in
  let visited = Hashtbl.create 31 in (* Temporary mark *)  

  (* only visit nodes that are currently in the domain *)
  (* https://en.wikipedia.org/wiki/Topological_sorting *)
  (* dfs   *)
  let rec visit path current =
    if Hashtbl.mem visited current then
      Bs_exception.error (Bs_cyclic_depends path)
    else if String_set.mem current !worklist then
      begin
        Hashtbl.add visited current () ;
        let depends = next current in
        List.iter
          (fun node ->
             if  Hashtbl.mem h node then
               visit (current::path) node)
          depends ;
        worklist := String_set.remove  current !worklist;
        Ext_list.ref_push current result ;
        Hashtbl.remove visited current;
      end in        
  while not (String_set.is_empty !worklist) do 
    visit  [] (String_set.choose !worklist)
  done;
  if Js_config.get_diagnose () then
    Format.fprintf Format.err_formatter
      "Reverse order: @[%a@]@."    
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Format.pp_print_string)
      !result ;       
  !result
;;



let prepare  ast_table = 
  let file_dependencies 
      source_file ast  acc =
    let extracted_deps =
      read_parse_and_extract ast 
        (  match ast with
           | Ml (ast,_) -> fun set _ ->  Depend.add_implementation set ast 
           | Mli (ast,_) -> fun set _ ->   Depend.add_signature set ast ) in
    ({source_file ; ast }, extracted_deps) :: acc  in
  let files = Hashtbl.fold file_dependencies ast_table []  in
  let tbl = Hashtbl.create 31 in   
  let stack = sort_files_by_dependencies tbl files in 
  stack, tbl 

