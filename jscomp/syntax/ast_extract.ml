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



module String_set = Depend.StringSet

let read_parse_and_extract ast extract_function : String_set.t =
  Depend.free_structure_names := String_set.empty;
  (let bound_vars = String_set.empty in
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
    ast : ast;
    module_name : string     
  }


let module_name_of_file file =
    String.capitalize 
      (Filename.chop_extension @@ Filename.basename file)  


let merge (files : (info * String_set.t) list )  =
  let tbl = Hashtbl.create 31 in     
  let domain =
    List.fold_left
      (fun acc ({ source_file ; module_name}, _)
        ->
          Hashtbl.add tbl module_name source_file;
          String_set.add module_name acc           
      ) String_set.empty files in

  tbl,domain, List.fold_left
    (fun  acc ({source_file = file; module_name  ; _}, deps) ->
       match String_map.find  module_name acc with 
       | new_deps -> 
         String_map.add  module_name
           (String_set.inter domain 
              (String_set.union deps new_deps)) acc
       | exception Not_found -> 
         String_map.add  module_name
           (String_set.inter deps domain) acc
    ) String_map.empty  files



  
let sort_files_by_dependencies  files
  =
  let tbl, domain, h  = merge  files in
  let next current =
    String_set.elements (String_map.find  current h) in    
  let worklist = ref domain in

  let result = Queue.create () in
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
             if  String_map.mem node  h then
               visit (current::path) node)
          depends ;
        worklist := String_set.remove  current !worklist;
        Queue.push current result ;
        Hashtbl.remove visited current;
      end in        
  while not (String_set.is_empty !worklist) do 
    visit  [] (String_set.choose !worklist)
  done;
  if Js_config.get_diagnose () then
    Format.fprintf Format.err_formatter
      "Order: @[%a@]@."    
      (Ext_format.pp_print_queue
         ~pp_sep:Format.pp_print_space
         Format.pp_print_string)
      result ;       
  result,tbl 
;;



let prepare  ast_table = 
  let file_dependencies 
      source_file ast  acc =
    let extracted_deps =
      read_parse_and_extract ast 
        (  match ast with
           | Ml (ast,_) -> fun set _ ->  Depend.add_implementation set ast 
           | Mli (ast,_) -> fun set _ ->   Depend.add_signature set ast ) in
    ({source_file ; ast ; module_name = module_name_of_file source_file },
     extracted_deps) :: acc  in
  let files = Hashtbl.fold file_dependencies ast_table []  in
  sort_files_by_dependencies  files 


