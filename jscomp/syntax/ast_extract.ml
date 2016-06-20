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

let file_dependencies (files : (info * Depend.StringSet.t) list ref)
    source_file ast  =
  let extracted_deps =
    read_parse_and_extract ast 
     (  match ast with
     | Ml (ast,_) -> fun set _ ->  Depend.add_implementation set ast 
     | Mli (ast,_) -> fun set _ ->   Depend.add_signature set ast ) in
  files := ({source_file ; ast }, extracted_deps) :: !files


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


let sort_files_by_dependencies tbl files
    =
  let h = merge tbl files in
  let worklist = ref [] in
  let ()= 
    Hashtbl.iter (fun key _     ->  worklist := key :: !worklist ) h in
  let result = ref [] in
  let visited = Hashtbl.create 31 in

  while not @@ (function [] -> true | _ ->  false) !worklist do 
    let current = List.hd !worklist  in 
    if Hashtbl.mem visited current then
      worklist := List.tl !worklist
    else 
      match Depend.StringSet.elements (Hashtbl.find h current) with 
      | depends -> 
          let really_depends = 
            List.filter 
              (fun x ->  (Hashtbl.mem h x && (not (Hashtbl.mem visited x ))))
              depends in
          begin match really_depends with 
          |[] -> 
            begin
              let v = List.hd !worklist in 
              worklist := List.tl !worklist ; 
              Hashtbl.add visited  v () ;
              Ext_list.ref_push current result 
            end
          | _ -> 
              List.iter  (fun x -> worklist :=  x :: ! worklist) really_depends
          end
      | exception Not_found ->  assert false 
  done;
  result
;;



let prepare  ast_table = 
  let tbl = Hashtbl.create 31 in 
  let files = ref [] in
  Hashtbl.iter (fun sourcefile ast  -> file_dependencies files sourcefile ast) ast_table;
  let stack = sort_files_by_dependencies tbl !files in 
  !stack, tbl 

