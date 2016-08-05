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

type module_name = private string

module String_set = Depend.StringSet

type _ kind =
  | Ml_kind : Parsetree.structure kind
  | Mli_kind : Parsetree.signature kind
        
let read_parse_and_extract (type t) (k : t kind) (ast : t) : String_set.t =
  Depend.free_structure_names := String_set.empty;
  let bound_vars = String_set.empty in
  List.iter
    (fun modname  ->
       Depend.open_module bound_vars (Longident.Lident modname))
    (!Clflags.open_modules);
  (match k with
   | Ml_kind  -> Depend.add_implementation bound_vars ast
   | Mli_kind  -> Depend.add_signature bound_vars ast  ); 
  !Depend.free_structure_names


type ('a,'b) ast_info =
  | Ml of
      string * (* sourcefile *)
      'a *
      string (* opref *)      
  | Mli of string * (* sourcefile *)
           'b *
           string (* opref *)
  | Ml_mli of
      string * (* sourcefile *)
      'a *
      string  * (* opref1 *)
      string * (* sourcefile *)      
      'b *
      string (* opref2*)

type ('a,'b) t =
  { module_name : string ; ast_info : ('a,'b) ast_info }


(* only visit nodes that are currently in the domain *)
(* https://en.wikipedia.org/wiki/Topological_sorting *)
(* dfs   *)
let sort_files_by_dependencies ~domain dependency_graph =
  let next current =
    (String_map.find  current dependency_graph) in    
  let worklist = ref domain in
  let result = Queue.create () in
  let rec visit visiting path current =
    if String_set.mem current visiting then
      Bs_exception.error (Bs_cyclic_depends (current::path))
    else if String_set.mem current !worklist then
      begin
        next current |>        
        String_set.iter
          (fun node ->
             if  String_map.mem node  dependency_graph then
               visit (String_set.add current visiting) (current::path) node)
        ;
        worklist := String_set.remove  current !worklist;
        Queue.push current result ;
      end in        
  while not (String_set.is_empty !worklist) do 
    visit String_set.empty []  (String_set.choose !worklist)
  done;
  if Js_config.get_diagnose () then
    Format.fprintf Format.err_formatter
      "Order: @[%a@]@."    
      (Ext_format.pp_print_queue
         ~pp_sep:Format.pp_print_space
         Format.pp_print_string)
      result ;       
  result
;;



let sort  (ast_table : _ t String_map.t) = 
  let domain =
    String_map.fold
      (fun k _ acc -> String_set.add k acc)
      ast_table String_set.empty in
  let h =
    String_map.map
      (fun
        ({ast_info})
        ->
          match ast_info with
          | Ml (_, ast,  _)
            ->
            read_parse_and_extract Ml_kind ast            
          | Mli (_, ast, _)
            ->
            read_parse_and_extract Mli_kind ast
          | Ml_mli (_, impl, _, _, intf, _)
            ->
            String_set.union
              (read_parse_and_extract Ml_kind impl)
              (read_parse_and_extract Mli_kind intf)              
      ) ast_table in    
  sort_files_by_dependencies  domain h


