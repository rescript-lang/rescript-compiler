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

(* FIXME: [Clflags.open_modules] seems not to be properly used *)
#if OCAML_VERSION =~ ">4.03.0" then 
module SMap = Depend.StringMap
let bound_vars = SMap.empty 
#else 
let bound_vars = String_set.empty 
#end

type 'a kind = 'a Ml_binary.kind 


let read_parse_and_extract (type t) (k : t kind) (ast : t) : String_set.t =
  Depend.free_structure_names := String_set.empty;
  Ext_ref.protect Clflags.transparent_modules false begin fun _ -> 
  List.iter
    (fun modname  ->
#if OCAML_VERSION =~ ">4.03.0" then
       ignore @@ 
#end       
       Depend.open_module bound_vars (Longident.Lident modname))
    (!Clflags.open_modules);
  (match k with
   | Ml_binary.Ml  -> Depend.add_implementation bound_vars ast
   | Ml_binary.Mli  -> Depend.add_signature bound_vars ast  ); 
  !Depend.free_structure_names
  end

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
let sort_files_by_dependencies ~(domain : String_set.t) (dependency_graph : String_set.t String_map.t) : 
  string Queue.t =
  let next current =
    String_map.find_exn  dependency_graph current in    
  let worklist = ref domain in
  let result = Queue.create () in
  let rec visit (visiting : String_set.t) path (current : string) =
    let next_path = current :: path in 
    if String_set.mem current visiting then
      Bs_exception.error (Bs_cyclic_depends next_path)
    else if String_set.mem current !worklist then
      begin
        let next_set = String_set.add current visiting in         
        next current |>        
        String_set.iter
          (fun node ->
             if  String_map.mem dependency_graph node then
               visit next_set next_path node)
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



let sort  project_ml project_mli (ast_table : _ t String_map.t) = 
  let domain =
    String_map.fold ast_table String_set.empty 
      (fun k _ acc -> String_set.add k acc)
  in
  let h =
    String_map.map ast_table
      (fun
        ({ast_info})
        ->
          match ast_info with
          | Ml (_, ast,  _)
            ->
            read_parse_and_extract Ml (project_ml ast)            
          | Mli (_, ast, _)
            ->
            read_parse_and_extract Mli (project_mli ast)
          | Ml_mli (_, impl, _, _, intf, _)
            ->
            String_set.union
              (read_parse_and_extract Ml (project_ml impl))
              (read_parse_and_extract Mli (project_mli intf))              
      ) in    
  sort_files_by_dependencies  ~domain h

(** same as {!Ocaml_parse.check_suffix} but does not care with [-c -o] option*)
let check_suffix  name  = 
  if Ext_path.check_suffix_case name ".ml"
  || Ext_path.check_suffix_case name ".mlt" then 
    `Ml,
    Ext_path.chop_extension_if_any  name 
  else if Ext_path.check_suffix_case name !Config.interface_suffix then 
    `Mli,   Ext_path.chop_extension_if_any  name 
  else 
    raise(Arg.Bad("don't know what to do with " ^ name))


let collect_ast_map ppf files parse_implementation parse_interface  =
  Ext_list.fold_left files String_map.empty
    (fun acc source_file ->
      match check_suffix source_file with
      | `Ml, opref ->
        let module_name = Ext_modulename.module_name_of_file source_file in
        begin match String_map.find_exn acc module_name with
          | exception Not_found ->
            String_map.add acc module_name
              {ast_info =
                 (Ml (source_file, parse_implementation
                        ppf source_file, opref));
               module_name ;
              } 
          | {ast_info = (Ml (source_file2, _, _)
                        | Ml_mli(source_file2, _, _,_,_,_))} ->
            Bs_exception.error
              (Bs_duplicated_module (source_file, source_file2))
          | {ast_info =  Mli (source_file2, intf, opref2)}
            ->
            String_map.add acc module_name
              {ast_info =
                 Ml_mli (source_file,
                         parse_implementation ppf source_file,
                         opref,
                         source_file2,
                         intf,
                         opref2
                        );
               module_name} 
        end
      | `Mli, opref ->
        let module_name = Ext_modulename.module_name_of_file source_file in
        begin match String_map.find_exn acc module_name with
          | exception Not_found ->
            String_map.add acc module_name
              {ast_info = (Mli (source_file, parse_interface
                                  ppf source_file, opref));
               module_name } 
          | {ast_info =
               (Mli (source_file2, _, _) |
                Ml_mli(_,_,_,source_file2,_,_)) } ->
            Bs_exception.error
              (Bs_duplicated_module (source_file, source_file2))
          | {ast_info = Ml (source_file2, impl, opref2)}
            ->
            String_map.add acc module_name
              {ast_info =
                 Ml_mli
                   (source_file2,
                    impl,
                    opref2,
                    source_file,
                    parse_interface ppf source_file,
                    opref
                   );
               module_name} 
        end
    ) 
;;
type dir_spec = 
  { dir : string ;
    mutable  excludes : string list 
  }

let collect_from_main 
    ?(extra_dirs=[])
    ?(excludes=[])
    ?alias_map
    (ppf : Format.formatter)
    parse_implementation
    parse_interface
    project_impl 
    project_intf 
    main_module =
  let files = 
    Ext_list.fold_left extra_dirs [] (fun acc dir_spec -> 
        let  dirname, excludes = 
          match dir_spec with 
          | { dir =  dirname; excludes = dir_excludes} ->
            (*   dirname, excludes *)
            (* | `Dir_with_excludes (dirname, dir_excludes) -> *)
            dirname,
             (Ext_list.flat_map_append 
              dir_excludes  excludes
              (fun x -> [x ^ ".ml" ; x ^ ".mli" ])
              ) 
        in 
        Ext_array.fold_left (Sys.readdir dirname) acc (fun acc source_file -> 
            if (Ext_string.ends_with source_file ".ml" ||
                Ext_string.ends_with source_file ".mli" )
            && (* not_excluded source_file *) (not (Ext_list.mem_string excludes source_file ))
            then 
              (Filename.concat dirname source_file) :: acc else acc
          ) )
  in
  let ast_table = collect_ast_map ppf files parse_implementation parse_interface in 
  let visited = String_hashtbl.create 31 in
  let result = Queue.create () in  
  let next module_name : String_set.t =
    let module_set = 
      match String_map.find_exn ast_table module_name with
      | exception _ -> String_set.empty
      | {ast_info = Ml (_,  impl, _)} ->
        read_parse_and_extract Ml (project_impl impl)
      | {ast_info = Mli (_,  intf,_)} ->
        read_parse_and_extract Mli (project_intf intf)
      | {ast_info = Ml_mli(_, impl, _, _,  intf, _)}
        -> 
        String_set.union
          (read_parse_and_extract Ml (project_impl impl))
          (read_parse_and_extract Mli (project_intf intf))
    in 
    match alias_map with 
    | None -> module_set 
    | Some map -> 
      String_set.fold (fun x acc -> String_set.add (String_hashtbl.find_default map x x) acc  ) module_set String_set.empty
  in
  let rec visit visiting path current =
    if String_set.mem current visiting  then
      Bs_exception.error (Bs_cyclic_depends (current::path))
    else
    if not (String_hashtbl.mem visited current)
    && String_map.mem ast_table current then
      begin
        String_set.iter
          (visit
             (String_set.add current visiting)
             (current::path))
          (next current) ;
        Queue.push current result;
        String_hashtbl.add visited current ();
      end in
  visit (String_set.empty) [] main_module ;
  ast_table, result   


let build_queue ppf queue
    (ast_table : _ t String_map.t)
    after_parsing_impl
    after_parsing_sig    
  =
  queue
  |> Queue.iter
    (fun modname -> 
       match String_map.find_exn ast_table modname  with
       | {ast_info = Ml(source_file,ast, opref)}
         -> 
         after_parsing_impl ppf source_file 
           opref ast 
       | {ast_info = Mli (source_file,ast,opref) ; }  
         ->
         after_parsing_sig ppf source_file 
           opref ast 
       | {ast_info = Ml_mli(source_file1,impl,opref1,source_file2,intf,opref2)}
         -> 
         after_parsing_sig ppf source_file1 opref1 intf ;
         after_parsing_impl ppf source_file2 opref2 impl
       | exception Not_found -> assert false 
    )


let handle_queue 
  ppf 
  queue ast_table 
  decorate_module_only 
  decorate_interface_only 
  decorate_module = 
  queue 
  |> Queue.iter
    (fun base ->
       match (String_map.find_exn ast_table base ).ast_info with
       | exception Not_found -> assert false
       | Ml (ml_name,  ml_content, _)
         ->
         decorate_module_only  base ml_name ml_content
       | Mli (mli_name , mli_content, _) ->
         decorate_interface_only base  mli_name mli_content
       | Ml_mli (ml_name, ml_content, _, mli_name,   mli_content, _)
         ->
         decorate_module  base mli_name ml_name mli_content ml_content

    )



let build_lazy_queue ppf queue (ast_table : _ t String_map.t)
    after_parsing_impl
    after_parsing_sig    
  =
  queue |> Queue.iter (fun modname -> 
      match String_map.find_exn ast_table modname  with
      | {ast_info = Ml(source_file,lazy ast, opref)}
        -> 
        after_parsing_impl ppf source_file opref ast 
      | {ast_info = Mli (source_file,lazy ast,opref) ; }  
        ->
        after_parsing_sig ppf source_file opref ast 
      | {ast_info = Ml_mli(source_file1,lazy impl,opref1,source_file2,lazy intf,opref2)}
        -> 
        after_parsing_sig ppf source_file1 opref1 intf ;
        after_parsing_impl ppf source_file2 opref2 impl
      | exception Not_found -> assert false 
    )

