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

(* we can cache it, since all deps have already being processed,
   but having this functionalilty will introduce deps on {!Unix.stat}
*)
let process_result ppf  main_file ast_table result = 
  if Js_config.get_diagnose () then
    Format.fprintf Format.err_formatter
      "Order: @[%a@]@."
      (Ext_format.pp_print_queue
         ~pp_sep:Format.pp_print_space
         Format.pp_print_string)
      result ;
  Ast_extract.build_lazy_queue ppf result ast_table
    Js_implementation.after_parsing_impl
    Js_implementation.after_parsing_sig 
  ;
  if not (!Clflags.compile_only) then
    Sys.command
      ("node " ^
        Ext_package_name.js_name_of_basename (Filename.chop_extension main_file)
      )
  else 0

type task = 
  | Main of string
  | Eval of string 
  (* currently we just output JS file, 
     it is compilicated to run via node.
     1. Create a temporary file, it has to be in the same directory?
     2. Via `node -e`, we need a module to do shell escaping properly
  *)
  | None


let print_if ppf flag printer arg =
  if !flag then Format.fprintf ppf "%a@." printer arg;
  arg

let batch_compile ppf search_dirs files main_file =
  Compenv.readenv ppf Before_compile; 
  Compmisc.init_path  false;
  if files <> [] then 
    begin
      let ast_table =
        Ast_extract.collect_ast_map ppf files
          Ocaml_parse.parse_implementation
          Ocaml_parse.parse_interface in
      Ast_extract.build_queue ppf
        (Ast_extract.sort Ext_pervasives.id  Ext_pervasives.id  ast_table)
        ast_table
        Js_implementation.after_parsing_impl
        Js_implementation.after_parsing_sig        
    end        
  ;
  begin match main_file with
    | Main main_file -> 
      let main_module = (Ext_filename.module_name_of_file main_file) in
      let ast_table, result =
        Ast_extract.collect_from_main ppf 
          ~extra_dirs:(List.map
                         (fun x -> 
                            ({ dir = x ; excludes = [] } : Ast_extract.dir_spec)) search_dirs)
          Ocaml_parse.lazy_parse_implementation
          Ocaml_parse.lazy_parse_interface         
          Lazy.force
          Lazy.force
          main_module
      in
      if Queue.is_empty result then
        Bs_exception.error (Bs_main_not_exist main_module);
      (* ; Not necessary since we will alwasy check [main_file] is valid or not,
         so if we support 
         bsc -I xx -I yy -bs-main Module_name
      *)
      process_result ppf main_file ast_table result     
    | None ->  0
    | Eval s ->
      Ext_ref.protect_list 
        [Clflags.dont_write_files , true ; 
         Clflags.annotations, false;
         Clflags.binary_annotations, false;
         Js_config.dump_js, true ;
        ]  (fun _ -> 
            Ocaml_parse.parse_implementation_from_string s 
            (* FIXME: Note in theory, the order of applying our built in ppx 
               and apply third party ppx should not matter, but in practice  
               it may.
               We should make it more consistent. 
               Thirdy party ppx may be buggy to drop annotations.
               If we always put our ppx in the beginning, it will be more robust, 
               however, the current implementation (in the batch compilation mode) 
               seems to apply our ppx after all ppx transformations
            *)
            |> Pparse.apply_rewriters_str ~tool_name:Js_config.tool_name
            |> print_if ppf Clflags.dump_parsetree Printast.implementation
            |> print_if ppf Clflags.dump_source Pprintast.structure
            |> Js_implementation.after_parsing_impl ppf "//<toplevel>//" "Bs_internal_eval" 
          ); 0
  end




