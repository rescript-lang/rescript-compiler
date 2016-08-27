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
      Ast_extract.build_queue ppf
        (Ast_extract.sort (fun x -> x ) (fun x -> x )ast_table)
        ast_table
        Js_implementation.after_parsing_impl
        Js_implementation.after_parsing_sig        
    end        
  ;
  if String.length main_file <> 0 then
    let ast_table, result =
      Ast_extract.handle_main_file ppf
        Ocaml_parse.lazy_parse_implementation
        Ocaml_parse.lazy_parse_interface         
        main_file in
    if Queue.is_empty result then 
      Bs_exception.error (Bs_main_not_exist main_file)
    ;
    process_result ppf main_file ast_table result     
  else 0



                    
