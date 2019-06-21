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



type task = 

  | Bsc_task_eval of string 
  (* currently we just output JS file, 
     it is compilicated to run via node.
     1. Create a temporary file, it has to be in the same directory?
     2. Via `node -e`, we need a module to do shell escaping properly
  *)
  | Bsc_task_none


let print_if ppf flag printer arg =
  if !flag then Format.fprintf ppf "%a@." printer arg;
  arg

let batch_compile ppf main_file =
#if OCAML_VERSION =~ ">4.03.0" then
  Compenv.readenv ppf (Before_compile ""); (*FIXME*)
#else
  Compenv.readenv ppf Before_compile; 
#end  
  Compmisc.init_path  false;
  begin match main_file with       
    | Bsc_task_none ->  0
    | Bsc_task_eval s ->
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




