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

let parse_interface ppf sourcefile = 
  let ast = Pparse.parse_interface ~tool_name:Js_config.tool_name ppf sourcefile in
  if !Js_config.no_builtin_ppx_mli then ast else  !Ppx_entry.rewrite_signature ast

let lazy_parse_interface ppf sourcefile =
  lazy (parse_interface ppf sourcefile)

let parse_implementation ppf sourcefile = 
  let ast = 
    Pparse.parse_implementation ~tool_name:Js_config.tool_name ppf sourcefile in 
  if !Js_config.no_builtin_ppx_ml then ast else
    !Ppx_entry.rewrite_implementation ast 

let parse_implementation_from_string  str = 
  let lb = Lexing.from_string str in
  Location.init lb "//toplevel//";
  let ast = Parse.implementation lb  in 
  if !Js_config.no_builtin_ppx_ml then ast else 
    !Ppx_entry.rewrite_implementation ast 


let lazy_parse_implementation ppf sourcefile =
  lazy (parse_implementation ppf sourcefile)

type valid_input = 
  | Ml 
  | Mli
  | Mlast    
  | Mliast 
  | Mlmap
  | Cmi
  
let check_suffix  name  = 
  if Ext_path.check_suffix_case name ".ml"
  || Ext_path.check_suffix_case name ".mlt" then 
    Ml,
    (** This is per-file based, 
        when [ocamlc] [-c -o another_dir/xx.cmi] 
        it will return (another_dir/xx)
    *)    
    Compenv.output_prefix name 
  else if Ext_path.check_suffix_case name !Config.interface_suffix then 
    Mli,  Compenv.output_prefix name 
  else if Ext_path.check_suffix_case name ".mlast" then 
    Mlast, Compenv.output_prefix name 
  else if Ext_path.check_suffix_case name ".mliast" then 
    Mliast, Compenv.output_prefix name 
  else if Ext_path.check_suffix_case name ".mlmap"  then 
    Mlmap, Compenv.output_prefix name 
  else if Ext_path.check_suffix_case name ".cmi" then 
    Cmi, Compenv.output_prefix name
  else 
    raise(Arg.Bad("don't know what to do with " ^ name))
