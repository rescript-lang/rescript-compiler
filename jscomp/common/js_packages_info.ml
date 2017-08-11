(* Copyright (C) 2017 Authors of BuckleScript
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


type path = string

type module_system =
  | NodeJS 
  | AmdJS 
  | Goog
  | Es6
  | Es6_global (* ignore node_modules, just calcluating relative path *)
  | AmdJS_global (* see ^ *)

(* ocamlopt could not optimize such simple case..*)
let compatible (exist : module_system) 
    (query : module_system) =
  match query with 
  | NodeJS -> exist = NodeJS 
  | AmdJS -> exist = AmdJS
  | Goog -> exist = Goog
  | Es6  -> exist = Es6
  | Es6_global  
    -> exist = Es6_global || exist = Es6
  | AmdJS_global 
    -> exist = AmdJS_global || exist = AmdJS
(* As a dependency Leaf Node, it is the same either [global] or [not] *)


type package_info =
  ( module_system * string )

type package_name  = string
type t =
  | Empty (* No set *)
  | NonBrowser of (package_name * package_info  list)
  (* we don't want force people to use package *) 


let string_of_module_system (ms : module_system) = 
  (match ms with 
   | NodeJS -> "NodeJS"
   | AmdJS -> "AmdJS"
   | Goog -> "Goog"
   | Es6 -> "Es6"
   | Es6_global -> "Es6_global"
   | AmdJS_global -> "AmdJS_globl"
  )

let module_system_of_string package_name : module_system option = 
  match package_name with
  | "commonjs" -> Some NodeJS
  | "amdjs" -> Some AmdJS
  | "goog" -> Some Goog
  | "es6" -> Some Es6
  | "es6-global" -> Some Es6_global
  | "amdjs-global" -> Some AmdJS_global 
  | _ -> None 
  
let dump_package_info 
    (fmt : Format.formatter)
    ((ms, name) : package_info)
  = 
  Format.fprintf
    fmt 
    "@[%s:@ %s@]"
    (string_of_module_system ms)
    name 


let dump_packages_info 
    (fmt : Format.formatter) 
    (p : t) = 
  match p with 
  | Empty -> Format.pp_print_string fmt  "<Empty>"
  | NonBrowser (name, ls) ->
    Format.fprintf fmt "@[%s;@ @[%a@]@]"
      name
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_space fmt ())
         dump_package_info 
      ) ls
      
type info_query =
  | Package_empty
  | Package_script of string
  | Package_found of package_name * string
  | Package_not_found 

  

let query_package_infos 
    (package_infos : t) module_system : info_query =
  match package_infos with
  | Empty -> Package_empty
  | NonBrowser (name, []) -> Package_script name
  | NonBrowser (name, paths) ->
    begin match List.find (fun (k, _) -> 
        compatible k  module_system) paths with
    | (_, x) -> Package_found (name, x)
    | exception _ -> Package_not_found
    end
  

(* for a single pass compilation, [output_dir]
   can be cached
*)
let get_output_dir ~pkg_dir module_system output_prefix 
  packages_info =
  match packages_info with
  | Empty | NonBrowser (_, [])->
    if Filename.is_relative output_prefix then
      Filename.concat (Lazy.force Ext_filename.cwd )
      (Filename.dirname output_prefix)
    else
      Filename.dirname output_prefix
  | NonBrowser (_,  modules) ->
    begin match List.find (fun (k,_) -> 
      compatible k  module_system) modules with
      | (_, path) -> Filename.concat pkg_dir  path
      |  exception _ -> assert false
    end

    