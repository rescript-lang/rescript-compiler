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


type info =  string list  


val zero : info

#if BS_NATIVE then
val separate_ppx_entries_and_filter :
  Bsb_config_types.entries_t list ->
  Bsb_config_types.compilation_kind_t ->
  (Bsb_config_types.entries_t list * Bsb_config_types.entries_t list)

val get_local_ppx_deps : 
  ppx_list:string list -> 
  root_project_dir:string -> 
  backend:Bsb_config_types.compilation_kind_t -> 
  dependency_info:Bsb_dependency_info.t -> 
  ppx_entries:Bsb_config_types.entries_t list -> 
  (string list * string list)
#end

val handle_file_groups :
  out_channel ->
  package_specs:Bsb_package_specs.t ->  
  bs_suffix:bool ->
  js_post_build_cmd:string option -> 
#if BS_NATIVE then
  backend:Bsb_config_types.compilation_kind_t -> 
  entries:Bsb_config_types.entries_t list ->
  dependency_info:Bsb_dependency_info.t -> 
  root_project_dir:string ->
  is_top_level:bool ->
  ppx_flags_internal:string list ->
#end
  files_to_install:String_hash_set.t ->  
  custom_rules:Bsb_rule.t String_map.t ->
  Bsb_file_groups.file_groups ->
  string option -> 
  info -> info
