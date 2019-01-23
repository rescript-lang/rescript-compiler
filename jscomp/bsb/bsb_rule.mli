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




type t  

val get_name : t  -> out_channel -> string

val build_ast_and_module_sets : t
(** TODO: Implement it on top of pp_flags *)
val build_ast_and_module_sets_from_re : t 
val build_ast_and_module_sets_from_rei : t 
(** platform dependent, on Win32,
  invoking cmd.exe
 *)
val copy_resources : t



(** Rules below all need restat *)
val build_bin_deps : t 
val build_cmj_js : t
val build_cmj_cmi_js : t 
val build_cmi : t

#if BS_NATIVE then
val build_ast_and_module_sets_gen_simple : t
val build_ast_and_module_sets_from_re_gen_simple : t 
val build_ast_and_module_sets_from_rei_gen_simple : t 
val build_bin_deps_bytecode : t 
val build_bin_deps_native : t 
val reload : t 
val build_cmo_cmi_bytecode : t
val build_cmi_bytecode : t
val build_cmx_cmi_native : t
val build_cmi_native : t
val linking_bytecode : t
val linking_native : t
val build_cma_library : t
val build_cmxa_library : t
#end

val build_package : t 
#if BS_NATIVE then
val build_package_gen_mlast_simple : t
val build_package_build_cmi_bytecode : t
val build_package_build_cmi_native : t
#end

(** rules are generally composed of built-in rules and customized rules, there are two design choices:
    1. respect custom rules with the same name, then we need adjust our built-in 
    rules dynamically in case the conflict.
    2. respect our built-in rules, then we only need re-load custom rules for each bsconfig.json
*)


(** Since now we generate ninja files per bsconfig.json in a single process, 
    we must make sure it is re-entrant
*)
val reset : string String_map.t -> t String_map.t

#if BS_NATIVE then
val define :
    command:string ->
    ?depfile:string ->
    ?restat:unit ->
    ?description:string ->
    string -> 
    t
#end
