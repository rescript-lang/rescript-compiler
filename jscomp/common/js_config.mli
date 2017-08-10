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


type module_system = 
  | NodeJS 
  | AmdJS
  | Goog  (* This will be serliazed *)
  | Es6
  | Es6_global
  | AmdJS_global

type package_info = 
 (module_system * string )

type package_name  = string
type packages_info =
  | Empty 
  | NonBrowser of (package_name * package_info  list)

val dump_packages_info : 
  Format.formatter -> packages_info -> unit

val cmj_ext : string 


(* val is_browser : unit -> bool  *)
(* val set_browser : unit -> unit *)


(*val get_ext : unit -> string*)

(** depends on [package_infos], used in {!Js_program_loader} *)
val get_output_dir : pkg_dir:string -> module_system -> string -> string


(** used by command line option *)
val set_npm_package_path : string -> unit 
val get_packages_info : unit -> packages_info

type info_query = 
  | Empty 
  | Package_script of string
  | Found of package_name * string
  | NotFound 
  

val query_package_infos : 
  packages_info ->
  module_system ->
  info_query



(** set/get header *)
val no_version_header : bool ref 


(** return [package_name] and [path] 
    when in script mode: 
*)

val get_current_package_name_and_path : 
  module_system -> info_query


val set_package_name : string -> unit 
val get_package_name : unit -> string option

(** corss module inline option *)
val cross_module_inline : bool ref
val set_cross_module_inline : bool -> unit
val get_cross_module_inline : unit -> bool
  
(** diagnose option *)
val diagnose : bool ref 
val get_diagnose : unit -> bool 
val set_diagnose : bool -> unit 


(** generate tds option *)
val default_gen_tds : bool ref

(** options for builtion ppx *)
val no_builtin_ppx_ml : bool ref 
val no_builtin_ppx_mli : bool ref 
val no_warn_ffi_type : bool ref 
val no_warn_unused_bs_attribute : bool ref 
val no_error_unused_bs_attribute : bool ref 
(** check-div-by-zero option *)
val check_div_by_zero : bool ref 
val get_check_div_by_zero : unit -> bool 

(* It will imply [-noassert] be set too, note from the implmentation point of view, 
   in the lambda layer, it is impossible to tell whehther it is [assert (3 <> 2)] or 
   [if (3<>2) then assert false]
 *)
val no_any_assert : bool ref 
val set_no_any_assert : unit -> unit
val get_no_any_assert : unit -> bool 


val block : string
val int32 : string
val gc : string 
val backtrace : string

val builtin_exceptions : string
val exceptions : string
val io : string
val oo : string
val sys : string
val lexer : string 
val parser : string
val obj_runtime : string
val array : string
val format : string
val string : string
val bytes : string  
val float : string 
val curry : string 
val caml_oo_curry : string 
(* val bigarray : string *)
(* val unix : string *)
val int64 : string
val md5 : string
val hash : string
val weak : string
val js_primitive : string
val module_ : string
val missing_polyfill : string
val exn : string
(** Debugging utilies *)
val set_current_file : string -> unit 
val get_current_file : unit -> string
val get_module_name : unit -> string

val iset_debug_file : string -> unit
val set_debug_file : string -> unit
val get_debug_file : unit -> string

val is_same_file : unit -> bool 

val tool_name : string


val sort_imports : bool ref 
val dump_js : bool ref
val syntax_only  : bool ref
val binary_ast : bool ref


