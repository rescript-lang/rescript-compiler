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







(** Browser is not set via command line only for internal use *)


let no_version_header = ref false

let cross_module_inline = ref false



let diagnose = ref false
let get_diagnose () = 
  !diagnose
#if undefined BS_RELEASE_BUILD
  || Sys.getenv_opt "RES_DEBUG_FILE" <> None
#end

(* let (//) = Filename.concat *)

(* let get_packages_info () = !packages_info *)

let no_builtin_ppx = ref false




let tool_name = "ReScript"

let check_div_by_zero = ref true
let get_check_div_by_zero () = !check_div_by_zero



let syntax_only = ref false
let binary_ast = ref false



let debug = ref false

let cmi_only = ref false  
let cmj_only = ref false

let force_cmi = ref false
let force_cmj = ref false

let jsx_version = ref (-1)


let js_stdout = ref true

let all_module_aliases = ref false

let no_stdlib = ref false

let no_export = ref false



let as_ppx = ref false



let customize_runtime = ref None 

let as_pp = ref false

let self_stack : string Stack.t = Stack.create ()