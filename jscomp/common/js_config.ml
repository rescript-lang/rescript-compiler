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


type path = string

type module_system =
  | NodeJS 
  | AmdJS 
  | Goog
  | Es6
  | Es6_global (* ignore node_modules, just calcluating relative path *)
  | AmdJS_global (* see ^ *)

type package_info =
 ( module_system * string )

type package_name  = string
type packages_info =
  | Empty (* No set *)
  | NonBrowser of (package_name * package_info  list)
(** we don't force people to use package *)

let dump_package_info 
  (fmt : Format.formatter)
  ((ms, name) : package_info)
  = 
  Format.fprintf
  fmt 
  "@[%s:@ %s@]"
  (match ms with 
  | NodeJS -> "NodeJS"
  | AmdJS -> "AmdJS"
  | Goog -> "Goog"
  | Es6 -> "Es6"
  | Es6_global -> "Es6_global"
  | AmdJS_global -> "AmdJS_globl"
  ) name 

  
let dump_packages_info 
  (fmt : Format.formatter) 
  (p : packages_info) = 
  match p with 
  | Empty -> Format.pp_print_string fmt  "<Empty>"
  | NonBrowser (name, ls) ->
    Format.fprintf fmt "@[%s;@ @[%a@]@]"
      name
      (Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_space fmt ())
       dump_package_info 
      ) ls

let cmj_ext = ".cmj"



(*let get_ext () = !ext*)


let packages_info : packages_info ref = ref Empty


let get_package_name () =
  match !packages_info with
  | Empty  -> None
  | NonBrowser(n,_) -> Some n

let no_version_header = ref false

let set_package_name name =
  match !packages_info with
  | Empty -> packages_info := NonBrowser(name,  [])
  |  _ ->
    Ext_pervasives.bad_argf "duplicated flag for -bs-package-name"


let set_npm_package_path s =
  match !packages_info  with
  | Empty ->
    Ext_pervasives.bad_argf "please set package name first using -bs-package-name ";
  | NonBrowser(name,  envs) ->
    let env, path =
      match Ext_string.split ~keep_empty:false s ':' with
      | [ package_name; path]  ->
        (match package_name with
         | "commonjs" -> NodeJS
         | "amdjs" -> AmdJS
         | "goog" -> Goog
         | "es6" -> Es6
         | "es6-global" -> Es6_global
         | "amdjs-global" -> AmdJS_global
         | _ ->
           Ext_pervasives.bad_argf "invalid module system %s" package_name), path
      | [path] ->
        NodeJS, path
      | _ ->
        Ext_pervasives.bad_argf "invalid npm package path: %s" s
    in
    packages_info := NonBrowser (name,  ((env,path) :: envs))
   (** Browser is not set via command line only for internal use *)




let cross_module_inline = ref false

let get_cross_module_inline () = !cross_module_inline
let set_cross_module_inline b =
  cross_module_inline := b


let diagnose = ref false
let get_diagnose () = !diagnose
let set_diagnose b = diagnose := b

let (//) = Filename.concat

let get_packages_info () = !packages_info

type info_query =
  | Empty
  | Package_script of string
  | Found of package_name * string
  | NotFound 

(* ocamlopt could not optimize such simple case..*)
let compatible exist query =
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

let query_package_infos (package_infos : packages_info) module_system =
  match package_infos with
  | Empty -> Empty
  | NonBrowser (name, []) -> Package_script name
  | NonBrowser (name, paths) ->
    begin match List.find (fun (k, _) -> compatible k  module_system) paths with
      | (_, x) -> Found (name, x)
      | exception _ -> NotFound
    end

let get_current_package_name_and_path   module_system =
  query_package_infos !packages_info module_system


(* for a single pass compilation, [output_dir]
   can be cached
*)
let get_output_dir ~pkg_dir module_system filename =
  match !packages_info with
  | Empty | NonBrowser (_, [])->
    if Filename.is_relative filename then
      Lazy.force Ext_filename.cwd //
      Filename.dirname filename
    else
      Filename.dirname filename
  | NonBrowser (_,  modules) ->
    begin match List.find (fun (k,_) -> compatible k  module_system) modules with
      | (_, _path) -> pkg_dir // _path
      |  exception _ -> assert false
    end




let default_gen_tds = ref false

let no_builtin_ppx_ml = ref false
let no_builtin_ppx_mli = ref false
let no_warn_ffi_type = ref false

(** TODO: will flip the option when it is ready *)
let no_warn_unused_bs_attribute = ref false
let no_error_unused_bs_attribute = ref false 

let builtin_exceptions = "Caml_builtin_exceptions"
let exceptions = "Caml_exceptions"
let io = "Caml_io"
let sys = "Caml_sys"
let lexer = "Caml_lexer"
let parser = "Caml_parser"
let obj_runtime = "Caml_obj"
let array = "Caml_array"
let format = "Caml_format"
let string = "Caml_string"
let bytes = "Caml_bytes"
let float = "Caml_float"
let hash = "Caml_hash"
let oo = "Caml_oo"
let curry = "Curry"
let caml_oo_curry = "Caml_oo_curry"
let int64 = "Caml_int64"
let md5 = "Caml_md5"
let weak = "Caml_weak"
let backtrace = "Caml_backtrace"
let gc = "Caml_gc"
let int32 = "Caml_int32"
let block = "Block"
let js_primitive = "Js_primitive"
let module_ = "Caml_module"
let missing_polyfill = "Caml_missing_polyfill"
let exn = "Js_exn"

let current_file = ref ""
let debug_file = ref ""

let set_current_file f  = current_file := f
let get_current_file () = !current_file
let get_module_name () =
  Filename.chop_extension
    (Filename.basename (String.uncapitalize !current_file))

let iset_debug_file _ = ()
let set_debug_file  f = debug_file := f
let get_debug_file  () = !debug_file


let is_same_file () =
  !debug_file <> "" &&  !debug_file = !current_file

let tool_name = "BuckleScript"

let check_div_by_zero = ref true
let get_check_div_by_zero () = !check_div_by_zero

let no_any_assert = ref false

let set_no_any_assert () = no_any_assert := true
let get_no_any_assert () = !no_any_assert

let sort_imports = ref true
let dump_js = ref false



let syntax_only = ref false
let binary_ast = ref false

