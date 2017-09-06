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
let (//) = Ext_path.combine 

let lib_lit = "lib"
let lib_js = lib_lit //"js"
let lib_amd = lib_lit //"amdjs"
let lib_ocaml = lib_lit // "ocaml"
let lib_bs = lib_lit // "bs"
let lib_es6 = lib_lit // "es6"
let lib_es6_global = lib_lit // "es6_global"
let lib_amd_global = lib_lit // "amdjs_global"
let all_lib_artifacts = 
  [ lib_js ; 
    lib_amd ;
    lib_ocaml;
    lib_bs ; 
    lib_es6 ; 
    lib_es6_global;
    lib_amd_global
  ]
let rev_lib_bs = ".."// ".."


let rev_lib_bs_prefix p = rev_lib_bs // p 

let ocaml_bin_install_prefix p = lib_ocaml // p

let lazy_src_root_dir = "$src_root_dir" 
let proj_rel path = lazy_src_root_dir // path

(** it may not be a bad idea to hard code the binary path 
    of bsb in configuration time
*)






let cmd_package_specs = ref None 

