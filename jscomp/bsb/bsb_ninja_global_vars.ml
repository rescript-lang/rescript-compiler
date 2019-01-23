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


 
let bs_package_flags = "bs_package_flags"

let bsc = "bsc" 

let src_root_dir = "src_root_dir"
let bsdep = "bsdep"

let bsc_flags = "bsc_flags"

let ppx_flags = "ppx_flags"

let pp_flags = "pp_flags"

let bs_package_includes = "bs_package_includes"

let bs_package_dev_includes = "bs_package_dev_includes"

let refmt = "refmt"

let reason_react_jsx = "reason_react_jsx"

let refmt_flags = "refmt_flags"

let postbuild = "postbuild"

let namespace = "namespace" 


let warnings = "warnings"

#if BS_NATIVE then
let package_sep = "-"
let build_artifacts_dir = "build_artifacts_dir"

let bs_super_errors = "bs_super_errors"
let bs_super_errors_ocamlfind = "bs_super_errors_ocamlfind"

let ocamlc = "ocamlc"
let ocamlopt = "ocamlopt"
let ocamlfind = "ocamlfind"
let ocamlfind_dependencies = "ocamlfind_dependencies"
let external_deps_for_linking = "external_deps_for_linking"
let open_flag = "open_flag"
let ocaml_flags = "ocaml_flags"
let ocaml_linker_flags = "ocaml_linker_flags"
let berror = "berror"
let ocaml_dependencies = "ocaml_dependencies"
let bsb_helper_warnings = "bsb_helper_warnings"
let bsb_helper_verbose = "bsb_helper_verbose"
#end
