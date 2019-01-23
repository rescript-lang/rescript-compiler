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


let files = "files"
let version = "version"
let name = "name"
(* let ocaml_config = "ocaml-config" *)
let bsdep = "bsdep"
let ppx_flags = "ppx-flags"
let pp_flags = "pp-flags"
let bsc = "bsc"
let refmt = "refmt"
let refmt_flags = "refmt-flags"
let bs_external_includes = "bs-external-includes"
let bs_lib_dir = "bs-lib-dir"
let bs_dependencies = "bs-dependencies"
let bs_dev_dependencies = "bs-dev-dependencies"


let sources = "sources"
let dir = "dir"
let files = "files"
let subdirs = "subdirs"
let bsc_flags = "bsc-flags"
let excludes = "excludes"
let slow_re = "slow-re"
let resources = "resources"
let public = "public"
let js_post_build = "js-post-build"
let cmd = "cmd"
let ninja = "ninja"
let package_specs = "package-specs"

let generate_merlin = "generate-merlin"

let type_ = "type"
let dev = "dev"

let export_all = "all"
let export_none = "none"

let bsb_dir_group = "bsb_dir_group"
let bsc_lib_includes = "bsc_lib_includes"
let use_stdlib = "use-stdlib"
let reason = "reason"
let react_jsx = "react-jsx"

let entries = "entries"
(* @Deprecated This is now called "backend" *)
let kind = "kind"
(* @Deprecated This is now called "main-module" *)
let main = "main"
let cut_generators = "cut-generators"
let generators = "generators"
let command = "command"
let edge = "edge"
let namespace = "namespace"
let in_source = "in-source"
let warnings = "warnings"
let number = "number"
let error = "error"
let suffix = "suffix"

let main_module = "main-module"
let backend = "backend"
let static_libraries = "static-libraries"
let c_linker_flags = "c-linker-flags"
let build_script = "build-script"
let allowed_build_kinds = "allowed-build-kinds"
let ocamlfind_dependencies = "ocamlfind-dependencies"
let ocaml_flags = "ocaml-flags"
let ocaml_linker_flags = "ocaml-linker-flags"
let ocaml_dependencies = "ocaml-dependencies"
let output_name = "output-name"
let ppx = "ppx"
let ocaml_lib_includes = "ocaml_lib_includes"
