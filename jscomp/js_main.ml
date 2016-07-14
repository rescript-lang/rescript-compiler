(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


let process_interface_file ppf name =
  Js_implementation.interface ppf name (Compenv.output_prefix name)
let process_implementation_file ppf name =
  Js_implementation.implementation ppf name (Compenv.output_prefix name)


let process_file ppf name =
  match Ocaml_parse.check_suffix  name with 
  | `Ml, opref ->
    Js_implementation.implementation ppf name opref 
  | `Mli, opref -> 
    Js_implementation.interface ppf name opref 


let usage = "Usage: bsc <options> <files>\nOptions are:"

let ppf = Format.err_formatter

(* Error messages to standard error formatter *)
let anonymous filename =
  Compenv.readenv ppf Before_compile; process_file ppf filename;;
let impl filename =
  Compenv.readenv ppf Before_compile; process_implementation_file ppf filename;;
let intf filename =
  Compenv.readenv ppf Before_compile; process_interface_file ppf filename;;

let batch_files  = ref []

let collect_file name = 
  batch_files := name :: !batch_files







let add_include_path s = 
  let path = 
    Ext_filename.resolve_bs_package
      ~cwd:(Lazy.force Ext_filename.cwd) s   in 
  if Ext_sys.is_directory_no_exn path then 
    Clflags.include_dirs := path :: ! Clflags.include_dirs
  else 
    Ext_pervasives.failwithf ~loc:__LOC__ "%s is not a directory" s 

let set_noassert () = 
  Js_config.set_no_any_assert ();
  Clflags.noassert := true


let buckle_script_flags = 
  ("-bs-package-name", 
   Arg.String Js_config.set_package_name, 
   " set package name, useful when you want to produce npm packages")
  :: 
  ("-bs-no-version-header", 
   Arg.Set Js_config.no_version_header,
   " Don't print version header"
  )
  ::
  ("-bs-package-output", 
   Arg.String Js_config.set_npm_package_path, 
   " set npm-output-path: [opt_module]:path, for example: 'lib/cjs', 'amdjs:lib/amdjs' and 'goog:lib/gjs'")
  ::
  ("-bs-package-include", 
   Arg.String add_include_path, 
   " set package names, for example bs-platform "  )
  :: 
  ("-bs-no-builtin-ppx-ml", 
   Arg.Set Js_config.no_builtin_ppx_ml,
   "disable built-in ppx for ml files (internal use)")
  :: 
  ("-bs-no-builtin-ppx-mli",
   Arg.Set Js_config.no_builtin_ppx_mli,
   "disable built-in ppx for mli files (internal use)")
  :: 
  ("-bs-cross-module-opt", 
   Arg.Set Js_config.cross_module_inline, 
   "enable cross module inlining(experimental), default(false)")
  :: 
  ("-bs-gen-tds",
   Arg.Set Js_config.default_gen_tds, 
   " set will generate `.d.ts` file for typescript (experimental)")
  :: 
  ("-bs-diagnose",
   Arg.Set Js_config.diagnose, 
   " More verbose output")
  :: 
  ("-bs-no-check-div-by-zero",
   Arg.Clear Js_config.check_div_by_zero, 
   " unsafe mode, don't check div by zero and mod by zero")
  ::
  ("-bs-no-any-assert",
   Arg.Unit set_noassert, 
   " no code containing any assertion"
  )
  :: 
  ("-bs-files", 
   Arg.Rest collect_file, 
   " Provide batch of files, the compiler will sort it before compiling"
  )
  :: Ocaml_options.mk_impl impl
  :: Ocaml_options.mk_intf intf 
  :: Ocaml_options.mk__ anonymous
  :: Ocaml_options.ocaml_options




let _ = 
  Clflags.unsafe_string := false;
  Clflags.debug := true;

  try
    Compenv.readenv ppf Before_args;
    Arg.parse buckle_script_flags anonymous usage;
    Ocaml_batch_compile.batch_compile ppf !batch_files; 
    exit 0
  with x ->
    Location.report_exception ppf x;
    exit 2




