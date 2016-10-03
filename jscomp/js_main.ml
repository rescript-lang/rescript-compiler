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
let main_file  = ref ""
let eval_string = ref ""
    
let collect_file name = 
  batch_files := name :: !batch_files


let set_main_entry name =
  if !eval_string <> "" then
    raise (Arg.Bad ("-bs-main conflicts with -bs-eval")) else 
  if Sys.file_exists name then 
    main_file := name else
  raise (Arg.Bad ("file " ^ name ^ " don't exist"))


let set_eval_string s = 
  if !main_file <> "" then 
    raise (Arg.Bad ("-bs-main conflicts with -bs-eval")) else 
  eval_string :=  s 


let (//) = Filename.concat
(** [resolve cwd module_name], 
    [cwd] is current working directory, absolute path
    Trying to find paths to load [module_name]
    it is sepcialized for option [-bs-package-include] which requires
    [npm_package_name/lib/ocaml]
*)
let  resolve_bs_package ~cwd name = 
  let sub_path = name // "lib" // "ocaml" in
  let rec aux origin cwd name = 
    let destdir =  cwd // Literals.node_modules // sub_path in 
    if Ext_sys.is_directory_no_exn destdir then destdir
    else 
      let cwd' = Filename.dirname cwd in 
      if String.length cwd' < String.length cwd then  
        aux origin   cwd' name
      else 
        try 
          let destdir = 
            Sys.getenv "npm_config_prefix" 
            // "lib" // Literals.node_modules // sub_path in
          if Ext_sys.is_directory_no_exn destdir
          then destdir
          else
            Bs_exception.error (Bs_package_not_found name)
        with 
          Not_found ->
          Bs_exception.error (Bs_package_not_found name)          
  in
  aux cwd cwd name

let add_package s = 
  let path = 
    resolve_bs_package
      ~cwd:(Lazy.force Ext_filename.cwd) s   in 
  Clflags.include_dirs := path :: ! Clflags.include_dirs


let set_noassert () = 
  Js_config.set_no_any_assert ();
  Clflags.noassert := true


let buckle_script_flags =
  ("-bs-eval", 
   Arg.String set_eval_string, 
   " (experimental) Set the string to be evaluated, note this flag will be conflicted with -bs-main"
  )
  ::
  (
    "-bs-sort-imports",
    Arg.Set Js_config.sort_imports,
    " Sort the imports by lexical order so the output will be more stable"
  )
  ::
  ("-bs-better-errors",
   Arg.Set Js_config.better_errors,
   " Better error message combined with other tools "
  )::
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
   Arg.String add_package, 
   " set package names, for example bs-platform "  )
  ::
  ("-bs-no-warn-unused-bs-attribute",
   Arg.Set Js_config.no_warn_unused_bs_attribute,
   " disable warnings on unused bs. attribute"
  )
  ::
  ("-bs-no-warn-ffi-type", 
   Arg.Set Js_config.no_warn_ffi_type,
   " disable warnings for ffi type"
  ) 
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
  ("-bs-main",
   Arg.String set_main_entry,   
   " set the Main entry file")
  :: 
  ("-bs-files", 
   Arg.Rest collect_file, 
   " Provide batch of files, the compiler will sort it before compiling"
  )
  (* :: *)
  (* ("-bs-list-directives", *)
  (* ) *)
  :: Ocaml_options.mk_impl impl
  :: Ocaml_options.mk_intf intf 
  :: Ocaml_options.mk__ anonymous
  :: Ocaml_options.ocaml_options




let _ = 
  Clflags.unsafe_string := false;
  Clflags.debug := true;
  Bs_conditional_initial.setup_env ();
  try
    Compenv.readenv ppf Before_args;
    Arg.parse buckle_script_flags anonymous usage;
    let main_file = !main_file in
    let eval_string = !eval_string in
    let task : Ocaml_batch_compile.task = 
      if main_file <> "" then 
        Main main_file
      else if eval_string <> "" then 
        Eval eval_string
      else None in
    exit (Ocaml_batch_compile.batch_compile ppf !batch_files task) 
  with x ->
    if not @@ !Js_config.better_errors then
      begin (* plain error messge reporting*)
        Location.report_exception ppf x;
        exit 2
      end
    else
      (** Fancy error message reporting*)
      exit 2




