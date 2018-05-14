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
  | Ml, opref ->
    Js_implementation.implementation ppf name opref 
  | Mli, opref -> 
    Js_implementation.interface ppf name opref 
  | Mliast, opref 
    -> Js_implementation.interface_mliast ppf name opref 
  | Mlast, opref 
    -> Js_implementation.implementation_mlast ppf name opref
  | Mlmap, opref 
    -> Js_implementation.implementation_map ppf name opref
  | Cmi, _ 
    ->
      let {Cmi_format.cmi_sign } =  Cmi_format.read_cmi name in 
      Printtyp.signature Format.std_formatter cmi_sign ; 
      Format.pp_print_newline Format.std_formatter ()
      

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
let script_dirs = ref []
let main_file  = ref ""
let eval_string = ref ""
        
let collect_file name = 
  batch_files := name :: !batch_files
let add_bs_dir v = 
  script_dirs := v :: !script_dirs

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




                       
let define_variable s =
  match Ext_string.split ~keep_empty:true s '=' with
  | [key; v] -> 
    if not @@ Lexer.define_key_value key v  then 
      raise (Arg.Bad ("illegal definition: " ^ s))
  | _ -> raise (Arg.Bad ("illegal definition: " ^ s))

  
let buckle_script_flags : (string * Arg.spec * string) list =
  ("-bs-super-errors",
    Arg.Unit 
      (* needs to be set here instead of, say, setting a
        Js_config.better_errors flag; otherwise, when `anonymous` runs, we
        don't have time to set the custom printer before it starts outputting
        warnings *)
      Super_main.setup
     ,
   " Better error message combined with other tools "
  )
  :: 
  ("-bs-re-out",
    Arg.Unit Reason_outcome_printer_main.setup,
   " Print compiler output in Reason syntax"
  )
  ::
  ("-bs-suffix",
    Arg.Set Js_config.bs_suffix,
    " Set suffix to .bs.js"
  )  
  :: 
  ("-bs-no-implicit-include", Arg.Set Clflags.no_implicit_current_dir
  , " Don't include current dir implicitly")
  ::
  ("-bs-assume-has-mli", Arg.Unit (fun _ -> Clflags.assume_no_mli := Clflags.Mli_exists), 
    " (internal) Assume mli always exist ")
  ::
  ("-bs-assume-no-mli", Arg.Unit (fun _ -> Clflags.assume_no_mli := Clflags.Mli_non_exists),
  " (internal) Don't lookup whether mli exist or not")
  ::
  ("-bs-D", Arg.String define_variable,
     " Define conditional variable e.g, -D DEBUG=true"
  )
  ::
  ("-bs-list-conditionals",
   Arg.Unit (fun () -> Lexer.list_variables Format.err_formatter),
   " List existing conditional variables")
  ::
  (
    "-bs-binary-ast", Arg.Set Js_config.binary_ast,
    " Generate binary .mli_ast and ml_ast"
  )
  ::
  ("-bs-syntax-only", 
   Arg.Set Js_config.syntax_only,
   " only check syntax"
  )
  ::
  ("-bs-no-bin-annot", Arg.Clear Clflags.binary_annotations, 
   " disable binary annotations (by default on)")
  ::
  ("-bs-eval", 
   Arg.String set_eval_string, 
   " (experimental) Set the string to be evaluated, note this flag will be conflicted with -bs-main"
  )
  ::
  ("-bs-g",
    Arg.Unit 
    (fun _ -> Js_config.debug := true;
      Lexer.replace_directive_bool "DEBUG" true
    ),
    " debug mode"
  )
  ::
  (
    "-bs-sort-imports",
    Arg.Set Js_config.sort_imports,
    " Sort the imports by lexical order so the output will be more stable (default false)"
  )
  ::
  ( "-bs-no-sort-imports", 
    Arg.Clear Js_config.sort_imports,
    " No sort (see -bs-sort-imports)"
  )
  ::
  ("-bs-package-name", 
   Arg.String Js_packages_state.set_package_name, 
   " set package name, useful when you want to produce npm packages")
  ::
  ( "-bs-package-map", 
   Arg.String Js_packages_state.set_package_map, 
   " set package map, not only set package name but also use it as a namespace"    
  )
  :: 
  ("-bs-no-version-header", 
   Arg.Set Js_config.no_version_header,
   " Don't print version header"
  )
  ::
  ("-bs-package-output", 
   Arg.String 
    Js_packages_state.update_npm_package_path, 
   " set npm-output-path: [opt_module]:path, for example: 'lib/cjs', 'amdjs:lib/amdjs', 'es6:lib/es6' ")
  ::
  ("-bs-no-warn-unimplemented-external",
    Arg.Set Js_config.no_warn_unimplemented_external,
    " disable warnings on unimplmented c externals"
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
  ("-bs-diagnose",
   Arg.Set Js_config.diagnose, 
   " More verbose output")
  :: 
  ("-bs-no-check-div-by-zero",
   Arg.Clear Js_config.check_div_by_zero, 
   " unsafe mode, don't check div by zero and mod by zero")
  ::
  ("-bs-noassertfalse",
    Arg.Set Clflags.no_assert_false,
    " no code for assert false"
  )
  ::
  ("-bs-main",
   Arg.String set_main_entry,   
   " set the Main entry module in script mode, for example -bs-main Main")
  ::
  ("-bs-I", 
   Arg.String add_bs_dir, 
   " add source dir search path in script mode"
  )
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
  (* Default configuration: sync up with 
    {!Jsoo_main}  *)
  Clflags.bs_only := true;  
  Clflags.unsafe_string := false;
  Clflags.debug := true;
  Clflags.record_event_when_debug := false;
  Clflags.binary_annotations := true; 
  Oprint.out_ident := Outcome_printer_ns.out_ident;
  Bs_conditional_initial.setup_env ();
  try
    Compenv.readenv ppf Before_args;
    Arg.parse buckle_script_flags anonymous usage;
    let main_file = !main_file in
    let eval_string = !eval_string in
    let task : Ocaml_batch_compile.task = 
      if main_file <> "" then 
        Bsc_task_main main_file
      else if eval_string <> "" then 
        Bsc_task_eval eval_string
      else Bsc_task_none in
    exit (Ocaml_batch_compile.batch_compile ppf 
            (if !Clflags.no_implicit_current_dir then !script_dirs else 
               Filename.current_dir_name::!script_dirs) !batch_files task) 
  with x -> 
    begin
      Location.report_exception ppf x;
      exit 2
    end
