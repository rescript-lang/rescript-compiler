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


let setup_reason_context () =
  Js_config.is_reason := true;
  Clflags.preprocessor := None;
  (* FIX #3988*)
  Lazy.force Super_main.setup;
  Lazy.force Reason_outcome_printer_main.setup


let reason_pp ~sourcefile =
  setup_reason_context ();
  Ast_reason_pp.pp sourcefile


type valid_input =
  | Ml
  | Mli
  | Re
  | Rei
  | Mlast
  | Mliast
  | Reast
  | Reiast
  | Mlmap
  | Cmi

(** This is per-file based, when [ocamlc] [-c -o another_dir/xx.cmi] it will
    return (another_dir/xx) *)

let process_file ppf sourcefile =
  (* This is a better default then "", it will be changed later The
     {!Location.input_name} relies on that we write the binary ast properly *)
  Location.set_input_name sourcefile;
  let ext = Ext_filename.get_extension_maybe sourcefile in
  let input =
    if ext = Literals.suffix_ml then Ml
    else if ext = Literals.suffix_re then Re
    else if ext = !Config.interface_suffix then Mli
    else if ext = Literals.suffix_rei then Rei
    else if ext = Literals.suffix_mlast then Mlast
    else if ext = Literals.suffix_mliast then Mliast
    else if ext = Literals.suffix_reast then Reast
    else if ext = Literals.suffix_reiast then Reiast
    else if ext = Literals.suffix_mlmap then Mlmap
    else if ext = Literals.suffix_cmi then Cmi
    else raise (Arg.Bad ("don't know what to do with " ^ sourcefile))
  in
  let opref = Compenv.output_prefix sourcefile in
  match input with
  | Re ->
      setup_reason_context ();
      let tmpfile = reason_pp ~sourcefile in
      Js_implementation.implementation ppf tmpfile opref;
      Ast_reason_pp.clean tmpfile
  | Rei ->
      setup_reason_context ();
      let tmpfile = reason_pp ~sourcefile in
      Js_implementation.interface ppf tmpfile opref;
      Ast_reason_pp.clean tmpfile
  | Reiast ->
      setup_reason_context ();
      Js_implementation.interface_mliast ppf sourcefile opref
  | Reast ->
      setup_reason_context ();
      Js_implementation.implementation_mlast ppf sourcefile opref
  | Ml -> Js_implementation.implementation ppf sourcefile opref
  | Mli -> Js_implementation.interface ppf sourcefile opref
  | Mliast -> Js_implementation.interface_mliast ppf sourcefile opref
  | Mlast -> Js_implementation.implementation_mlast ppf sourcefile opref
  | Mlmap -> Js_implementation.implementation_map ppf sourcefile opref
  | Cmi ->
      let cmi_sign = (Cmi_format.read_cmi sourcefile).cmi_sign in
      Printtyp.signature Format.std_formatter cmi_sign;
      Format.pp_print_newline Format.std_formatter ()


let usage = "Usage: bsc <options> <files>\nOptions are:"

let ppf = Format.err_formatter

(* Error messages to standard error formatter *)
let anonymous filename =
  Compenv.readenv ppf (Before_compile filename);
  process_file ppf filename


let impl filename =
  Compenv.readenv ppf (Before_compile filename);
  process_implementation_file ppf filename


let intf filename =
  Compenv.readenv ppf (Before_compile filename);
  process_interface_file ppf filename


let eval (s : string) ~suffix =
  let tmpfile = Filename.temp_file "eval" suffix in
  Ext_io.write_file tmpfile s;
  anonymous tmpfile;
  Ast_reason_pp.clean tmpfile


let ( // ) = Filename.concat

let define_variable s =
  match Ext_string.split ~keep_empty:true s '=' with
  | [ key; v ] ->
      if not @@ Lexer.define_key_value key v then
        raise (Arg.Bad ("illegal definition: " ^ s))
  | _ -> raise (Arg.Bad ("illegal definition: " ^ s))


let buckle_script_flags : (string * Arg.spec * string) list =
  ( "-bs-super-errors",
    Arg.Unit
      (* needs to be set here instead of, say, setting a Js_config.better_errors
         flag; otherwise, when `anonymous` runs, we don't have time to set the
         custom printer before it starts outputting warnings *)
      (fun _ -> Lazy.force Super_main.setup),
    " Better error message combined with other tools " )
  :: ( "-bs-re-out",
       Arg.Unit (fun _ -> Lazy.force Reason_outcome_printer_main.setup),
       " Print compiler output in Reason syntax" )
  :: ( "-bs-jsx",
       Arg.Int (fun i -> Js_config.jsx_version := i),
       " Set jsx version" )
  :: ( "-bs-refmt",
       Arg.String (fun s -> Js_config.refmt := Some s),
       " Set customized refmt path" )
  :: ( "-bs-gentype",
       Arg.String (fun s -> Clflags.bs_gentype := Some s),
       " Pass gentype command" )
  :: ( "-bs-suffix",
       Arg.Unit Js_package_info.deprecated_set_bs_extension,
       " (DEPRECATED) Set default suffix to .bs.js - use third compoment of \
        -bs-package-output instead" )
  :: ( "-bs-no-implicit-include",
       Arg.Set Clflags.no_implicit_current_dir,
       " Don't include current dir implicitly" )
  :: ( "-bs-read-cmi",
       Arg.Unit (fun _ -> Clflags.assume_no_mli := Clflags.Mli_exists),
       " (internal) Assume mli always exist " )
  :: ( "-bs-D",
       Arg.String define_variable,
       " Define conditional variable e.g, -D DEBUG=true" )
  :: ("-bs-quiet", Arg.Set Clflags.bs_quiet, " Quiet mode (no warnings printed)")
  :: ( "-bs-list-conditionals",
       Arg.Unit (fun () -> Lexer.list_variables Format.err_formatter),
       " List existing conditional variables" )
  :: ( "-bs-binary-ast",
       Arg.Set Js_config.binary_ast,
       " Generate binary .mli_ast and ml_ast" )
  :: ( "-bs-simple-binary-ast",
       Arg.Set Js_config.simple_binary_ast,
       " Generate binary .mliast_simple and mlast_simple" )
  :: ("-bs-syntax-only", Arg.Set Js_config.syntax_only, " only check syntax")
  :: ( "-bs-no-bin-annot",
       Arg.Clear Clflags.binary_annotations,
       " disable binary annotations (by default on)" )
  :: ( "-bs-eval",
       Arg.String (fun s -> eval s ~suffix:Literals.suffix_ml),
       " (experimental) Set the string to be evaluated in OCaml syntax" )
  :: ( "-e",
       Arg.String (fun s -> eval s ~suffix:Literals.suffix_re),
       " (experimental) Set the string to be evaluated in ReasonML syntax" )
  :: ( "-bs-cmi-only",
       Arg.Set Js_config.cmi_only,
       " Stop after generating cmi file" )
  :: ( "-bs-cmi",
       Arg.Set Js_config.force_cmi,
       " Not using cached cmi, always generate cmi" )
  :: ( "-bs-cmj",
       Arg.Set Js_config.force_cmj,
       " Not using cached cmj, always generate cmj" )
  :: ( "-bs-g",
       Arg.Unit
         (fun _ ->
           Js_config.debug := true;
           Lexer.replace_directive_bool "DEBUG" true),
       " debug mode" )
  :: ( "-bs-sort-imports",
       Arg.Set Js_config.sort_imports,
       " Sort the imports by lexical order so the output will be more stable \
        (default false)" )
  :: ( "-bs-no-sort-imports",
       Arg.Clear Js_config.sort_imports,
       " No sort (see -bs-sort-imports)" )
  :: ( "-bs-package-name",
       Arg.String Js_current_package_info.set_package_name,
       " set package name, useful when you want to produce npm packages" )
  :: ( "-bs-ns",
       Arg.String Js_current_package_info.set_package_map,
       " set package map, not only set package name but also use it as a \
        namespace" )
  :: ( "-bs-no-version-header",
       Arg.Set Js_config.no_version_header,
       " Don't print version header" )
  :: ( "-bs-package-output",
       Arg.String Js_current_package_info.append_location_descriptor_of_string,
       " set npm-output-path: [opt_module]:path:[ext], for example: 'lib/cjs', \
        'amdjs:lib/amdjs', 'es6:lib/es6:mjs' " )
  :: ( "-bs-no-warn-unimplemented-external",
       Arg.Set Js_config.no_warn_unimplemented_external,
       " disable warnings on unimplmented c externals" )
  :: ( "-bs-no-builtin-ppx-ml",
       Arg.Set Js_config.no_builtin_ppx_ml,
       "disable built-in ppx for ml files (internal use)" )
  :: ( "-bs-no-builtin-ppx-mli",
       Arg.Set Js_config.no_builtin_ppx_mli,
       "disable built-in ppx for mli files (internal use)" )
  :: ( "-bs-cross-module-opt",
       Arg.Set Js_config.cross_module_inline,
       "enable cross module inlining(experimental), default(false)" )
  :: ("-bs-diagnose", Arg.Set Js_config.diagnose, " More verbose output")
  :: ( "-bs-no-check-div-by-zero",
       Arg.Clear Js_config.check_div_by_zero,
       " unsafe mode, don't check div by zero and mod by zero" )
  :: ( "-bs-noassertfalse",
       Arg.Set Clflags.no_assert_false,
       " no code for assert false" )
  :: ( "-bs-loc",
       Arg.Set Clflags.dump_location,
       " dont display location with -dtypedtree, -dparsetree" )
  :: Ocaml_options.mk_impl (fun file ->
         Js_config.js_stdout := false;
         impl file)
  :: Ocaml_options.mk_intf (fun file ->
         Js_config.js_stdout := false;
         intf file)
  :: Ocaml_options.mk__ anonymous
  :: Ocaml_options.ocaml_options


let _ =
  (* ( print_endline ("BSB_PROJECT_ROOT :" ^ match Sys.getenv_opt
     "BSB_PROJECT_ROOT" with | None -> "None" | Some s -> s )); *)
  Bs_conditional_initial.setup_env ();
  try
    Compenv.readenv ppf Before_args;
    Arg.parse buckle_script_flags anonymous usage
  with x ->
    Ext_obj.bt ();
    Location.report_exception ppf x;
    exit 2
