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

(** Need sync up with {!Main_args} and {!Optmain} *)
open Clflags
open Compenv

let mk_absname f =
  "-absname", Arg.Unit f, " Show absolute filenames in error messages"
;;

let mk_annot f =
  "-annot", Arg.Unit f, " Save information in <filename>.annot"
;;

let mk_binannot f =
  "-bin-annot", Arg.Unit f, " Save typedtree in <filename>.cmt"
;;

let mk_c f =
  "-c", Arg.Unit f, " Compile only (do not link)"
;;

let mk_config f =
  "-config", Arg.Unit f, " Print configuration values and exit"
;;

let mk_g_byt f =
  "-g", Arg.Unit f, " Save debugging information"
;;

let mk_i f =
  "-i", Arg.Unit f, " Print inferred interface"
;;

let mk_I f =
  "-I", Arg.String f, "<dir>  Add <dir> to the list of include directories"
;;

let mk_impl f =
  "-impl", Arg.String f, "<file>  Compile <file> as a .ml file"
;;

let mk_intf f =
  "-intf", Arg.String f, "<file>  Compile <file> as a .mli file"
;;

let mk_intf_suffix f =
  "-intf-suffix", Arg.String f,
  "<string>  Suffix for interface files (default: .mli)"
;;

let mk_keep_docs f =
  "-keep-docs", Arg.Unit f, " Keep documentation strings in .cmi files"
;;

let mk_keep_locs f =
  "-keep-locs", Arg.Unit f, " Keep locations in .cmi files"
;;

let mk_labels f =
  "-labels", Arg.Unit f, " Use commuting label mode"
;;

let mk_no_alias_deps f =
  "-no-alias-deps", Arg.Unit f,
  " Do not record dependencies for module aliases"
;;

let mk_no_app_funct f =
  "-no-app-funct", Arg.Unit f, " Deactivate applicative functors"
;;

let mk_no_check_prims f =
  "-no-check-prims", Arg.Unit f, " Do not check runtime for primitives"
;;

let mk_noassert f =
  "-noassert", Arg.Unit f, " Do not compile assertion checks"
;;

let mk_nolabels f =
  "-nolabels", Arg.Unit f, " Ignore non-optional labels in types"
;;

let mk_nostdlib f =
  "-nostdlib", Arg.Unit f,
  " Do not add default directory to the list of include directories"
;;

let mk_o f =
  "-o", Arg.String f, "<file>  Set output file name to <file>"
;;

let mk_open f =
  "-open", Arg.String f, "<module>  Opens the module <module> before typing"

let mk_pp f =
  "-pp", Arg.String f, "<command>  Pipe sources through preprocessor <command>"
;;

let mk_ppx f =
  "-ppx", Arg.String f,
  "<command>  Pipe abstract syntax trees through preprocessor <command>"
;;

let mk_principal f =
  "-principal", Arg.Unit f, " Check principality of type inference"
;;

let mk_rectypes f =
  "-rectypes", Arg.Unit f, " Allow arbitrary recursive types"
;;

let mk_safe_string f =
  "-safe-string", Arg.Unit f, " Make strings immutable"
;;

let mk_short_paths f =
  "-short-paths", Arg.Unit f, " Shorten paths in types"
;;

let mk_stdin f =
  "-stdin", Arg.Unit f, " Read script from standard input"
;;

let mk_strict_sequence f =
  "-strict-sequence", Arg.Unit f,
  " Left-hand part of a sequence must have type unit"
;;

let mk_unsafe f =
  "-unsafe", Arg.Unit f,
  " Do not compile bounds checking on array and string access"
;;

let mk_v f =
  "-v", Arg.Unit f,
  " Print compiler version and location of standard library and exit"
;;

let mk_verbose f =
  "-verbose", Arg.Unit f, " Print calls to external commands"
;;

let mk_version f =
  "-version", Arg.Unit f, " Print version and exit"
;;

let mk_vnum f =
  "-vnum", Arg.Unit f, " Print version number and exit"
;;

let mk_w f =
  "-w", Arg.String f,
  Printf.sprintf
  "<list>  Enable or disable warnings according to <list>:\n\
  \        +<spec>   enable warnings in <spec>\n\
  \        -<spec>   disable warnings in <spec>\n\
  \        @<spec>   enable warnings in <spec> and treat them as errors\n\
  \     <spec> can be:\n\
  \        <num>             a single warning number\n\
  \        <num1>..<num2>    a range of consecutive warning numbers\n\
  \        <letter>          a predefined set\n\
  \     default setting is %S" Warnings.defaults_w
;;

let mk_warn_error f =
  "-warn-error", Arg.String f,
  Printf.sprintf
  "<list>  Enable or disable error status for warnings according\n\
  \     to <list>.  See option -w for the syntax of <list>.\n\
  \     Default setting is %S" Warnings.defaults_warn_error
;;

let mk_warn_help f =
  "-warn-help", Arg.Unit f, " Show description of warning numbers"
;;

let mk_where f =
  "-where", Arg.Unit f, " Print location of standard library and exit"
;;

let mk_color f =
  "-color", Arg.Symbol (["auto"; "always"; "never"], f),
  Printf.sprintf
  "  Enable or disable colors in compiler messages\n\
  \    The following settings are supported:\n\
  \      auto    use heuristics to enable colors only if supported\n\
  \      always  enable colors\n\
  \      never   disable colors\n\
  \    The default setting is 'auto', and the current heuristic\n\
  \    checks that the TERM environment variable exists and is\n\
  \    not empty or \"dumb\", and that isatty(stderr) holds."
;;


let mk_nopervasives f =
  "-nopervasives", Arg.Unit f, " (undocumented)"
;;

let mk_dparsetree f =
  "-dparsetree", Arg.Unit f, " (undocumented)"
;;

let mk_dtypedtree f =
  "-dtypedtree", Arg.Unit f, " (undocumented)"
;;

let mk_drawlambda f =
  "-drawlambda", Arg.Unit f, " (undocumented)"
;;

let mk_dsource f =
  "-dsource", Arg.Unit f, " (undocumented)"
;;

let mk_dlambda f =
  "-dlambda", Arg.Unit f, " (undocumented)"
;;

let mk_opaque f =
  "-opaque", Arg.Unit f,
  " Does not generate cross-module optimization information\n\
  \     (reduces necessary recompilation on module change)"
;;

let mk_strict_formats f =
  "-strict-formats", Arg.Unit f,
  " Reject invalid formats accepted by legacy implementations\n\
  \     (Warning: Invalid formats may behave differently from\n\
  \      previous OCaml versions, and will become always-rejected\n\
  \      in future OCaml versions. You should use this flag\n\
  \      to detect and fix invalid formats.)"
;;

let mk__ f =
  "-", Arg.String f,
  "<file>  Treat <file> as a file name (even if it starts with `-')"
;;

let show_config () =
  Config.print_config stdout;
  exit 0;
;;

let bs_version_string = 
  "BuckleScript " ^ Bs_version.version ^
  " (Using OCaml" ^ Config.version ^ " )" 

let print_version_string () = 
  print_string bs_version_string;
  print_newline (); 
  exit 0 

  
let print_standard_library () = 
  print_string Bs_conditional_initial.standard_library; print_newline(); exit 0

let print_version_and_library compiler =
  Printf.printf "The OCaml %s, version " compiler;
  print_string bs_version_string; print_newline();
  print_string "Standard library directory: ";
  print_string Bs_conditional_initial.standard_library; print_newline();
  exit 0 

let ocaml_options = 
  let set r () = r := true in 
  let unset r () = r := false in 
  let _absname = set Location.absname in 
  let _color option = 
    match Clflags.parse_color_setting option with
    | None -> ()
    | Some setting -> Clflags.color := setting in 
  let _annot = set annotations in 
  let _binannot = set binary_annotations in 
  let _c = set compile_only in 
  let _config = show_config in 
  let _g = set debug in 
  let _i () = print_types := true; compile_only := true in 
  let _I s = include_dirs := s :: !include_dirs in 
  (* let _impl = impl in  *)
  (* let _intf = intf in  *)
  let _intf_suffix s = Config.interface_suffix := s in 
  let _keep_docs = set keep_docs in 
  let _keep_locs = set keep_locs in 
  let _labels = unset classic in 
  let _no_alias_deps = set transparent_modules in 
  let _no_app_funct = unset applicative_functors in 
  let _noassert = set noassert in 
  let _nolabels = set classic in 
  let _nostdlib = set no_std_include in 
  let _o s = output_name := Some s in 
  let _open s = open_modules := s :: !open_modules in 
  let _pp s = preprocessor := Some s in 
  let _ppx s = first_ppx := s :: !first_ppx in 
  let _principal = set principal in 
  let _rectypes = set recursive_types in 
  let _safe_string = unset unsafe_string in 
  let _short_paths = unset real_paths in 
  let _strict_sequence = set strict_sequence in 
  let _strict_formats = set strict_formats in 
  let _unsafe = set fast in 
  let _unsafe_string = set unsafe_string in 
  let _v () = print_version_and_library "compiler" in 
  let _version = print_version_string in 
  let _vnum = print_version_string in 
  let _w = (Warnings.parse_options false) in
  let _warn_error = (Warnings.parse_options true) in
  let _warn_help = Warnings.help_warnings in
  let _where = print_standard_library in 
  let _verbose = set verbose in 
  let _nopervasives = set nopervasives in
  let _dsource = set dump_source in 
  let _dparsetree = set dump_parsetree in 
  let _dtypedtree = set dump_typedtree in
  let _drawlambda = set dump_rawlambda in
  let _dlambda = set dump_lambda in
  (* let anonymous = anonymous in *)

  [ mk_absname _absname;
    mk_annot _annot;
    mk_binannot _binannot;
    mk_c _c;
    mk_config _config;
    mk_g_byt _g;
    mk_i _i;
    mk_I _I;
    mk_color _color;
    (* mk_impl _impl; *)
    (* mk_intf _intf; *)
    mk_intf_suffix _intf_suffix;
    mk_keep_docs _keep_docs;
    mk_keep_locs _keep_locs;
    mk_labels _labels;
    mk_no_alias_deps _no_alias_deps;
    mk_no_app_funct _no_app_funct;
    mk_noassert _noassert;
    mk_nolabels _nolabels;
    mk_nostdlib _nostdlib;
    mk_o _o;
    mk_open _open;
    mk_pp _pp;
    mk_ppx _ppx;
    mk_principal _principal;
    mk_rectypes _rectypes;
    mk_safe_string _safe_string;
    mk_short_paths _short_paths;
    mk_strict_sequence _strict_sequence;
    mk_strict_formats _strict_formats;
    mk_unsafe _unsafe;
    mk_v _v;
    mk_verbose _verbose;
    mk_version _version;
    mk_vnum _vnum;
    mk_w _w;
    mk_warn_error _warn_error;
    mk_warn_help _warn_help;
    mk_where _where;
    mk_color _color;
    (* mk__ anonymous; *)
    mk_nopervasives _nopervasives;
    mk_dsource _dsource;
    mk_dparsetree _dparsetree;
    mk_dtypedtree _dtypedtree;
    mk_drawlambda _drawlambda;
    mk_dlambda _dlambda ]

