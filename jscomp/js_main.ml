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

open Config
open Clflags
(* open Compenv *)

let output_prefix = Compenv.output_prefix
let readenv = Compenv.readenv
let first_ccopts = Compenv.first_ccopts
let first_ppx = Compenv.first_ppx

let bs_version_string = 
  "BuckleScript " ^ Js_config.version ^
  " (Using OCaml" ^ Config.version ^ " )"

let print_version_and_library compiler =
  Printf.printf "The OCaml %s, version " compiler;
  print_string bs_version_string; print_newline();
  print_string "Standard library directory: ";
  print_string Config.standard_library; print_newline();
  exit 0

let print_standard_library = Compenv.print_standard_library

let print_version_string () = 
  print_string bs_version_string;
  print_newline (); 
  exit 0 



let process_interface_file ppf name =
  Js_implementation.interface ppf name (output_prefix name)

let process_implementation_file ppf name =
  let opref = output_prefix name in
  Js_implementation.implementation ppf name opref;
  objfiles := (opref ^ ".cmo") :: !objfiles
let process_file ppf name =
  if Filename.check_suffix name ".ml"
  || Filename.check_suffix name ".mlt" then begin
    let opref = output_prefix name in
    Js_implementation.implementation ppf name opref;
    objfiles := (opref ^ ".cmo") :: !objfiles
  end
  else if Filename.check_suffix name !Config.interface_suffix then begin
    let opref = output_prefix name in
    Js_implementation.interface ppf name opref;
    if !make_package then objfiles := (opref ^ ".cmi") :: !objfiles
  end
  else
    raise(Arg.Bad("don't know what to do with " ^ name))

let usage = "Usage: bsc <options> <files>\nOptions are:"

let ppf = Format.err_formatter

(* Error messages to standard error formatter *)
let anonymous filename =
  readenv ppf Before_compile; process_file ppf filename;;
let impl filename =
  readenv ppf Before_compile; process_implementation_file ppf filename;;
let intf filename =
  readenv ppf Before_compile; process_interface_file ppf filename;;

let show_config () =
  Config.print_config stdout;
  exit 0;
;;

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




let add_include_path s = 
  let (//) = Filename.concat in
  let path = 
    Ext_filename.resolve 
      (Lazy.force Ext_filename.cwd) s // "lib"// "ocaml"  in 
  if Ext_sys.is_directory_no_exn path then 
    Clflags.include_dirs := path :: ! Clflags.include_dirs
  else 
    Ext_pervasives.failwithf "%s is not a directory" s 


let buckle_script_flags = 
  ("-js-npm-output-path", Arg.String Js_config.set_npm_package_path, 
   " set npm-output-path: package-name:path, for example `bs-platform:lib/js`")
  ::
  ("-js-npm-package-include", Arg.String add_include_path, 
   " set package names, for example bs-platform "  )
  :: ("-js-module", Arg.String Js_config.cmd_set_module, 
    " set module system: commonjs (default), amdjs, google:package_name")
  :: ("-js-no-builtin-ppx-ml", Arg.Set Js_config.no_builtin_ppx_ml,
      "disable built-in ppx for ml files (internal use)")
  :: ("-js-no-builtin-ppx-mli", Arg.Set Js_config.no_builtin_ppx_mli,
      "disable built-in ppx for mli files (internal use)")
  :: ("-js-gen-tds", Arg.Set Js_config.default_gen_tds, 
    " set will generate `.d.ts` file for typescript (experimental)")
  :: (
    let module F = struct
      let set r () = r := true
      let unset r () = r := false
      let _a = set make_archive
      let _absname = set Location.absname
      let _annot = set annotations
      let _binannot = set binary_annotations
      let _c = set compile_only
      let _cc s = c_compiler := Some s
      let _cclib s = ccobjs := Misc.rev_split_words s @ !ccobjs
      let _ccopt s = first_ccopts := s :: !first_ccopts
      let _compat_32 = set bytecode_compatible_32
      let _config = show_config
      let _custom = set custom_runtime
      let _no_check_prims = set no_check_prims
      let _dllib s = dllibs := Misc.rev_split_words s @ !dllibs
      let _dllpath s = dllpaths := !dllpaths @ [s]
      let _for_pack s = for_package := Some s
      let _g = set debug
      let _i () = print_types := true; compile_only := true
      let _I s = include_dirs := s :: !include_dirs
      let _impl = impl
      let _intf = intf
      let _intf_suffix s = Config.interface_suffix := s
      let _keep_docs = set keep_docs
      let _keep_locs = set keep_locs
      let _labels = unset classic
      let _linkall = set link_everything
      let _make_runtime () =
        custom_runtime := true; make_runtime := true; link_everything := true
      let _no_alias_deps = set transparent_modules
      let _no_app_funct = unset applicative_functors
      let _noassert = set noassert
      let _nolabels = set classic
      let _noautolink = set no_auto_link
      let _nostdlib = set no_std_include
      let _o s = output_name := Some s
      let _open s = open_modules := s :: !open_modules
      let _output_obj () = output_c_object := true; custom_runtime := true
      let _output_complete_obj () =
        output_c_object := true; output_complete_object := true; custom_runtime := true
      let _pack = set make_package
      let _pp s = preprocessor := Some s
      let _ppx s = first_ppx := s :: !first_ppx
      let _principal = set principal
      let _rectypes = set recursive_types
      let _runtime_variant s = runtime_variant := s
      let _safe_string = unset unsafe_string
      let _short_paths = unset real_paths
      let _strict_sequence = set strict_sequence
      let _strict_formats = set strict_formats
      let _thread = set use_threads
      let _vmthread = set use_vmthreads
      let _unsafe = set fast
      let _unsafe_string = set unsafe_string
      let _use_prims s = use_prims := s
      let _use_runtime s = use_runtime := s
      let _v () = print_version_and_library "compiler"
      let _version = print_version_string
      let _vnum = print_version_string
      let _w = (Warnings.parse_options false)
      let _warn_error = (Warnings.parse_options true)
      let _warn_help = Warnings.help_warnings
      let _where = print_standard_library
      let _verbose = set verbose
      let _nopervasives = set nopervasives
      let _dsource = set dump_source
      let _dparsetree = set dump_parsetree
      let _dtypedtree = set dump_typedtree
      let _drawlambda = set dump_rawlambda
      let _dlambda = set dump_lambda
      let _dinstr = set dump_instr
      let anonymous = anonymous
    end in 
    [ mk_absname F._absname;
      mk_annot F._annot;
      mk_binannot F._binannot;
      mk_c F._c;
      mk_config F._config;
      mk_g_byt F._g;
      mk_i F._i;
      mk_I F._I;
      mk_impl F._impl;
      mk_intf F._intf;
      mk_intf_suffix F._intf_suffix;
      mk_keep_docs F._keep_docs;
      mk_keep_locs F._keep_locs;
      mk_labels F._labels;
      mk_no_alias_deps F._no_alias_deps;
      mk_no_app_funct F._no_app_funct;
      mk_noassert F._noassert;
      mk_nolabels F._nolabels;
      mk_nostdlib F._nostdlib;
      mk_o F._o;
      mk_open F._open;
      mk_pp F._pp;
      mk_ppx F._ppx;
      mk_principal F._principal;
      mk_rectypes F._rectypes;
      mk_safe_string F._safe_string;
      mk_short_paths F._short_paths;
      mk_strict_sequence F._strict_sequence;
      mk_strict_formats F._strict_formats;
      mk_unsafe F._unsafe;
      mk_v F._v;
      mk_verbose F._verbose;
      mk_version F._version;
      mk_vnum F._vnum;
      mk_w F._w;
      mk_warn_error F._warn_error;
      mk_warn_help F._warn_help;
      mk_where F._where;
      mk__ F.anonymous;
      mk_nopervasives F._nopervasives;
      mk_dsource F._dsource;
      mk_dparsetree F._dparsetree;
      mk_dtypedtree F._dtypedtree;
      mk_drawlambda F._drawlambda;
      mk_dlambda F._dlambda ]
  )

let () = 
  Clflags.unsafe_string := false;
  Clflags.debug := true

let main () =
  try
    readenv ppf Before_args;
    Arg.parse buckle_script_flags anonymous usage;
    exit 0
  with x ->
    Location.report_exception ppf x;
    exit 2

let _ = main ()




