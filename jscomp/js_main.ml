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
open Compenv


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

module Options = Main_args.Make_bytecomp_options (struct
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
end)

let add_include_path s = 
  let (//) = Filename.concat in
  let path = 
    Ext_filename.resolve 
      (Lazy.force Ext_filename.cwd) s // "lib"// "ocaml"  in 
  Clflags.include_dirs := path :: ! Clflags.include_dirs

let buckle_script_flags = 
  ("-js-npm-output-path", Arg.String Js_config.set_npm_package_path, 
   " set npm-output-path: package-name:path, for example `bs-platform:lib/js`")
  ::
  ("-js-npm-package-include", Arg.String add_include_path, 
   " set package names, for example bs-platform "  )
  :: ("-js-module", Arg.String Js_config.cmd_set_module, 
    " set module system: commonjs (default), amdjs, google:package_name")
  :: ("-js-gen-tds", Arg.Set Js_config.default_gen_tds, 
    " set will generate `.d.ts` file for typescript (experimental)")
  :: Options.list 

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




