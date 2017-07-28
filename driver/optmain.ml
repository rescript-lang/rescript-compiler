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
  Optcompile.interface ppf name (output_prefix name)

let process_implementation_file ppf name =
  let opref = output_prefix name in
  Optcompile.implementation ppf name opref;
  objfiles := (opref ^ ".cmx") :: !objfiles

let cmxa_present = ref false;;

let process_file ppf name =
  if Filename.check_suffix name ".ml"
  || Filename.check_suffix name ".mlt" then
    process_implementation_file ppf name
  else if Filename.check_suffix name !Config.interface_suffix then begin
    let opref = output_prefix name in
    Optcompile.interface ppf name opref;
    if !make_package then objfiles := (opref ^ ".cmi") :: !objfiles
  end
  else if Filename.check_suffix name ".cmx" then
    objfiles := name :: !objfiles
  else if Filename.check_suffix name ".cmxa" then begin
    cmxa_present := true;
    objfiles := name :: !objfiles
  end else if Filename.check_suffix name ".cmi" && !make_package then
    objfiles := name :: !objfiles
  else if Filename.check_suffix name ext_obj
       || Filename.check_suffix name ext_lib then
    ccobjs := name :: !ccobjs
  else if Filename.check_suffix name ".c" then begin
    Optcompile.c_file name;
    ccobjs := (Filename.chop_suffix (Filename.basename name) ".c" ^ ext_obj)
              :: !ccobjs
  end
  else
    raise(Arg.Bad("don't know what to do with " ^ name))

let usage = "Usage: ocamlopt <options> <files>\nOptions are:"

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

module Options = Main_args.Make_optcomp_options (struct
  let set r () = r := true
  let clear r () = r := false

  let _a = set make_archive
  let _absname = set Location.absname
  let _annot = set annotations
  let _binannot = set binary_annotations
  let _c = set compile_only
  let _cc s = c_compiler := Some s
  let _cclib s = ccobjs := Misc.rev_split_words s @ !ccobjs
  let _ccopt s = first_ccopts := s :: !first_ccopts
  let _compact = clear optimize_for_speed
  let _config () = show_config ()
  let _for_pack s = for_package := Some s
  let _g = set debug
  let _i () = print_types := true; compile_only := true
  let _I dir = include_dirs := dir :: !include_dirs
  let _impl = impl
  let _inline n = inline_threshold := n * 8
  let _intf = intf
  let _intf_suffix s = Config.interface_suffix := s
  let _keep_docs = set keep_docs
  let _keep_locs = set keep_locs
  let _labels = clear classic
  let _linkall = set link_everything
  let _no_alias_deps = set transparent_modules
  let _no_app_funct = clear applicative_functors
  let _no_float_const_prop = clear float_const_prop
  let _noassert = set noassert
  let _noautolink = set no_auto_link
  let _nodynlink = clear dlcode
  let _nolabels = set classic
  let _nostdlib = set no_std_include
  let _o s = output_name := Some s
  let _open s = open_modules := s :: !open_modules
  let _output_obj = set output_c_object
  let _output_complete_obj s =
    set output_c_object s; set output_complete_object s
  let _p = set gprofile
  let _pack = set make_package
  let _pp s = preprocessor := Some s
  let _ppx s = first_ppx := s :: !first_ppx
  let _principal = set principal
  let _rectypes = set recursive_types
  let _runtime_variant s = runtime_variant := s
  let _safe_string = clear unsafe_string
  let _short_paths = clear real_paths
  let _strict_sequence = set strict_sequence
  let _strict_formats = set strict_formats
  let _shared () = shared := true; dlcode := true
  let _S = set keep_asm_file
  let _thread = set use_threads
  let _unsafe = set fast
  let _unsafe_string = set unsafe_string
  let _v () = print_version_and_library "native-code compiler"
  let _version () = print_version_string ()
  let _vnum () = print_version_string ()
  let _verbose = set verbose
  let _w s = Warnings.parse_options false s
  let _warn_error s = Warnings.parse_options true s
  let _warn_help = Warnings.help_warnings
  let _color option =
    begin match Clflags.parse_color_setting option with
          | None -> ()
          | Some setting -> Clflags.color := setting
    end
  let _where () = print_standard_library ()

  let _nopervasives = set nopervasives
  let _dsource = set dump_source
  let _dparsetree = set dump_parsetree
  let _dtypedtree = set dump_typedtree
  let _drawlambda = set dump_rawlambda
  let _dlambda = set dump_lambda
  let _dclambda = set dump_clambda
  let _dcmm = set dump_cmm
  let _dsel = set dump_selection
  let _dcombine = set dump_combine
  let _dcse = set dump_cse
  let _dlive () = dump_live := true; Printmach.print_live := true
  let _dspill = set dump_spill
  let _dsplit = set dump_split
  let _dinterf = set dump_interf
  let _dprefer = set dump_prefer
  let _dalloc = set dump_regalloc
  let _dreload = set dump_reload
  let _dscheduling = set dump_scheduling
  let _dlinear = set dump_linear
  let _dstartup = set keep_startup_file
  let _opaque = set opaque

  let anonymous = anonymous
end);;

let main () =
  native_code := true;
  let ppf = Format.err_formatter in
  try
    readenv ppf Before_args;
    Arg.parse (Arch.command_line_options @ Options.list) anonymous usage;
    readenv ppf Before_link;
    if
      List.length (List.filter (fun x -> !x)
                     [make_package; make_archive; shared;
                      compile_only; output_c_object]) > 1
    then
      fatal "Please specify at most one of -pack, -a, -shared, -c, -output-obj";
    if !make_archive then begin
      if !cmxa_present then
        fatal "Option -a cannot be used with .cmxa input files.";
      Compmisc.init_path true;
      let target = extract_output !output_name in
      Asmlibrarian.create_archive (get_objfiles ()) target;
      Warnings.check_fatal ();
    end
    else if !make_package then begin
      Compmisc.init_path true;
      let target = extract_output !output_name in
      Asmpackager.package_files ppf (Compmisc.initial_env ())
        (get_objfiles ()) target;
      Warnings.check_fatal ();
    end
    else if !shared then begin
      Compmisc.init_path true;
      let target = extract_output !output_name in
      Asmlink.link_shared ppf (get_objfiles ()) target;
      Warnings.check_fatal ();
    end
    else if not !compile_only && !objfiles <> [] then begin
      let target =
        if !output_c_object then
          let s = extract_output !output_name in
          if (Filename.check_suffix s Config.ext_obj
            || Filename.check_suffix s Config.ext_dll)
          then s
          else
            fatal
              (Printf.sprintf
                 "The extension of the output file must be %s or %s"
                 Config.ext_obj Config.ext_dll
              )
        else
          default_output !output_name
      in
      Compmisc.init_path true;
      Asmlink.link ppf (get_objfiles ()) target;
      Warnings.check_fatal ();
    end;
    exit 0
  with x ->
      Location.report_exception ppf x;
      exit 2

let _ = main ()
