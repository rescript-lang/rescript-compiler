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

open Clflags

let usage =
   "Usage: ocamlnat <options> <object-files> [script-file]\noptions are:"

let preload_objects = ref []

let prepare ppf =
  Opttoploop.set_paths ();
  try
    let res =
      List.for_all (Opttopdirs.load_file ppf) (List.rev !preload_objects)
    in
    !Opttoploop.toplevel_startup_hook ();
    res
  with x ->
    try Location.report_exception ppf x; false
    with x ->
      Format.fprintf ppf "Uncaught exception: %s\n" (Printexc.to_string x);
      false

let file_argument name =
  let ppf = Format.err_formatter in
  if Filename.check_suffix name ".cmxs"
    || Filename.check_suffix name ".cmx"
    || Filename.check_suffix name ".cmxa"
  then preload_objects := name :: !preload_objects
  else
    begin
      let newargs = Array.sub Sys.argv !Arg.current
                              (Array.length Sys.argv - !Arg.current)
      in
      if prepare ppf && Opttoploop.run_script ppf name newargs
      then exit 0
      else exit 2
    end

let print_version () =
  Printf.printf "The OCaml toplevel, version %s\n" Sys.ocaml_version;
  exit 0;
;;

let print_version_num () =
  Printf.printf "%s\n" Sys.ocaml_version;
  exit 0;
;;

module Options = Main_args.Make_opttop_options (struct
  let set r () = r := true
  let clear r () = r := false

  let _absname = set Location.absname
  let _compact = clear optimize_for_speed
  let _I dir =
    let dir = Misc.expand_directory Config.standard_library dir in
    include_dirs := dir :: !include_dirs
  let _init s = init_file := Some s
  let _inline n = inline_threshold := n * 8
  let _labels = clear classic
  let _no_alias_deps = set transparent_modules
  let _no_app_funct = clear applicative_functors
  let _noassert = set noassert
  let _nolabels = set classic
  let _noprompt = set noprompt
  let _nopromptcont = set nopromptcont
  let _nostdlib = set no_std_include
  let _ppx s = Compenv.first_ppx := s :: !Compenv.first_ppx
  let _principal = set principal
  let _real_paths = set real_paths
  let _rectypes = set recursive_types
  let _strict_sequence = set strict_sequence
  let _strict_formats = set strict_formats
  let _S = set keep_asm_file
  let _short_paths = clear real_paths
  let _stdin () = file_argument ""
  let _unsafe = set fast
  let _version () = print_version ()
  let _vnum () = print_version_num ()
  let _w s = Warnings.parse_options false s
  let _warn_error s = Warnings.parse_options true s
  let _warn_help = Warnings.help_warnings

  let _dsource = set dump_source
  let _dparsetree = set dump_parsetree
  let _dtypedtree = set dump_typedtree
  let _drawlambda = set dump_rawlambda
  let _dlambda = set dump_lambda
  let _dclambda = set dump_clambda
  let _dcmm = set dump_cmm
  let _dsel = set dump_selection
  let _dcombine = set dump_combine
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

  let anonymous = file_argument
end);;

let main () =
  Arg.parse Options.list file_argument usage;
  if not (prepare Format.err_formatter) then exit 2;
  Opttoploop.loop Format.std_formatter
