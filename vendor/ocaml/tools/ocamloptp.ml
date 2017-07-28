(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Damien Doligez, projet Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Printf

let compargs = ref ([] : string list)
let profargs = ref ([] : string list)
let toremove = ref ([] : string list)

let option opt () = compargs := opt :: !compargs
let option_with_arg opt arg =
  compargs := (Filename.quote arg) :: opt :: !compargs
;;
let option_with_int opt arg =
  compargs := (string_of_int arg) :: opt :: !compargs
;;

let make_archive = ref false;;
let with_impl = ref false;;
let with_intf = ref false;;
let with_mli = ref false;;
let with_ml = ref false;;

let process_file filename =
  if Filename.check_suffix filename ".ml" then with_ml := true;
  if Filename.check_suffix filename ".mli" then with_mli := true;
  compargs := (Filename.quote filename) :: !compargs
;;

let usage = "Usage: ocamloptp <options> <files>\noptions are:"

let incompatible o =
  fprintf stderr "ocamloptp: profiling is incompatible with the %s option\n" o;
  exit 2

module Options = Main_args.Make_optcomp_options (struct
  let _a () = make_archive := true; option "-a" ()
  let _absname = option "-absname"
  let _annot = option "-annot"
  let _binannot = option "-bin-annot"
  let _c = option "-c"
  let _cc s = option_with_arg "-cc" s
  let _cclib s = option_with_arg "-cclib" s
  let _ccopt s = option_with_arg "-ccopt" s
  let _compact = option "-compact"
  let _config = option "-config"
  let _for_pack s = option_with_arg "-for-pack" s
  let _g = option "-g"
  let _i = option "-i"
  let _I s = option_with_arg "-I" s
  let _impl s = with_impl := true; option_with_arg "-impl" s
  let _inline n = option_with_int "-inline" n
  let _intf s = with_intf := true; option_with_arg "-intf" s
  let _intf_suffix s = option_with_arg "-intf-suffix" s
  let _keep_docs = option "-keep-docs"
  let _keep_locs = option "-keep-locs"
  let _labels = option "-labels"
  let _linkall = option "-linkall"
  let _no_alias_deps = option "-no-alias-deps"
  let _no_app_funct = option "-no-app-funct"
  let _no_float_const_prop = option "-no-float-const-prop"
  let _noassert = option "-noassert"
  let _noautolink = option "-noautolink"
  let _nodynlink = option "-nodynlink"
  let _nolabels = option "-nolabels"
  let _nostdlib = option "-nostdlib"
  let _o s = option_with_arg "-o" s
  let _open s = option_with_arg "-open" s
  let _output_obj = option "-output-obj"
  let _output_complete_obj = option "-output-complete-obj"
  let _p = option "-p"
  let _pack = option "-pack"
  let _pp _s = incompatible "-pp"
  let _ppx _s = incompatible "-ppx"
  let _principal = option "-principal"
  let _rectypes = option "-rectypes"
  let _runtime_variant s = option_with_arg "-runtime-variant" s
  let _S = option "-S"
  let _safe_string = option "-safe-string"
  let _short_paths = option "-short-paths"
  let _strict_sequence = option "-strict-sequence"
  let _strict_formats = option "-strict-formats"
  let _shared = option "-shared"
  let _thread = option "-thread"
  let _unsafe = option "-unsafe"
  let _unsafe_string = option "-unsafe-string"
  let _v = option "-v"
  let _version = option "-version"
  let _vnum = option "-vnum"
  let _verbose = option "-verbose"
  let _w = option_with_arg "-w"
  let _warn_error = option_with_arg "-warn-error"
  let _warn_help = option "-warn-help"
  let _color s = option_with_arg "-color" s
  let _where = option "-where"

  let _nopervasives = option "-nopervasives"
  let _dsource = option "-dsource"
  let _dparsetree = option "-dparsetree"
  let _dtypedtree = option "-dtypedtree"
  let _drawlambda = option "-drawlambda"
  let _dlambda = option "-dlambda"
  let _dclambda = option "-dclambda"
  let _dcmm = option "-dcmm"
  let _dsel = option "-dsel"
  let _dcombine = option "-dcombine"
  let _dcse = option "-dcse"
  let _dlive = option "-dlive"
  let _dspill = option "-dspill"
  let _dsplit = option "-dsplit"
  let _dinterf = option "-dinterf"
  let _dprefer = option "-dprefer"
  let _dalloc = option "-dalloc"
  let _dreload = option "-dreload"
  let _dscheduling = option "-dscheduling"
  let _dlinear = option "-dlinear"
  let _dstartup = option "-dstartup"
  let _opaque = option "-opaque"

  let anonymous = process_file
end);;

let add_profarg s =
  profargs := (Filename.quote s) :: "-m" :: !profargs
;;

let optlist =
    ("-P", Arg.String add_profarg,
           "[afilmt]  Profile constructs specified by argument (default fm):\n\
        \032     a  Everything\n\
        \032     f  Function calls and method calls\n\
        \032     i  if ... then ... else\n\
        \032     l  while and for loops\n\
        \032     m  match ... with\n\
        \032     t  try ... with")
    :: Options.list
in
Arg.parse optlist process_file usage;
if !with_impl && !with_intf then begin
  fprintf stderr "ocamloptp cannot deal with both \"-impl\" and \"-intf\"\n";
  fprintf stderr "please compile interfaces and implementations separately\n";
  exit 2;
end else if !with_impl && !with_mli then begin
  fprintf stderr "ocamloptp cannot deal with both \"-impl\" and .mli files\n";
  fprintf stderr "please compile interfaces and implementations separately\n";
  exit 2;
end else if !with_intf && !with_ml then begin
  fprintf stderr "ocamloptp cannot deal with both \"-intf\" and .ml files\n";
  fprintf stderr "please compile interfaces and implementations separately\n";
  exit 2;
end;
if !with_impl then profargs := "-impl" :: !profargs;
if !with_intf then profargs := "-intf" :: !profargs;
let status =
  Sys.command
    (Printf.sprintf "ocamlopt -pp \"ocamlprof -instrument %s\" %s %s"
        (String.concat " " (List.rev !profargs))
        (if !make_archive then "" else "profiling.cmx")
        (String.concat " " (List.rev !compargs)))
in
exit status
;;
