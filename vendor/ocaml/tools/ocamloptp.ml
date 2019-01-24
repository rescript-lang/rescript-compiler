(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Damien Doligez, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

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
let option_with_float opt arg =
  compargs := (string_of_float arg) :: opt :: !compargs
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
  let _afl_instrument = option "-afl-instrument"
  let _afl_inst_ratio n = option_with_int "-afl-inst-ratio" n
  let _annot = option "-annot"
  let _binannot = option "-bin-annot"
  let _c = option "-c"
  let _cc s = option_with_arg "-cc" s
  let _cclib s = option_with_arg "-cclib" s
  let _ccopt s = option_with_arg "-ccopt" s
  let _clambda_checks = option "-clambda-checks"
  let _compact = option "-compact"
  let _config = option "-config"
  let _for_pack s = option_with_arg "-for-pack" s
  let _g = option "-g"
  let _i = option "-i"
  let _I s = option_with_arg "-I" s
  let _impl s = with_impl := true; option_with_arg "-impl" s
  let _inline s = option_with_arg "-inline" s
  let _inline_toplevel n = option_with_arg "-inline-toplevel" n
  let _inlining_report = option "-inlining-report"
  let _dump_pass = option_with_arg "-dump-pass"
  let _inline_max_depth n = option_with_arg "-inline-max-depth" n
  let _rounds n = option_with_int "-rounds" n
  let _inline_max_unroll n = option_with_arg "-unroll" n
  let _inline_call_cost n = option_with_arg "-inline-call-cost" n
  let _inline_alloc_cost n = option_with_arg "-inline-alloc-cost" n
  let _inline_prim_cost n = option_with_arg "-inline-prim-cost" n
  let _inline_branch_cost n = option_with_arg "-inline-branch-cost" n
  let _inline_indirect_cost n = option_with_arg "-inline-indirect-cost" n
  let _inline_lifting_benefit n = option_with_arg "-inline-lifting-benefit" n
  let _inline_branch_factor n = option_with_arg "-inline-branch-factor" n
  let _classic_inlining = option "-Oclassic"
  let _intf s = with_intf := true; option_with_arg "-intf" s
  let _intf_suffix s = option_with_arg "-intf-suffix" s
  let _keep_docs = option "-keep-docs"
  let _no_keep_docs = option "-no-keep-docs"
  let _keep_locs = option "-keep-locs"
  let _no_keep_locs = option "-no-keep-locs"
  let _labels = option "-labels"
  let _linkall = option "-linkall"
  let _alias_deps = option "-alias-deps"
  let _no_alias_deps = option "-no-alias-deps"
  let _app_funct = option "-app-funct"
  let _no_app_funct = option "-no-app-funct"
  let _no_float_const_prop = option "-no-float-const-prop"
  let _noassert = option "-noassert"
  let _noautolink = option "-noautolink"
  let _nodynlink = option "-nodynlink"
  let _nolabels = option "-nolabels"
  let _nostdlib = option "-nostdlib"
  let _no_unbox_free_vars_of_closures = option "-no-unbox-free-vars-of-closures"
  let _no_unbox_specialised_args = option "-no-unbox-specialised-args"
  let _o s = option_with_arg "-o" s
  let _o2 = option "-O2"
  let _o3 = option "-O3"
  let _open s = option_with_arg "-open" s
  let _output_obj = option "-output-obj"
  let _output_complete_obj = option "-output-complete-obj"
  let _p = option "-p"
  let _pack = option "-pack"
  let _plugin = option_with_arg "-plugin"
  let _pp _s = incompatible "-pp"
  let _ppx _s = incompatible "-ppx"
  let _principal = option "-principal"
  let _no_principal = option "-no-principal"
  let _rectypes = option "-rectypes"
  let _no_rectypes = option "-no-rectypes"
  let _remove_unused_arguments = option "-remove-unused-arguments"
  let _runtime_variant s = option_with_arg "-runtime-variant" s
  let _S = option "-S"
  let _safe_string = option "-safe-string"
  let _short_paths = option "-short-paths"
  let _strict_sequence = option "-strict-sequence"
  let _no_strict_sequence = option "-no-strict-sequence"
  let _strict_formats = option "-strict-formats"
  let _no_strict_formats = option "-no-strict-formats"
  let _shared = option "-shared"
  let _thread = option "-thread"
  let _unbox_closures = option "-unbox-closures"
  let _unbox_closures_factor = option_with_int "-unbox-closures"
  let _unboxed_types = option "-unboxed-types"
  let _no_unboxed_types = option "-no-unboxed-types"
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

  let _linscan = option "-linscan"
  let _nopervasives = option "-nopervasives"
  let _dsource = option "-dsource"
  let _dparsetree = option "-dparsetree"
  let _dtypedtree = option "-dtypedtree"
  let _drawlambda = option "-drawlambda"
  let _dlambda = option "-dlambda"
  let _drawclambda = option "-drawclambda"
  let _dclambda = option "-dclambda"
  let _drawflambda = option "-drawflambda"
  let _dflambda = option "-dflambda"
  let _dflambda_no_invariants = option "-dflambda-no-invariants"
  let _dflambda_let stamp = option_with_int "-dflambda-let" stamp
  let _dflambda_verbose = option "-dflambda-verbose"
  let _dcmm = option "-dcmm"
  let _dsel = option "-dsel"
  let _dcombine = option "-dcombine"
  let _dcse = option "-dcse"
  let _dlive = option "-dlive"
  let _davail = option "-davail"
  let _drunavail = option "-drunavail"
  let _dspill = option "-dspill"
  let _dsplit = option "-dsplit"
  let _dinterf = option "-dinterf"
  let _dprefer = option "-dprefer"
  let _dalloc = option "-dalloc"
  let _dreload = option "-dreload"
  let _dscheduling = option "-dscheduling"
  let _dlinear = option "-dlinear"
  let _dstartup = option "-dstartup"
  let _dinterval = option "-dinterval"
  let _dtimings = option "-dtimings"
  let _dprofile = option "-dprofile"
  let _opaque = option "-opaque"

  let _args = Arg.read_arg
  let _args0 = Arg.read_arg0
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
Arg.parse_expand optlist process_file usage;
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
