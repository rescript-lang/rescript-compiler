(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Clflags

let usage =
   "Usage: ocamlnat <options> <object-files> [script-file]\noptions are:"

let preload_objects = ref []

(* Position of the first non expanded argument *)
let first_nonexpanded_pos = ref 0

let current = ref (!Arg.current)

let argv = ref Sys.argv

(* Test whether the option is part of a responsefile *)
let is_expanded pos = pos < !first_nonexpanded_pos

let expand_position pos len =
  if pos < !first_nonexpanded_pos then
    first_nonexpanded_pos := !first_nonexpanded_pos + len (* Shift the position *)
  else
    first_nonexpanded_pos :=  pos + len + 2 (* New last position *)


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
  else if is_expanded !current then begin
    (* Script files are not allowed in expand options because otherwise the
       check in override arguments may fail since the new argv can be larger
       than the original argv.
    *)
    Printf.eprintf "For implementation reasons, the toplevel does not support\
    \ having script files (here %S) inside expanded arguments passed through the\
    \ -args{,0} command-line option.\n" name;
    exit 2
  end else begin
    let newargs = Array.sub !argv !Arg.current
                              (Array.length !argv - !Arg.current)
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

let wrap_expand f s =
  let start = !current in
  let arr = f s in
  expand_position start (Array.length arr);
  arr

module Options = Main_args.Make_opttop_options (struct
  let set r () = r := true
  let clear r () = r := false

  let _absname = set Location.absname
  let _compact = clear optimize_for_speed
  let _I dir =
    let dir = Misc.expand_directory Config.standard_library dir in
    include_dirs := dir :: !include_dirs
  let _init s = init_file := Some s
  let _noinit = set noinit
  let _clambda_checks () = clambda_checks := true
  let _inline spec =
    Float_arg_helper.parse spec
      "Syntax: -inline <n> | <round>=<n>[,...]"
      inline_threshold
  let _inline_indirect_cost spec =
    Int_arg_helper.parse spec
      "Syntax: -inline-indirect-cost <n> | <round>=<n>[,...]"
      inline_indirect_cost
  let _inline_toplevel spec =
    Int_arg_helper.parse spec
      "Syntax: -inline-toplevel <n> | <round>=<n>[,...]"
      inline_toplevel_threshold
  let _inlining_report () = inlining_report := true
  let _dump_pass pass = set_dumped_pass pass true
  let _rounds n = simplify_rounds := Some n
  let _inline_max_unroll spec =
    Int_arg_helper.parse spec
      "Syntax: -inline-max-unroll <n> | <round>=<n>[,...]"
      inline_max_unroll
  let _classic_inlining () = classic_inlining := true
  let _inline_call_cost spec =
    Int_arg_helper.parse spec
      "Syntax: -inline-call-cost <n> | <round>=<n>[,...]"
       inline_call_cost
  let _inline_alloc_cost spec =
    Int_arg_helper.parse spec
      "Syntax: -inline-alloc-cost <n> | <round>=<n>[,...]"
      inline_alloc_cost
  let _inline_prim_cost spec =
    Int_arg_helper.parse spec
      "Syntax: -inline-prim-cost <n> | <round>=<n>[,...]"
       inline_prim_cost
  let _inline_branch_cost spec =
    Int_arg_helper.parse spec
      "Syntax: -inline-branch-cost <n> | <round>=<n>[,...]"
      inline_branch_cost
  let _inline_lifting_benefit spec =
    Int_arg_helper.parse spec
      "Syntax: -inline-lifting-benefit <n> | <round>=<n>[,...]"
      inline_lifting_benefit
  let _inline_branch_factor spec =
    Float_arg_helper.parse spec
      "Syntax: -inline-branch-factor <n> | <round>=<n>[,...]"
      inline_branch_factor
  let _inline_max_depth spec =
    Int_arg_helper.parse spec
      "Syntax: -inline-max-depth <n> | <round>=<n>[,...]"
      inline_max_depth
  let _no_unbox_free_vars_of_closures = clear unbox_free_vars_of_closures
  let _no_unbox_specialised_args = clear unbox_specialised_args
  let _o s = output_name := Some s
  let _o2 () =
    default_simplify_rounds := 2;
    use_inlining_arguments_set o2_arguments;
    use_inlining_arguments_set ~round:0 o1_arguments
  let _o3 () =
    default_simplify_rounds := 3;
    use_inlining_arguments_set o3_arguments;
    use_inlining_arguments_set ~round:1 o2_arguments;
    use_inlining_arguments_set ~round:0 o1_arguments
  let _remove_unused_arguments = set remove_unused_arguments
  let _unbox_closures = set unbox_closures
  let _unbox_closures_factor f = unbox_closures_factor := f
  let _drawclambda = set dump_rawclambda
  let _dclambda = set dump_clambda
  let _drawflambda = set dump_rawflambda
  let _dflambda = set dump_flambda
  let _dflambda_let stamp = dump_flambda_let := Some stamp
  let _dflambda_verbose () =
    set dump_flambda ();
    set dump_flambda_verbose ()
  let _dflambda_no_invariants = clear flambda_invariant_checks
  let _labels = clear classic
  let _alias_deps = clear transparent_modules
  let _no_alias_deps = set transparent_modules
  let _dlinscan = set use_linscan
  let _app_funct = set applicative_functors
  let _no_app_funct = clear applicative_functors
  let _noassert = set noassert
  let _nolabels = set classic
  let _noprompt = set noprompt
  let _nopromptcont = set nopromptcont
  let _nostdlib = set no_std_include
  let _ppx s = Compenv.first_ppx := s :: !Compenv.first_ppx
  let _principal = set principal
  let _no_principal = clear principal
  let _real_paths = set real_paths
  let _rectypes = set recursive_types
  let _no_rectypes = clear recursive_types
  let _strict_sequence = set strict_sequence
  let _no_strict_sequence = clear strict_sequence
  let _strict_formats = set strict_formats
  let _no_strict_formats = clear strict_formats
  let _S = set keep_asm_file
  let _short_paths = clear real_paths
  let _stdin () = file_argument ""
  let _unboxed_types = set unboxed_types
  let _no_unboxed_types = clear unboxed_types
  let _unsafe = set fast
  let _verbose = set verbose
  let _version () = print_version ()
  let _vnum () = print_version_num ()
  let _no_version = set noversion
  let _w s = Warnings.parse_options false s
  let _warn_error s = Warnings.parse_options true s
  let _warn_help = Warnings.help_warnings

  let _dsource = set dump_source
  let _dparsetree = set dump_parsetree
  let _dtypedtree = set dump_typedtree
  let _drawlambda = set dump_rawlambda
  let _dlambda = set dump_lambda
  let _drawclambda = set dump_rawclambda
  let _dclambda = set dump_clambda
  let _dcmm = set dump_cmm
  let _dsel = set dump_selection
  let _dcombine = set dump_combine
  let _dcse = set dump_cse
  let _dlive () = dump_live := true; Printmach.print_live := true
  let _davail () = dump_avail := true
  let _drunavail () = debug_runavail := true
  let _dspill = set dump_spill
  let _dsplit = set dump_split
  let _dinterf = set dump_interf
  let _dprefer = set dump_prefer
  let _dalloc = set dump_regalloc
  let _dreload = set dump_reload
  let _dscheduling = set dump_scheduling
  let _dlinear = set dump_linear
  let _dinterval = set dump_interval
  let _dstartup = set keep_startup_file
  let _safe_string = clear unsafe_string
  let _unsafe_string = set unsafe_string
  let _open s = open_modules := s :: !open_modules

  let _args = wrap_expand Arg.read_arg
  let _args0 = wrap_expand Arg.read_arg0

  let anonymous = file_argument
end);;

let main () =
  native_code := true;
  let list = ref Options.list in
  begin
    try
      Arg.parse_and_expand_argv_dynamic current argv list file_argument usage;
    with
    | Arg.Bad msg -> Format.fprintf Format.err_formatter "%s%!" msg; exit 2
    | Arg.Help msg -> Format.fprintf Format.std_formatter "%s%!" msg; exit 0
  end;
  if not (prepare Format.err_formatter) then exit 2;
  Compmisc.init_path true;
  Opttoploop.loop Format.std_formatter
