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
open Compenv

let usage = "Usage: ocaml <options> <object-files> [script-file [arguments]]\n\
             options are:"

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
  Toploop.set_paths ();
  try
    let res =
      let objects =
        List.rev (!preload_objects @ !first_objfiles)
      in
      List.for_all (Topdirs.load_file ppf) objects
    in
    !Toploop.toplevel_startup_hook ();
    res
  with x ->
    try Location.report_exception ppf x; false
    with x ->
      Format.fprintf ppf "Uncaught exception: %s\n" (Printexc.to_string x);
      false

(* If [name] is "", then the "file" is stdin treated as a script file. *)
let file_argument name =
  let ppf = Format.err_formatter in
  if Filename.check_suffix name ".cmo" || Filename.check_suffix name ".cma"
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
      let newargs = Array.sub !argv !current
                              (Array.length !argv - !current)
      in
      Compenv.readenv ppf Before_link;
      if prepare ppf && Toploop.run_script ppf name newargs
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

module Options = Main_args.Make_bytetop_options (struct
  let set r () = r := true
  let clear r () = r := false

  let _absname = set Location.absname
  let _I dir =
    let dir = Misc.expand_directory Config.standard_library dir in
    include_dirs := dir :: !include_dirs
  let _init s = init_file := Some s
  let _noinit = set noinit
  let _labels = clear classic
  let _alias_deps = clear transparent_modules
  let _no_alias_deps = set transparent_modules
  let _app_funct = set applicative_functors
  let _no_app_funct = clear applicative_functors
  let _noassert = set noassert
  let _nolabels = set classic
  let _noprompt = set noprompt
  let _nopromptcont = set nopromptcont
  let _nostdlib = set no_std_include
  let _open s = open_modules := s :: !open_modules
  let _ppx s = first_ppx := s :: !first_ppx
  let _principal = set principal
  let _no_principal = clear principal
  let _rectypes = set recursive_types
  let _no_rectypes = clear recursive_types
  let _safe_string = clear unsafe_string
  let _short_paths = clear real_paths
  let _stdin () = file_argument ""
  let _strict_sequence = set strict_sequence
  let _no_strict_sequence = clear strict_sequence
  let _strict_formats = set strict_formats
  let _no_strict_formats = clear strict_formats
  let _unboxed_types = set unboxed_types
  let _no_unboxed_types = clear unboxed_types
  let _unsafe = set fast
  let _unsafe_string = set unsafe_string
  let _version () = print_version ()
  let _vnum () = print_version_num ()
  let _no_version = set noversion
  let _w s = Warnings.parse_options false s
  let _warn_error s = Warnings.parse_options true s
  let _warn_help = Warnings.help_warnings
  let _dparsetree = set dump_parsetree
  let _dtypedtree = set dump_typedtree
  let _dsource = set dump_source
  let _drawlambda = set dump_rawlambda
  let _dlambda = set dump_lambda
  let _dflambda = set dump_flambda
  let _dtimings () = profile_columns := [ `Time ]
  let _dprofile () = profile_columns := Profile.all_columns
  let _dinstr = set dump_instr

  let _args = wrap_expand Arg.read_arg
  let _args0 = wrap_expand Arg.read_arg0

  let anonymous s = file_argument s
end);;


let main () =
  let ppf = Format.err_formatter in
  Compenv.readenv ppf Before_args;
  let list = ref Options.list in
  begin
    try
      Arg.parse_and_expand_argv_dynamic current argv list file_argument usage;
    with
    | Arg.Bad msg -> Printf.eprintf "%s" msg; exit 2
    | Arg.Help msg -> Printf.printf "%s" msg; exit 0
  end;
  Compenv.readenv ppf Before_link;
  if not (prepare ppf) then exit 2;
  Compmisc.init_path false;
  Toploop.loop Format.std_formatter
