(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Printf
open Ocamlmklibconfig

(* PR#4783: under Windows, don't use absolute paths because we do
   not know where the binary distribution will be installed. *)
let compiler_path name =
  if Sys.os_type = "Win32" then name else Filename.concat bindir name

let bytecode_objs = ref []  (* .cmo,.cma,.ml,.mli files to pass to ocamlc *)
and native_objs = ref []    (* .cmx,.cmxa,.ml,.mli files to pass to ocamlopt *)
and c_objs = ref []         (* .o, .a, .obj, .lib, .dll files to pass
                               to mksharedlib and ar *)
and caml_libs = ref []      (* -cclib to pass to ocamlc, ocamlopt *)
and caml_opts = ref []      (* -ccopt to pass to ocamlc, ocamlopt *)
and dynlink = ref supports_shared_libraries
and failsafe = ref false    (* whether to fall back on static build only *)
and c_libs = ref []         (* libs to pass to mksharedlib and ocamlc -cclib *)
and c_Lopts = ref []        (* options to pass to mksharedlib and ocamlc -cclib *)
and c_opts = ref []         (* options to pass to mksharedlib and ocamlc -ccopt *)
and ld_opts = ref []        (* options to pass only to the linker *)
and ocamlc = ref (compiler_path "ocamlc")
and ocamlc_opts = ref []    (* options to pass only to ocamlc *)
and ocamlopt = ref (compiler_path "ocamlopt")
and ocamlopt_opts = ref []  (* options to pass only to ocamlc *)
and output = ref "a"        (* Output name for OCaml part of library *)
and output_c = ref ""       (* Output name for C part of library *)
and rpath = ref []          (* rpath options *)
and debug = ref false       (* -g option *)
and verbose = ref false

let starts_with s pref =
  String.length s >= String.length pref &&
  String.sub s 0 (String.length pref) = pref
let ends_with = Filename.check_suffix
let chop_prefix s pref =
  String.sub s (String.length pref) (String.length s - String.length pref)
let chop_suffix = Filename.chop_suffix

exception Bad_argument of string

let print_version () =
  printf "ocamlmklib, version %s\n" Sys.ocaml_version;
  exit 0;
;;

let print_version_num () =
  printf "%s\n" Sys.ocaml_version;
  exit 0;
;;

let parse_arguments argv =
  let i = ref 1 in
  let next_arg () =
    if !i + 1 >= Array.length argv
    then raise (Bad_argument("Option " ^ argv.(!i) ^ " expects one argument"));
    incr i; argv.(!i) in
  while !i < Array.length argv do
    let s = argv.(!i) in
    if ends_with s ".cmo" || ends_with s ".cma" then
      bytecode_objs := s :: !bytecode_objs
    else if ends_with s ".cmx" || ends_with s ".cmxa" then
      native_objs := s :: !native_objs
    else if ends_with s ".ml" || ends_with s ".mli" then
     (bytecode_objs := s :: !bytecode_objs;
      native_objs := s :: !native_objs)
    else if List.exists (ends_with s) [".o"; ".a"; ".obj"; ".lib"; ".dll"] then
      c_objs := s :: !c_objs
    else if s = "-cclib" then
      caml_libs := next_arg () :: "-cclib" :: !caml_libs
    else if s = "-ccopt" then
      caml_opts := next_arg () :: "-ccopt" :: !caml_opts
    else if s = "-custom" then
      dynlink := false
    else if s = "-I" then
      caml_opts := next_arg () :: "-I" :: !caml_opts
    else if s = "-failsafe" then
      failsafe := true
    else if s = "-g" then
      debug := true
    else if s = "-h" || s = "-help" || s = "--help" then
      raise (Bad_argument "")
    else if s = "-ldopt" then
      ld_opts := next_arg () :: !ld_opts
    else if s = "-linkall" then
      caml_opts := s :: !caml_opts
    else if starts_with s "-l" then
      c_libs := s :: !c_libs
    else if starts_with s "-L" then
     (c_Lopts := s :: !c_Lopts;
      let l = chop_prefix s "-L" in
      if not (Filename.is_relative l) then rpath := l :: !rpath)
    else if s = "-ocamlcflags" then
      ocamlc_opts := next_arg () :: !ocamlc_opts
    else if s = "-ocamlc" then
      ocamlc := next_arg ()
    else if s = "-ocamlopt" then
      ocamlopt := next_arg ()
    else if s = "-ocamloptflags" then
      ocamlopt_opts := next_arg () :: !ocamlopt_opts
    else if s = "-o" then
      output := next_arg()
    else if s = "-oc" then
      output_c := next_arg()
    else if s = "-dllpath" || s = "-R" || s = "-rpath" then
      rpath := next_arg() :: !rpath
    else if starts_with s "-R" then
      rpath := chop_prefix s "-R" :: !rpath
    else if s = "-Wl,-rpath" then
     (let a = next_arg() in
      if starts_with a "-Wl,"
      then rpath := chop_prefix a "-Wl," :: !rpath
      else raise (Bad_argument("Option -Wl,-rpath expects a -Wl, argument")))
    else if starts_with s "-Wl,-rpath," then
      rpath := chop_prefix s "-Wl,-rpath," :: !rpath
    else if starts_with s "-Wl,-R" then
      rpath := chop_prefix s "-Wl,-R" :: !rpath
    else if s = "-v" || s = "-verbose" then
      verbose := true
    else if s = "-version" then
      print_version ()
    else if s = "-vnum" then
      print_version_num ()
    else if starts_with s "-F" then
      c_opts := s :: !c_opts
    else if s = "-framework" then
      (let a = next_arg() in c_opts := a :: s :: !c_opts)
    else if starts_with s "-" then
      prerr_endline ("Unknown option " ^ s)
    else
      raise (Bad_argument("Don't know what to do with " ^ s));
    incr i
  done;
  List.iter
    (fun r -> r := List.rev !r)
    [ bytecode_objs; native_objs; caml_libs; caml_opts;
      c_libs; c_objs; c_opts; ld_opts; rpath ];
(* Put -L options in front of -l options in -cclib to mimic -ccopt behavior *)
  c_libs := !c_Lopts @ !c_libs;

  if !output_c = "" then output_c := !output

let usage = "\
Usage: ocamlmklib [options] <.cmo|.cma|.cmx|.cmxa|.ml|.mli|.o|.a|.obj|.lib|\
                             .dll files>\
\nOptions are:\
\n  -cclib <lib>   C library passed to ocamlc -a or ocamlopt -a only\
\n  -ccopt <opt>   C option passed to ocamlc -a or ocamlopt -a only\
\n  -custom        Disable dynamic loading\
\n  -g             Build with debug information\
\n  -dllpath <dir> Add <dir> to the run-time search path for DLLs\
\n  -F<dir>        Specify a framework directory (MacOSX)\
\n  -framework <name>    Use framework <name> (MacOSX)\
\n  -help          Print this help message and exit\
\n  --help         Same as -help\
\n  -h             Same as -help\
\n  -I <dir>       Add <dir> to the path searched for OCaml object files\
\n  -failsafe      fall back to static linking if DLL construction failed\
\n  -ldopt <opt>   C option passed to the shared linker only\
\n  -linkall       Build OCaml archive with link-all behavior\
\n  -l<lib>        Specify a dependent C library\
\n  -L<dir>        Add <dir> to the path searched for C libraries\
\n  -ocamlc <cmd>  Use <cmd> in place of \"ocamlc\"\
\n  -ocamlcflags <opt>    Pass <opt> to ocamlc\
\n  -ocamlopt <cmd> Use <cmd> in place of \"ocamlopt\"\
\n  -ocamloptflags <opt>  Pass <opt> to ocamlopt\
\n  -o <name>      Generated OCaml library is named <name>.cma or <name>.cmxa\
\n  -oc <name>     Generated C library is named dll<name>.so or lib<name>.a\
\n  -rpath <dir>   Same as -dllpath <dir>\
\n  -R<dir>        Same as -rpath\
\n  -verbose       Print commands before executing them\
\n  -v             same as -verbose\
\n  -version       Print version and exit\
\n  -vnum          Print version number and exit\
\n  -Wl,-rpath,<dir>     Same as -dllpath <dir>\
\n  -Wl,-rpath -Wl,<dir> Same as -dllpath <dir>\
\n  -Wl,-R<dir>          Same as -dllpath <dir>\
\n"

let command cmd =
  if !verbose then (print_string "+ "; print_string cmd; print_newline());
  Sys.command cmd

let scommand cmd =
  if command cmd <> 0 then exit 2

let safe_remove s =
  try Sys.remove s with Sys_error _ -> ()

let make_set l =
  let rec merge l = function
    []     -> List.rev l
  | p :: r -> if List.mem p l then merge l r else merge (p::l) r
  in
  merge [] l

let make_rpath flag =
  if !rpath = [] || flag = ""
  then ""
  else flag ^ String.concat ":" (make_set !rpath)

let make_rpath_ccopt flag =
  if !rpath = [] || flag = ""
  then ""
  else "-ccopt " ^ flag ^ String.concat ":" (make_set !rpath)

let prefix_list pref l =
  List.map (fun s -> pref ^ s) l

let prepostfix pre name post =
  let base = Filename.basename name in
  let dir = Filename.dirname name in
  Filename.concat dir (pre ^ base ^ post)
;;

let transl_path s =
  match Sys.os_type with
    | "Win32" ->
        let s = Bytes.of_string s in
        let rec aux i =
          if i = Bytes.length s || Bytes.get s i = ' ' then s
          else begin
            if Bytes.get s i = '/' then Bytes.set s i '\\';
            aux (i + 1)
          end
        in Bytes.to_string (aux 0)
    | _ -> s

let build_libs () =
  if !c_objs <> [] then begin
    if !dynlink then begin
      let retcode = command
          (Printf.sprintf "%s %s -o %s %s %s %s %s %s"
             mkdll
             (if !debug then "-g" else "")
             (prepostfix "dll" !output_c ext_dll)
             (String.concat " " !c_objs)
             (String.concat " " !c_opts)
             (String.concat " " !ld_opts)
             (make_rpath mksharedlibrpath)
             (String.concat " " !c_libs)
          )
      in
      if retcode <> 0 then if !failsafe then dynlink := false else exit 2
    end;
    safe_remove (prepostfix "lib" !output_c ext_lib);
    scommand
      (mklib (prepostfix "lib" !output_c ext_lib)
             (String.concat " " !c_objs) "");
  end;
  if !bytecode_objs <> [] then
    scommand
      (sprintf "%s -a %s %s %s -o %s.cma %s %s -dllib -l%s -cclib -l%s %s %s %s %s"
                  (transl_path !ocamlc)
                  (if !debug then "-g" else "")
                  (if !dynlink then "" else "-custom")
                  (String.concat " " !ocamlc_opts)
                  !output
                  (String.concat " " !caml_opts)
                  (String.concat " " !bytecode_objs)
                  (Filename.basename !output_c)
                  (Filename.basename !output_c)
                  (String.concat " " (prefix_list "-ccopt " !c_opts))
                  (make_rpath_ccopt byteccrpath)
                  (String.concat " " (prefix_list "-cclib " !c_libs))
                  (String.concat " " !caml_libs));
  if !native_objs <> [] then
    scommand
      (sprintf "%s -a %s %s -o %s.cmxa %s %s -cclib -l%s %s %s %s %s"
                  (transl_path !ocamlopt)
                  (if !debug then "-g" else "")
                  (String.concat " " !ocamlopt_opts)
                  !output
                  (String.concat " " !caml_opts)
                  (String.concat " " !native_objs)
                  (Filename.basename !output_c)
                  (String.concat " " (prefix_list "-ccopt " !c_opts))
                  (make_rpath_ccopt nativeccrpath)
                  (String.concat " " (prefix_list "-cclib " !c_libs))
                  (String.concat " " !caml_libs))

let _ =
  try
    parse_arguments Sys.argv;
    build_libs()
  with
  | Bad_argument "" ->
      prerr_string usage; exit 0
  | Bad_argument s ->
      prerr_endline s; prerr_string usage; exit 4
  | Sys_error s ->
      prerr_string "System error: "; prerr_endline s; exit 4
  | x ->
      raise x
