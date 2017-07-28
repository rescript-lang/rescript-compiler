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

(* Compiling C files and building C libraries *)

let command cmdline =
  if !Clflags.verbose then begin
    prerr_string "+ ";
    prerr_string cmdline;
    prerr_newline()
  end;
  Sys.command cmdline

let run_command cmdline = ignore(command cmdline)

(* Build @responsefile to work around Windows limitations on
   command-line length *)
let build_diversion lst =
  let (responsefile, oc) = Filename.open_temp_file "camlresp" "" in
  List.iter (fun f -> Printf.fprintf oc "%s\n" f) lst;
  close_out oc;
  at_exit (fun () -> Misc.remove_file responsefile);
  "@" ^ responsefile

let quote_files lst =
  let lst = List.filter (fun f -> f <> "") lst in
  let quoted = List.map Filename.quote lst in
  let s = String.concat " " quoted in
  if String.length s >= 4096 && Sys.os_type = "Win32"
  then build_diversion quoted
  else s

let quote_prefixed pr lst =
  let lst = List.filter (fun f -> f <> "") lst in
  let lst = List.map (fun f -> pr ^ f) lst in
  quote_files lst

let quote_optfile = function
  | None -> ""
  | Some f -> Filename.quote f

let compile_file name =
  command
    (Printf.sprintf
       "%s -c %s %s %s %s %s"
       (match !Clflags.c_compiler with
        | Some cc -> cc
        | None ->
            if !Clflags.native_code
            then Config.native_c_compiler
            else Config.bytecomp_c_compiler)
       (if !Clflags.debug then "-g" else "")
       (String.concat " " (List.rev !Clflags.all_ccopts))
       (quote_prefixed "-I" (List.rev !Clflags.include_dirs))
       (Clflags.std_include_flag "-I")
       (Filename.quote name))

let create_archive archive file_list =
  Misc.remove_file archive;
  let quoted_archive = Filename.quote archive in
  match Config.ccomp_type with
    "msvc" ->
      command(Printf.sprintf "link /lib /nologo /out:%s %s"
                             quoted_archive (quote_files file_list))
  | _ ->
      assert(String.length Config.ar > 0);
      let r1 =
        command(Printf.sprintf "%s rc %s %s"
                Config.ar quoted_archive (quote_files file_list)) in
      if r1 <> 0 || String.length Config.ranlib = 0
      then r1
      else command(Config.ranlib ^ " " ^ quoted_archive)

let expand_libname name =
  if String.length name < 2 || String.sub name 0 2 <> "-l"
  then name
  else begin
    let libname =
      "lib" ^ String.sub name 2 (String.length name - 2) ^ Config.ext_lib in
    try
      Misc.find_in_path !Config.load_path libname
    with Not_found ->
      libname
  end

type link_mode =
  | Exe
  | Dll
  | MainDll
  | Partial

let remove_Wl cclibs =
  cclibs |> List.map (fun cclib ->
    (* -Wl,-foo,bar -> -foo bar *)
    if String.length cclib >= 4 && "-Wl," = String.sub cclib 0 4 then
      String.map (function ',' -> ' ' | c -> c)
                 (String.sub cclib 4 (String.length cclib - 4))
    else cclib)

let call_linker mode output_name files extra =
  let cmd =
    if mode = Partial then
      Printf.sprintf "%s%s %s %s %s"
        Config.native_pack_linker
        (Filename.quote output_name)
        (quote_prefixed "-L" !Config.load_path)
        (quote_files (remove_Wl files))
        extra
    else
      Printf.sprintf "%s -o %s %s %s %s %s %s %s"
        (match !Clflags.c_compiler, mode with
        | Some cc, _ -> cc
        | None, Exe -> Config.mkexe
        | None, Dll -> Config.mkdll
        | None, MainDll -> Config.mkmaindll
        | None, Partial -> assert false
        )
        (Filename.quote output_name)
        (if !Clflags.gprofile then Config.cc_profile else "")
        ""  (*(Clflags.std_include_flag "-I")*)
        (quote_prefixed "-L" !Config.load_path)
        (String.concat " " (List.rev !Clflags.all_ccopts))
        (quote_files files)
        extra
  in
  command cmd = 0
