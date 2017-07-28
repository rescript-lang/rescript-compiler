(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          OCaml port by John Malecki and Xavier Leroy                *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Input_handling
open Question
open Command_line
open Debugger_config
open Checkpoints
open Time_travel
open Parameters
open Program_management
open Frames
open Show_information
open Format
open Primitives

let line_buffer = Lexing.from_function read_user_input

let rec loop ppf = line_loop ppf line_buffer

let current_duration = ref (-1L)

let rec protect ppf restart loop =
  try
    loop ppf
  with
  | End_of_file ->
      protect ppf restart (function ppf ->
        forget_process
          !current_checkpoint.c_fd
          !current_checkpoint.c_pid;
        pp_print_flush ppf ();
        stop_user_input ();
        restart ppf)
  | Toplevel ->
      protect ppf restart (function ppf ->
        pp_print_flush ppf ();
        stop_user_input ();
        restart ppf)
  | Sys.Break ->
      protect ppf restart (function ppf ->
        fprintf ppf "Interrupted.@.";
        Exec.protect (function () ->
          stop_user_input ();
          if !loaded then begin
            try_select_frame 0;
            show_current_event ppf;
          end);
        restart ppf)
  | Current_checkpoint_lost ->
      protect ppf restart (function ppf ->
        fprintf ppf "Trying to recover...@.";
        stop_user_input ();
        recover ();
        try_select_frame 0;
        show_current_event ppf;
        restart ppf)
  | Current_checkpoint_lost_start_at (time, init_duration) ->
      protect ppf restart (function ppf ->
        let b =
          if !current_duration = -1L then begin
            let msg = sprintf "Restart from time %Ld and try to get \
                               closer of the problem" time in
            stop_user_input ();
            if yes_or_no msg then
              (current_duration := init_duration; true)
            else
              false
            end
          else
            true in
        if b then
          begin
            go_to time;
            current_duration := Int64.div !current_duration 10L;
            if !current_duration > 0L then
              while true do
                step !current_duration
              done
            else begin
              current_duration := -1L;
              stop_user_input ();
              show_current_event ppf;
              restart ppf;
            end
          end
        else
          begin
            recover ();
            show_current_event ppf;
            restart ppf
          end)
  | x ->
      kill_program ();
      raise x

let execute_file_if_any () =
  let buffer = Buffer.create 128 in
  begin
    try
      let base = ".ocamldebug" in
      let file =
        if Sys.file_exists base then
          base
        else
          Filename.concat (Sys.getenv "HOME") base in
      let ch = open_in file in
      fprintf Format.std_formatter "Executing file %s@." file;
      while true do
        let line = string_trim (input_line ch) in
        if line <> ""  && line.[0] <> '#' then begin
          Buffer.add_string buffer line;
          Buffer.add_char buffer '\n'
        end
      done;
    with _ -> ()
  end;
  let len = Buffer.length buffer in
  if len > 0 then
    let commands = Buffer.sub buffer 0 (pred len) in
    line_loop Format.std_formatter (Lexing.from_string commands)

let toplevel_loop () =
  interactif := false;
  current_prompt := "";
  execute_file_if_any ();
  interactif := true;
  current_prompt := debugger_prompt;
  protect Format.std_formatter loop loop

(* Parsing of command-line arguments *)

exception Found_program_name

let anonymous s =
  program_name := Unix_tools.make_absolute s; raise Found_program_name
let add_include d =
  default_load_path :=
    Misc.expand_directory Config.standard_library d :: !default_load_path
let set_socket s =
  socket_name := s
let set_checkpoints n =
  checkpoint_max_count := n
let set_directory dir =
  Sys.chdir dir
let print_version () =
  printf "The OCaml debugger, version %s@." Sys.ocaml_version;
  exit 0;
;;
let print_version_num () =
  printf "%s@." Sys.ocaml_version;
  exit 0;
;;

let speclist = [
   "-c", Arg.Int set_checkpoints,
      "<count>  Set max number of checkpoints kept";
   "-cd", Arg.String set_directory,
      "<dir>  Change working directory";
   "-emacs", Arg.Tuple [Arg.Set emacs; Arg.Set machine_readable],
      "For running the debugger under emacs; implies -machine-readable";
   "-I", Arg.String add_include,
      "<dir>  Add <dir> to the list of include directories";
   "-machine-readable", Arg.Set machine_readable,
      "Print information in a format more suitable for machines";
   "-s", Arg.String set_socket,
      "<filename>  Set the name of the communication socket";
   "-version", Arg.Unit print_version,
      " Print version and exit";
   "-vnum", Arg.Unit print_version_num,
      " Print version number and exit";
   ]

let function_placeholder () =
  raise Not_found

let main () =
  Callback.register "Debugger.function_placeholder" function_placeholder;
  try
    socket_name :=
      (match Sys.os_type with
        "Win32" ->
          (Unix.string_of_inet_addr Unix.inet_addr_loopback)^
          ":"^
          (string_of_int (10000 + ((Unix.getpid ()) mod 10000)))
      | _ -> Filename.concat (Filename.get_temp_dir_name ())
                                ("camldebug" ^ (string_of_int (Unix.getpid ())))
      );
    begin try
      Arg.parse speclist anonymous "";
      Arg.usage speclist
        "No program name specified\n\
         Usage: ocamldebug [options] <program> [arguments]\n\
         Options are:";
      exit 2
    with Found_program_name ->
      for j = !Arg.current + 1 to Array.length Sys.argv - 1 do
        arguments := !arguments ^ " " ^ (Filename.quote Sys.argv.(j))
      done
    end;
    printf "\tOCaml Debugger version %s@.@." Config.version;
    Config.load_path := !default_load_path;
    Clflags.recursive_types := true;    (* Allow recursive types. *)
    toplevel_loop ();                   (* Toplevel. *)
    kill_program ();
    exit 0
  with
    Toplevel ->
      exit 2
  | Env.Error e ->
      eprintf "Debugger [version %s] environment error:@ @[@;" Config.version;
      Env.report_error err_formatter e;
      eprintf "@]@.";
      exit 2
  | Cmi_format.Error e ->
      eprintf "Debugger [version %s] environment error:@ @[@;" Config.version;
      Cmi_format.report_error err_formatter e;
      eprintf "@]@.";
      exit 2

let _ =
  Printexc.catch (Unix.handle_unix_error main) ()
