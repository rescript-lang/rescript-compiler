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

(* Manage the loading of the program *)

open Int64ops
open Unix
open Unix_tools
open Debugger_config
open Primitives
open Parameters
open Input_handling
open Question
open Program_loading
open Time_travel

(*** Connection opening and control. ***)

(* Name of the file if the socket is in the unix domain.*)
let file_name = ref (None : string option)

(* Default connection handler. *)
let buffer = Bytes.create 1024
let control_connection pid fd =
  if (read fd.io_fd buffer 0 1024) = 0 then
    forget_process fd pid
  else begin
    prerr_string "Garbage data from process ";
    prerr_int pid;
    prerr_endline ""
    end

(* Accept a connection from another process. *)
let accept_connection continue fd =
  let (sock, _) = accept fd.io_fd in
  let io_chan = io_channel_of_descr sock in
  let pid = input_binary_int io_chan.io_in in
  if pid = -1 then begin
    let pid' = input_binary_int io_chan.io_in in
    new_checkpoint pid' io_chan;
    Input_handling.add_file io_chan (control_connection pid');
    continue ()
    end
  else begin
    if set_file_descriptor pid io_chan then
      Input_handling.add_file io_chan (control_connection pid)
    end

(* Initialize the socket. *)
let open_connection address continue =
  try
    let (sock_domain, sock_address) = convert_address address in
      file_name :=
        (match sock_address with
           ADDR_UNIX file ->
             Some file
         | _ ->
             None);
      let sock = socket sock_domain SOCK_STREAM 0 in
        (try
           bind sock sock_address;
           setsockopt sock SO_REUSEADDR true;
           listen sock 3;
           connection := io_channel_of_descr sock;
           Input_handling.add_file !connection (accept_connection continue);
           connection_opened := true
         with x -> close sock; raise x)
  with
    Failure _ -> raise Toplevel
  | (Unix_error _) as err -> report_error err; raise Toplevel

(* Close the socket. *)
let close_connection () =
  if !connection_opened then begin
    connection_opened := false;
    Input_handling.remove_file !connection;
    close_io !connection;
    match !file_name with
      Some file ->
        unlink file
    | None ->
        ()
    end

(*** Kill program. ***)
let loaded = ref false

let kill_program () =
  Breakpoints.remove_all_breakpoints ();
  History.empty_history ();
  kill_all_checkpoints ();
  loaded := false;
  close_connection ()

let ask_kill_program () =
  if not !loaded then
    true
  else
    let answer = yes_or_no "A program is being debugged already. Kill it" in
      if answer then
        kill_program ();
      answer

(*** Program loading and initializations. ***)

let initialize_loading () =
  if !debug_loading then begin
    prerr_endline "Loading debugging information...";
    Printf.fprintf Pervasives.stderr "\tProgram: [%s]\n%!" !program_name;
  end;
  begin try access !program_name [F_OK]
  with Unix_error _ ->
    prerr_endline "Program not found.";
    raise Toplevel;
  end;
  Symbols.read_symbols !program_name;
  Config.load_path := !Config.load_path @ !Symbols.program_source_dirs;
  Envaux.reset_cache ();
  if !debug_loading then
    prerr_endline "Opening a socket...";
  open_connection !socket_name
    (function () ->
      go_to _0;
      Symbols.set_all_events();
      exit_main_loop ())

(* Ensure the program is already loaded. *)
let ensure_loaded () =
  if not !loaded then begin
    print_string "Loading program... ";
    flush Pervasives.stdout;
    if !program_name = "" then begin
      prerr_endline "No program specified.";
      raise Toplevel
    end;
    try
      initialize_loading();
      !launching_func ();
      if !debug_loading then
        prerr_endline "Waiting for connection...";
      main_loop ();
      loaded := true;
      prerr_endline "done."
    with
      x ->
        kill_program();
        raise x
  end
