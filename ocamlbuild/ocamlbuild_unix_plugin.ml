(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Nicolas Pouillard *)
open Format
open Ocamlbuild_pack
open My_unix

let report_error f =
  function
  | Unix.Unix_error(err, fun_name, arg) ->
      fprintf f "%s: %S failed" Sys.argv.(0) fun_name;
      if String.length arg > 0 then
        fprintf f " on %S" arg;
      fprintf f ": %s" (Unix.error_message err)
  | exn -> raise exn

let mkstat unix_stat x =
  let st =
    try unix_stat x
    with Unix.Unix_error _ as e -> raise (Sys_error (My_std.sbprintf "%a" report_error e))
  in
  { stat_key = sprintf "(%d,%d)" st.Unix.st_dev st.Unix.st_ino;
    stat_file_kind =
      match st.Unix.st_kind with
      | Unix.S_LNK -> FK_link
      | Unix.S_DIR -> FK_dir
      | Unix.S_CHR | Unix.S_BLK | Unix.S_FIFO | Unix.S_SOCK -> FK_other
      | Unix.S_REG -> FK_file }

let is_link s = (Unix.lstat s).Unix.st_kind = Unix.S_LNK

let at_exit_once callback =
  let pid = Unix.getpid () in
  at_exit begin fun () ->
    if pid = Unix.getpid () then callback ()
  end

let run_and_open s kont =
  let ic = Unix.open_process_in s in
  let close () =
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
        failwith (Printf.sprintf "Error while running: %s" s) in
  let res = try
      kont ic
    with e -> (close (); raise e)
  in close (); res

let stdout_isatty () =
  Unix.isatty Unix.stdout &&
    try Unix.getenv "TERM" <> "dumb" with Not_found -> true

let execute_many =
  let exit i = raise (My_std.Exit_with_code i) in
  let exit = function
    | Ocamlbuild_executor.Subcommand_failed -> exit Exit_codes.rc_executor_subcommand_failed
    | Ocamlbuild_executor.Subcommand_got_signal -> exit Exit_codes.rc_executor_subcommand_got_signal
    | Ocamlbuild_executor.Io_error -> exit Exit_codes.rc_executor_io_error
    | Ocamlbuild_executor.Exceptionl_condition -> exit Exit_codes.rc_executor_excetptional_condition
  in
  Ocamlbuild_executor.execute ~exit

(* Ocamlbuild code assumes throughout that [readlink] will return a file name
   relative to the current directory. Let's make it so. *)
let myunixreadlink x =
  let y = Unix.readlink x in
  if Filename.is_relative y then
    Filename.concat (Filename.dirname x) y
  else
    y

let setup () =
  implem.is_degraded <- false;
  implem.stdout_isatty <- stdout_isatty;
  implem.gettimeofday <- Unix.gettimeofday;
  implem.report_error <- report_error;
  implem.execute_many <- execute_many;
  implem.readlink <- myunixreadlink;
  implem.run_and_open <- run_and_open;
  implem.at_exit_once <- at_exit_once;
  implem.is_link <- is_link;
  implem.stat <- mkstat Unix.stat;
  implem.lstat <- mkstat Unix.lstat;;
