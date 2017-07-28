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

(* Program loading *)

open Unix
open Debugger_config
open Parameters
open Input_handling

(*** Debugging. ***)

let debug_loading = ref false

(*** Load a program. ***)

(* Function used for launching the program. *)
let launching_func = ref (function () -> ())

let load_program () =
  !launching_func ();
  main_loop ()

(*** Launching functions. ***)

(* Returns a command line prefix to set environment for the debuggee *)
let get_unix_environment () =
  let f (vname, vvalue) =
    Printf.sprintf "%s=%s " vname (Filename.quote vvalue)
  in
  String.concat "" (List.map f !Debugger_config.environment)
;;

(* Notes:
   1. This quoting is not the same as [Filename.quote] because the "set"
      command is a shell built-in and its quoting rules are different
      from regular commands.
   2. Microsoft's documentation omits the double-quote from the list
      of characters that need quoting, but that is a mistake (unquoted
      quotes are included in the value, but they alter the quoting of
      characters between them).
   Reference: http://msdn.microsoft.com/en-us/library/bb490954.aspx
 *)
let quote_for_windows_shell s =
  let b = Buffer.create (20 + String.length s) in
  for i = 0 to String.length s - 1 do
    begin match s.[i] with
    | '<' | '>' | '|' | '&' | '^' | '\"' ->
      Buffer.add_char b '^';
    | _ -> ()
    end;
    Buffer.add_char b s.[i];
  done;
  Buffer.contents b
;;

(* Returns a command line prefix to set environment for the debuggee *)
let get_win32_environment () =
  (* Note: no space before the & or Windows will add it to the value *)
  let f (vname, vvalue) =
    Printf.sprintf "set %s=%s&" vname (quote_for_windows_shell vvalue)
  in
  String.concat "" (List.map f !Debugger_config.environment)

(* A generic function for launching the program *)
let generic_exec_unix cmdline = function () ->
  if !debug_loading then
    prerr_endline "Launching program...";
  let child =
    try
      fork ()
    with x ->
      Unix_tools.report_error x;
      raise Toplevel in
  match child with
    0 ->
      begin try
         match fork () with
           0 -> (* Try to detach the process from the controlling terminal,
                   so that it does not receive SIGINT on ctrl-C. *)
                begin try ignore(setsid()) with Invalid_argument _ -> () end;
                execv shell [| shell; "-c"; cmdline() |]
         | _ -> exit 0
       with x ->
         Unix_tools.report_error x;
         exit 1
       end
  | _ ->
     match wait () with
       (_, WEXITED 0) -> ()
     | _ -> raise Toplevel

let generic_exec_win cmdline = function () ->
  if !debug_loading then
    prerr_endline "Launching program...";
  try ignore(create_process "cmd.exe" [| "/C"; cmdline() |] stdin stdout stderr)
  with x ->
    Unix_tools.report_error x;
    raise Toplevel

let generic_exec =
  match Sys.os_type with
    "Win32" -> generic_exec_win
  | _ -> generic_exec_unix

(* Execute the program by calling the runtime explicitly *)
let exec_with_runtime =
  generic_exec
    (function () ->
      match Sys.os_type with
        "Win32" ->
          (* This would fail on a file name with spaces
             but quoting is even worse because Unix.create_process
             thinks each command line parameter is a file.
             So no good solution so far *)
          Printf.sprintf "%sset CAML_DEBUG_SOCKET=%s& %s %s %s"
                     (get_win32_environment ())
                     !socket_name
                     runtime_program
                     !program_name
                     !arguments
      | _ ->
          Printf.sprintf "%sCAML_DEBUG_SOCKET=%s %s %s %s"
                     (get_unix_environment ())
                     !socket_name
                     (Filename.quote runtime_program)
                     (Filename.quote !program_name)
                     !arguments)

(* Excute the program directly *)
let exec_direct =
  generic_exec
    (function () ->
      match Sys.os_type with
        "Win32" ->
          (* See the comment above *)
          Printf.sprintf "%sset CAML_DEBUG_SOCKET=%s& %s %s"
                     (get_win32_environment ())
                     !socket_name
                     !program_name
                     !arguments
      | _ ->
          Printf.sprintf "%sCAML_DEBUG_SOCKET=%s %s %s"
                     (get_unix_environment ())
                     !socket_name
                     (Filename.quote !program_name)
                     !arguments)

(* Ask the user. *)
let exec_manual =
  function () ->
    print_newline ();
    print_string "Waiting for connection...";
    print_string ("(the socket is " ^ !socket_name ^ ")");
    print_newline ()

(*** Selection of the launching function. ***)

type launching_function = (unit -> unit)

let loading_modes =
  ["direct", exec_direct;
   "runtime", exec_with_runtime;
   "manual", exec_manual]

let set_launching_function func =
  launching_func := func

(* Initialization *)

let _ =
  set_launching_function exec_direct

(*** Connection. ***)

let connection = ref Primitives.std_io
let connection_opened = ref false
