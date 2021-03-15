[@@@bs.config { flags = [|"-bs-no-cross-module-opt"; |]}]
#2 "stdlib/sys.mlp"
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

(* WARNING: sys.ml is generated from sys.mlp.  DO NOT EDIT sys.ml or
   your changes will be lost.
*)

type backend_type =
  | Native
  | Bytecode
  | Other of string
(* System interface *)

(* external get_config: unit -> string * int * bool = "caml_sys_get_config" *)
external get_argv: unit -> string * string array = "caml_sys_get_argv"
external big_endian : unit -> bool = "%big_endian"
external word_size : unit -> int = "%word_size"
external int_size : unit -> int = "%int_size"
(* external max_wosize : unit -> int = "%max_wosize" *)
external unix : unit -> bool = "%ostype_unix"
external win32 : unit -> bool = "%ostype_win32"
external cygwin : unit -> bool = "%ostype_cygwin"
external get_backend_type : unit -> backend_type = "%backend_type"

let (executable_name, argv) = get_argv()
#if BS 
external get_os_type : unit -> string = "#os_type"
let os_type = get_os_type ()
#else
let (os_type, _, _) = get_config()
#end
let backend_type = get_backend_type ()
let big_endian = big_endian ()
let word_size = word_size ()
let int_size = int_size ()
let unix = unix ()
let win32 = win32 ()
let cygwin = cygwin ()
#if BS 
let max_array_length = 2147483647 (* 2^ 31 - 1 *)
let max_string_length = 2147483647
#else
let max_array_length = max_wosize ()
let max_string_length = word_size / 8 * max_array_length - 1
#end
external runtime_variant : unit -> string = "caml_runtime_variant"
external runtime_parameters : unit -> string = "caml_runtime_parameters"

external file_exists: string -> bool = "caml_sys_file_exists"
external is_directory : string -> bool = "caml_sys_is_directory"
external remove: string -> unit = "caml_sys_remove"
external rename : string -> string -> unit = "caml_sys_rename"
external getenv: string -> string = "caml_sys_getenv"

#if BS 
external getEnv : 'a -> string -> string option = "" [@@bs.get_index] 
let getenv_opt s =
    match [%external process ] with 
    | None -> None
    | Some x -> getEnv x##env s
#else
let getenv_opt s =
  (* TODO: expose a non-raising primitive directly. *)
  try Some (getenv s)
  with Not_found -> None
#end

external command: string -> int = "caml_sys_system_command"
external time: unit -> (float [@unboxed]) =
  "caml_sys_time" "caml_sys_time_unboxed" [@@noalloc]
external chdir: string -> unit = "caml_sys_chdir"
external getcwd: unit -> string = "caml_sys_getcwd"
external readdir : string -> string array = "caml_sys_read_directory"

let interactive = ref false

type signal_behavior =
    Signal_default
  | Signal_ignore
  | Signal_handle of (int -> unit)

external signal : int -> signal_behavior -> signal_behavior
                = "caml_install_signal_handler"

let set_signal sig_num sig_beh = ignore(signal sig_num sig_beh)

let sigabrt = -1
let sigalrm = -2
let sigfpe = -3
let sighup = -4
let sigill = -5
let sigint = -6
let sigkill = -7
let sigpipe = -8
let sigquit = -9
let sigsegv = -10
let sigterm = -11
let sigusr1 = -12
let sigusr2 = -13
let sigchld = -14
let sigcont = -15
let sigstop = -16
let sigtstp = -17
let sigttin = -18
let sigttou = -19
let sigvtalrm = -20
let sigprof = -21
let sigbus = -22
let sigpoll = -23
let sigsys = -24
let sigtrap = -25
let sigurg = -26
let sigxcpu = -27
let sigxfsz = -28

exception Break

let catch_break on =
  if on then
    set_signal sigint (Signal_handle(fun _ -> raise Break))
  else
    set_signal sigint Signal_default

#if BS
let enable_runtime_warnings : bool -> unit = fun _ -> () 
let runtime_warnings_enabled : unit -> bool = fun _ -> false
#else
external enable_runtime_warnings: bool -> unit =
  "caml_ml_enable_runtime_warnings"
external runtime_warnings_enabled: unit -> bool =
  "caml_ml_runtime_warnings_enabled"
#end
(* The version string is found in file ../VERSION *)

let ocaml_version = "4.06.2+BS"

(* Optimization *)

external opaque_identity : 'a -> 'a = "%opaque"
