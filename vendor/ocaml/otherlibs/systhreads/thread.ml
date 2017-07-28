(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*  Xavier Leroy and Pascal Cuoq, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(* User-level threads *)

type t

external thread_initialize : unit -> unit = "caml_thread_initialize"
external thread_cleanup : unit -> unit = "caml_thread_cleanup"
external thread_new : (unit -> unit) -> t = "caml_thread_new"
external thread_uncaught_exception : exn -> unit =
            "caml_thread_uncaught_exception"

external yield : unit -> unit = "caml_thread_yield"
external self : unit -> t = "caml_thread_self"
external id : t -> int = "caml_thread_id"
external join : t -> unit = "caml_thread_join"
external exit : unit -> unit = "caml_thread_exit"

(* For new, make sure the function passed to thread_new never
   raises an exception. *)

let create fn arg =
  thread_new
    (fun () ->
      try
        fn arg; ()
      with exn ->
             flush stdout; flush stderr;
             thread_uncaught_exception exn)

(* Thread.kill is currently not implemented due to problems with
   cleanup handlers on several platforms *)

let kill th = invalid_arg "Thread.kill: not implemented"

(* Preemption *)

let preempt signal = yield()

(* Initialization of the scheduler *)

let preempt_signal =
  match Sys.os_type with
  | "Win32" -> Sys.sigterm
  | _       -> Sys.sigvtalrm

let _ =
  Sys.set_signal preempt_signal (Sys.Signal_handle preempt);
  thread_initialize();
  at_exit
    (fun () ->
        thread_cleanup();
        (* In case of DLL-embedded OCaml the preempt_signal handler
           will point to nowhere after DLL unloading and an accidental
           preempt_signal will crash the main program. So restore the
           default handler. *)
        Sys.set_signal preempt_signal Sys.Signal_default
    )

(* Wait functions *)

let delay time = ignore(Unix.select [] [] [] time)

let wait_read fd = ()
let wait_write fd = ()

let wait_timed_read fd d =
  match Unix.select [fd] [] [] d with ([], _, _) -> false | (_, _, _) -> true
let wait_timed_write fd d =
  match Unix.select [] [fd] [] d with (_, [], _) -> false | (_, _, _) -> true
let select = Unix.select

let wait_pid p = Unix.waitpid [] p

external sigmask : Unix.sigprocmask_command -> int list -> int list
   = "caml_thread_sigmask"
external wait_signal : int list -> int = "caml_wait_signal"
