(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(* User-level threads *)

type t

let critical_section = ref false

type resumption_status =
    Resumed_wakeup
  | Resumed_delay
  | Resumed_join
  | Resumed_io
  | Resumed_select of
      Unix.file_descr list * Unix.file_descr list * Unix.file_descr list
  | Resumed_wait of int * Unix.process_status

(* to avoid warning *)
let _ = [Resumed_wakeup; Resumed_delay; Resumed_join;
         Resumed_io; Resumed_select ([], [], []);
         Resumed_wait (0, Unix.WEXITED 0)]

(* It is mucho important that the primitives that reschedule are called
   through an ML function call, not directly. That's because when such a
   primitive returns, the bytecode interpreter is only semi-obedient:
   it takes sp from the new thread, but keeps pc from the old thread.
   But that's OK if all calls to rescheduling primitives are immediately
   followed by a RETURN operation, which will restore the correct pc
   from the stack. Furthermore, the RETURNs must all have the same
   frame size, which means that both the primitives and their ML wrappers
   must take exactly one argument. *)

external thread_initialize : unit -> unit = "thread_initialize"
external thread_initialize_preemption : unit -> unit
   = "thread_initialize_preemption"
external thread_new : (unit -> unit) -> t = "thread_new"
external thread_yield : unit -> unit = "thread_yield"
external thread_request_reschedule : unit -> unit = "thread_request_reschedule"
external thread_sleep : unit -> unit = "thread_sleep"
external thread_wait_read : Unix.file_descr -> unit = "thread_wait_read"
external thread_wait_write : Unix.file_descr -> unit = "thread_wait_write"
external thread_wait_timed_read :
  Unix.file_descr * float -> resumption_status     (* remember: 1 arg *)
  = "thread_wait_timed_read"
external thread_wait_timed_write :
  Unix.file_descr * float -> resumption_status     (* remember: 1 arg *)
  = "thread_wait_timed_write"
external thread_select :
  Unix.file_descr list * Unix.file_descr list *          (* remember: 1 arg *)
  Unix.file_descr list * float -> resumption_status
  = "thread_select"
external thread_join : t -> unit = "thread_join"
external thread_delay : float -> unit = "thread_delay"
external thread_wait_pid : int -> resumption_status = "thread_wait_pid"
external thread_wakeup : t -> unit = "thread_wakeup"
external thread_self : unit -> t = "thread_self"
external thread_kill : t -> unit = "thread_kill"
external thread_uncaught_exception : exn -> unit = "thread_uncaught_exception"

external id : t -> int = "thread_id"

(* In sleep() below, we rely on the fact that signals are detected
   only at function applications and beginning of loops,
   making all other operations atomic. *)

let yield () = thread_yield()
let sleep () = critical_section := false; thread_sleep()
let delay duration = thread_delay duration
let join th = thread_join th
let wakeup pid = thread_wakeup pid
let self () = thread_self()
let kill pid = thread_kill pid
let exit () = thread_kill(thread_self())

let select_aux arg = thread_select arg

let select readfds writefds exceptfds delay =
  match select_aux (readfds, writefds, exceptfds, delay) with
    Resumed_select(r, w, e) -> (r, w, e)
  | _ -> ([], [], [])

let wait_read fd = thread_wait_read fd
let wait_write fd = thread_wait_write fd

let wait_timed_read_aux arg = thread_wait_timed_read arg
let wait_timed_write_aux arg = thread_wait_timed_write arg

let wait_timed_read fd delay =
  match wait_timed_read_aux (fd, delay) with Resumed_io -> true | _ -> false

let wait_timed_write fd delay =
  match wait_timed_write_aux (fd, delay) with Resumed_io -> true | _ -> false

let wait_pid_aux pid = thread_wait_pid pid

let wait_pid pid =
  match wait_pid_aux pid with
    Resumed_wait(pid, status) -> (pid, status)
  | _ -> invalid_arg "Thread.wait_pid"

let wait_signal sigs =
  let gotsig = ref 0 in
  let self = thread_self() in
  let sighandler s = gotsig := s; wakeup self in
  let oldhdlrs =
    List.map (fun s -> Sys.signal s (Sys.Signal_handle sighandler)) sigs in
  if !gotsig = 0 then sleep();
  List.iter2 Sys.set_signal sigs oldhdlrs;
  !gotsig

(* For Thread.create, make sure the function passed to thread_new
   always terminates by calling Thread.exit. *)

let create fn arg =
  thread_new
    (fun () ->
      try
        fn arg; exit()
      with x ->
        flush stdout; flush stderr;
        thread_uncaught_exception x;
        exit())

(* Preemption *)

let preempt signal =
  if !critical_section then () else thread_request_reschedule()

(* Initialization of the scheduler *)

let _ =
  thread_initialize();
  Sys.set_signal Sys.sigvtalrm (Sys.Signal_handle preempt);
  thread_initialize_preemption()
