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

(** Lightweight threads. *)

type t
(** The type of thread handles. *)


(** {6 Thread creation and termination} *)

val create : ('a -> 'b) -> 'a -> t
(** [Thread.create funct arg] creates a new thread of control,
   in which the function application [funct arg]
   is executed concurrently with the other threads of the program.
   The application of [Thread.create]
   returns the handle of the newly created thread.
   The new thread terminates when the application [funct arg]
   returns, either normally or by raising an uncaught exception.
   In the latter case, the exception is printed on standard error,
   but not propagated back to the parent thread. Similarly, the
   result of the application [funct arg] is discarded and not
   directly accessible to the parent thread. *)

val self : unit -> t
(** Return the thread currently executing. *)

external id : t -> int = "thread_id"
(** Return the identifier of the given thread. A thread identifier
   is an integer that identifies uniquely the thread.
   It can be used to build data structures indexed by threads. *)

val exit : unit -> unit
(** Terminate prematurely the currently executing thread. *)

val kill : t -> unit
(** Terminate prematurely the thread whose handle is given.
   This functionality is available only with bytecode-level threads. *)

(** {6 Suspending threads} *)

val delay : float -> unit
(** [delay d] suspends the execution of the calling thread for
   [d] seconds. The other program threads continue to run during
   this time. *)

val join : t -> unit
(** [join th] suspends the execution of the calling thread
   until the thread [th] has terminated. *)

val wait_read : Unix.file_descr -> unit
(** See {!Thread.wait_write}.*)

val wait_write : Unix.file_descr -> unit
(** Suspend the execution of the calling thread until at least
   one character is available for reading ({!Thread.wait_read}) or
   one character can be written without blocking ([wait_write])
   on the given Unix file descriptor. *)

val wait_timed_read : Unix.file_descr -> float -> bool
(** See {!Thread.wait_timed_write}.*)

val wait_timed_write : Unix.file_descr -> float -> bool
(** Same as {!Thread.wait_read} and {!Thread.wait_write}, but wait for at most
   the amount of time given as second argument (in seconds).
   Return [true] if the file descriptor is ready for input/output
   and [false] if the timeout expired. *)

val select :
  Unix.file_descr list -> Unix.file_descr list -> Unix.file_descr list ->
    float ->
    Unix.file_descr list * Unix.file_descr list * Unix.file_descr list
(** Suspend the execution of the calling thead until input/output
   becomes possible on the given Unix file descriptors.
   The arguments and results have the same meaning as for
   {!Unix.select}. *)

val wait_pid : int -> int * Unix.process_status
(** [wait_pid p] suspends the execution of the calling thread
   until the Unix process specified by the process identifier [p]
   terminates. A pid [p] of [-1] means wait for any child.
   A pid of [0] means wait for any child in the same process group
   as the current process. Negative pid arguments represent
   process groups. Returns the pid of the child caught and
   its termination status, as per {!Unix.wait}. *)

val wait_signal : int list -> int
(** [wait_signal sigs] suspends the execution of the calling thread
   until the process receives one of the signals specified in the
   list [sigs].  It then returns the number of the signal received.
   Signal handlers attached to the signals in [sigs] will not
   be invoked.  Do not call [wait_signal] concurrently
   from several threads on the same signals. *)

val yield : unit -> unit
(** Re-schedule the calling thread without suspending it.
   This function can be used to give scheduling hints,
   telling the scheduler that now is a good time to
   switch to other threads. *)

(**/**)

(** {6 Synchronization primitives}

   The following primitives provide the basis for implementing
   synchronization functions between threads. Their direct use is
   discouraged, as they are very low-level and prone to race conditions
   and deadlocks. The modules {!Mutex}, {!Condition} and {!Event}
   provide higher-level synchronization primitives. *)

val critical_section : bool ref
(** Setting this reference to [true] deactivate thread preemption
   (the timer interrupt that transfers control from thread to thread),
   causing the current thread to run uninterrupted until
   [critical_section] is reset to [false] or the current thread
   explicitely relinquishes control using [sleep], [delay],
   [wait_inchan] or [wait_descr]. *)

val sleep : unit -> unit
(** Suspend the calling thread until another thread reactivates it
   using {!Thread.wakeup}. Just before suspending the thread,
   {!Thread.critical_section} is reset to [false]. Resetting
   {!Thread.critical_section} and suspending the calling thread is an
   atomic operation. *)

val wakeup : t -> unit
(** Reactivate the given thread. After the call to [wakeup],
   the suspended thread will resume execution at some future time. *)
