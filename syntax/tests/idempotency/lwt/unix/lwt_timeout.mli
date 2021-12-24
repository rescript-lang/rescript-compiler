(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(** Cancelable timeouts. *)

type t

val create : int -> (unit -> unit) -> t
(** [Lwt_timeout.create n f] creates a new timeout object with duration [n]
    seconds. [f] is the {e action}, a function to be called once the timeout
    expires. [f] should not raise exceptions.

    The timeout is not started until {!Lwt_timeout.start} is called on it. *)

val start : t -> unit
(** Starts the given timeout.

    Starting a timeout that has already been started has the same effect as
    stopping it, and then restarting it with its original duration. So,
    suppose you have [timeout] with a duration of three seconds, which was
    started two seconds ago. The next call to its action is scheduled for one
    second in the future. Calling [Lwt_timeout.start timeout] at this point
    cancels this upcoming action call, and schedules a call three seconds from
    now. *)

val stop : t -> unit
(** Stops (cancels) the given timeout. *)

val change : t -> int -> unit
(** Changes the duration of the given timeout.

    If the timeout has already been started, it is stopped, and restarted with
    its new duration. This is similar to how {!Lwt_timeout.start} works on a
    timeout that has already been started. *)

val set_exn_handler : (exn -> unit) -> unit
(** [Lwt_timeout.set_exn_handler f] sets the handler to be used for exceptions
    raised by timeout actions. Recall that actions are not allowed to raise
    exceptions. If they do raise an exception [exn] despite this, [f exn] is
    called.

    The default behavior of [f exn], set by [Lwt_timeout] on program startup, is
    to pass [exn] to [!]{!Lwt.async_exception_hook}. The default behavior of
    {e that} is to terminate the process. *)
