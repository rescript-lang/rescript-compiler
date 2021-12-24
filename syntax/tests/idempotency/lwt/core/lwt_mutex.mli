(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(** Cooperative locks for mutual exclusion *)

type t
  (** Type of Lwt mutexes *)

val create : unit -> t
  (** [create ()] creates a new mutex, which is initially unlocked *)

val lock : t -> unit Lwt.t
  (** [lock mutex] lockcs the mutex, that is:

      - if the mutex is unlocked, then it is marked as locked and
        {!lock} returns immediately

      - if it is locked, then {!lock} waits for all threads waiting on
        the mutex to terminate, then it resumes when the last one
        unlocks the mutex

      Note: threads are woken up in the same order they try to lock the
      mutex *)

val unlock : t -> unit
  (** [unlock mutex] unlock the mutex if no threads is waiting on
      it. Otherwise it will eventually removes the first one and
      resumes it. *)

val is_locked : t -> bool
  (** [locked mutex] returns whether [mutex] is currently locked *)

val is_empty : t -> bool
  (** [is_empty mutex] returns [true] if they are no thread waiting on
      the mutex, and [false] otherwise *)

val with_lock : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [with_lock lock f] is used to lock a mutex within a block scope.
      The function [f ()] is called with the mutex locked, and its
      result is returned from the call to [with_lock]. If an exception
      is raised from f, the mutex is also unlocked before the scope of
      [with_lock] is exited. *)
