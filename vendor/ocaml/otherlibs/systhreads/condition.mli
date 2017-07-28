(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(** Condition variables to synchronize between threads.

   Condition variables are used when one thread wants to wait until another
   thread has finished doing something: the former thread 'waits' on the
   condition variable, the latter thread 'signals' the condition when it
   is done. Condition variables should always be protected by a mutex.
   The typical use is (if [D] is a shared data structure, [m] its mutex,
   and [c] is a condition variable):
   {[
     Mutex.lock m;
     while (* some predicate P over D is not satisfied *) do
       Condition.wait c m
     done;
     (* Modify D *)
     if (* the predicate P over D is now satified *) then Condition.signal c;
     Mutex.unlock m
   ]}
*)

type t
(** The type of condition variables. *)

val create : unit -> t
(** Return a new condition variable. *)

val wait : t -> Mutex.t -> unit
(** [wait c m] atomically unlocks the mutex [m] and suspends the
   calling process on the condition variable [c]. The process will
   restart after the condition variable [c] has been signalled.
   The mutex [m] is locked again before [wait] returns. *)

val signal : t -> unit
(** [signal c] restarts one of the processes waiting on the
   condition variable [c]. *)

val broadcast : t -> unit
(** [broadcast c] restarts all processes waiting on the
   condition variable [c]. *)
