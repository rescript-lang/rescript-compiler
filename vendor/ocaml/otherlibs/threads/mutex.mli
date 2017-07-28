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

(** Locks for mutual exclusion.

   Mutexes (mutual-exclusion locks) are used to implement critical sections
   and protect shared mutable data structures against concurrent accesses.
   The typical use is (if [m] is the mutex associated with the data structure
   [D]):
   {[
     Mutex.lock m;
     (* Critical section that operates over D *);
     Mutex.unlock m
   ]}
*)

type t
(** The type of mutexes. *)

val create : unit -> t
(** Return a new mutex. *)

val lock : t -> unit
(** Lock the given mutex. Only one thread can have the mutex locked
   at any time. A thread that attempts to lock a mutex already locked
   by another thread will suspend until the other thread unlocks
   the mutex. *)

val try_lock : t -> bool
(** Same as {!Mutex.lock}, but does not suspend the calling thread if
   the mutex is already locked: just return [false] immediately
   in that case. If the mutex is unlocked, lock it and
   return [true]. *)

val unlock : t -> unit
(** Unlock the given mutex. Other threads suspended trying to lock
   the mutex will restart. *)
