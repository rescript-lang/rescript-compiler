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
(* Adapted siginifcantly by BuckleScript Authors *)
(** First-in first-out queues.

    This module implements queues (FIFOs), with in-place modification.
*)

type 'a t
(** The type of queues containing elements of type ['a]. *)

val clear : 'a t -> unit
(** Discard all elements from a queue. *)

val create : unit -> 'a t
(** Return a new queue, initially empty. *)

val addDone : 'a t -> 'a -> unit
(** [addDone q x] adds the element [x] at the end of the queue [q]. *)
val add : 'a t -> 'a  -> 'a t
    
val peek : 'a t -> 'a option
(** [peekOpt q] returns the first element in queue [q], without removing
    it from the queue. *)

val peekNull : 'a t -> 'a Js.null
(** [peekNull q] returns the first element in queue [q], without removing
    it from the queue. *)
val peekExn : 'a t -> 'a 

val pop : 'a t -> 'a option 
(** [pop q] removes and returns the first element in queue [q].*)

val popNull : 'a t -> 'a Js.null
(** [popNull q] removes and returns the first element in queue [q].*)

val popExn : 'a t -> 'a 

val copy : 'a t -> 'a t
(** Return a copy of the given queue. *)

val isEmpty : 'a t -> bool
(** Return [true] if the given queue is empty, [false] otherwise. *)

val length : 'a t -> int
(** Return the number of elements in a queue. *)

val forEach : 'a t -> ('a -> unit [@bs]) -> unit
(** [reduce f q] applies [f] in turn to all elements of [q],
    from the least recently entered to the most recently entered.
    The queue itself is unchanged. *)

val reduce : 'a t -> 'b -> ('b -> 'a -> 'b [@bs])  ->  'b
(** [reduce q accu f] is equivalent to [List.reduce f accu l],
    where [l] is the list of [q]'s elements. The queue remains
    unchanged. *)

val transfer : 'a t -> 'a t -> unit
(** [transfer q1 q2] adds all of [q1]'s elements at the end of
    the queue [q2], then clears [q1]. It is equivalent to the
    sequence [iter (fun x -> add x q2) q1; clear q1], but runs
    in constant time. *)

val toArray : 'a t -> 'a array    
(** First added will be in the beginning of the array *)
