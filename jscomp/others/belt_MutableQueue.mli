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
(* Adapted significantly by BuckleScript Authors *)
(** First-in first-out queues.

    This module implements queues (FIFOs), with in-place modification.
*)

type 'a t
(** The type of queues containing elements of type ['a]. *)

val make: unit -> 'a t
(** @return a new queue, initially empty. *)

val clear: 'a t -> unit
(** Discard all elements from the queue. *)

val isEmpty: 'a t -> bool
(** @return [true] if the given queue is empty, [false] otherwise. *)

val fromArray: 'a array -> 'a t
(** [fromArray a] is equivalent to [Array.forEach a (add q a)] *)    

val add: 'a t -> 'a -> unit
(** [add q x] adds the element [x] at the end of the queue [q]. *)
    
val peek: 'a t -> 'a option
(** [peekOpt q] returns the first element in queue [q], without removing
    it from the queue. *)

val peekUndefined: 'a t -> 'a Js.undefined
(** [peekUndefined q] returns [undefined] if not found *)
val peekExn: 'a t -> 'a 
(** [peekExn q]

    {b raise} an exception if [q] is empty *)

val pop: 'a t -> 'a option 
(** [pop q] removes and returns the first element in queue [q].*)

val popUndefined: 'a t -> 'a Js.undefined
(** [popUndefined q] removes and returns the first element in queue [q].
    it will return undefined if it is already empty
*)

val popExn: 'a t -> 'a 
(** [popExn q]

    {b raise} an exception if [q] is empty
*)
  
val copy: 'a t -> 'a t
(** [copy q]

    @return a fresh queue
*)

val size: 'a t -> int
(** @return the number of elements in a queue. *)

val mapU: 'a t -> ('a -> 'b [@bs]) -> 'b t
val map: 'a t -> ('a -> 'b ) -> 'b t

val forEachU: 'a t -> ('a -> unit [@bs]) -> unit
val forEach: 'a t -> ('a -> unit ) -> unit
(** [forEach q f] applies [f] in turn to all elements of [q],
    from the least recently entered to the most recently entered.
    The queue itself is unchanged. *)

val reduceU: 'a t -> 'b -> ('b -> 'a -> 'b [@bs])  ->  'b
val reduce: 'a t -> 'b -> ('b -> 'a -> 'b )  ->  'b
(** [reduce q accu f] is equivalent to [List.reduce l accu f],
    where [l] is the list of [q]'s elements. The queue remains
    unchanged. *)

val transfer: 'a t -> 'a t -> unit
(** [transfer q1 q2] adds all of [q1]'s elements at the end of
    the queue [q2], then clears [q1]. It is equivalent to the
    sequence [forEach (fun x -> add x q2) q1; clear q1], but runs
    in constant time. *)

val toArray: 'a t -> 'a array    
(** First added will be in the beginning of the array *)
