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

(** Last-in first-out stacks.

   This module implements stacks (LIFOs), with in-place modification.
*)

type 'a t
(** The type of stacks containing elements of type ['a]. *)

exception Empty
(** Raised when {!Stack.pop} or {!Stack.top} is applied to an empty stack. *)


val create : unit -> 'a t
(** Return a new stack, initially empty. *)

val push : 'a -> 'a t -> unit
(** [push x s] adds the element [x] at the top of stack [s]. *)

val pop : 'a t -> 'a
(** [pop s] removes and returns the topmost element in stack [s],
   or raises {!Empty} if the stack is empty. *)

val top : 'a t -> 'a
(** [top s] returns the topmost element in stack [s],
   or raises {!Empty} if the stack is empty. *)

val clear : 'a t -> unit
(** Discard all elements from a stack. *)

val copy : 'a t -> 'a t
(** Return a copy of the given stack. *)

val is_empty : 'a t -> bool
(** Return [true] if the given stack is empty, [false] otherwise. *)

val length : 'a t -> int
(** Return the number of elements in a stack. Time complexity O(1) *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f s] applies [f] in turn to all elements of [s],
   from the element at the top of the stack to the element at the
   bottom of the stack. The stack itself is unchanged. *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** [fold f accu s] is [(f (... (f (f accu x1) x2) ...) xn)]
    where [x1] is the top of the stack, [x2] the second element,
    and [xn] the bottom element. The stack is unchanged.
    @since 4.03 *)
