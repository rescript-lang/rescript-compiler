(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(** Mutable sequence of elements (deprecated) *)

(** A sequence is an object holding a list of elements which support
    the following operations:

    - adding an element to the left or the right in time and space O(1)
    - taking an element from the left or the right in time and space O(1)
    - removing a previously added element from a sequence in time and space O(1)
    - removing an element while the sequence is being transversed.

   @deprecated This module should be an internal implementation detail of Lwt,
   and may be removed from the API at some point in the future. Use package
   {{:https://github.com/mirage/lwt-dllist} [lwt-dllist]} instead.
*)

[@@@ocaml.deprecated
" Use package lwt-dllist. See
   https://github.com/mirage/lwt-dllist"]

type 'a t
  (** Type of a sequence holding values of type ['a] *)

type 'a node
  (** Type of a node holding one value of type ['a] in a sequence *)

(** {2 Operation on nodes} *)

val get : 'a node -> 'a
  (** Returns the contents of a node *)

val set : 'a node -> 'a -> unit
  (** Change the contents of a node *)

val remove : 'a node -> unit
  (** Removes a node from the sequence it is part of. It does nothing
      if the node has already been removed. *)

(** {2 Operations on sequence} *)

val create : unit -> 'a t
  (** [create ()] creates a new empty sequence *)

val is_empty : 'a t -> bool
  (** Returns [true] iff the given sequence is empty *)

val length : 'a t -> int
  (** Returns the number of elements in the given sequence. This is a
      O(n) operation where [n] is the number of elements in the
      sequence. *)

val add_l : 'a -> 'a t -> 'a node
  (** [add_l x s] adds [x] to the left of the sequence [s] *)

val add_r : 'a -> 'a t -> 'a node
  (** [add_l x s] adds [x] to the right of the sequence [s] *)

exception Empty
  (** Exception raised by [take_l] and [tale_s] and when the sequence
      is empty *)

val take_l : 'a t -> 'a
  (** [take_l x s] remove and returns the leftmost element of [s]

      @raise Empty if the sequence is empty *)

val take_r : 'a t -> 'a
  (** [take_l x s] remove and returns the rightmost element of [s]

      @raise Empty if the sequence is empty *)

val take_opt_l : 'a t -> 'a option
  (** [take_opt_l x s] remove and returns [Some x] where [x] is the
      leftmost element of [s] or [None] if [s] is empty *)

val take_opt_r : 'a t -> 'a option
  (** [take_opt_l x s] remove and returns [Some x] where [x] is the
      rightmost element of [s] or [None] if [s] is empty *)

val transfer_l : 'a t -> 'a t -> unit
  (** [transfer_l s1 s2] removes all elements of [s1] and add them at
      the left of [s2]. This operation runs in constant time and
      space. *)

val transfer_r : 'a t -> 'a t -> unit
  (** [transfer_r s1 s2] removes all elements of [s1] and add them at
      the right of [s2]. This operation runs in constant time and
      space. *)

(** {2 Sequence iterators} *)

(** Note: it is OK to remove a node while traversing a sequence *)

val iter_l : ('a -> unit) -> 'a t -> unit
  (** [iter_l f s] applies [f] on all elements of [s] starting from
      the left *)

val iter_r : ('a -> unit) -> 'a t -> unit
  (** [iter_l f s] applies [f] on all elements of [s] starting from
      the right *)

val iter_node_l : ('a node -> unit) -> 'a t -> unit
  (** [iter_l f s] applies [f] on all nodes of [s] starting from
      the left *)

val iter_node_r : ('a node -> unit) -> 'a t -> unit
  (** [iter_l f s] applies [f] on all nodes of [s] starting from
      the right *)

val fold_l : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold_l f s] is:
      {[
        fold_l f s x = f en (... (f e2 (f e1 x)))
      ]}
      where [e1], [e2], ..., [en] are the elements of [s]
  *)

val fold_r : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold_r f s] is:
      {[
        fold_r f s x = f e1 (f e2 (... (f en x)))
      ]}
      where [e1], [e2], ..., [en] are the elements of [s]
  *)

val find_node_opt_l : ('a -> bool) -> 'a t -> 'a node option
  (** [find_node_opt_l f s] returns [Some x], where [x] is the first node of
      [s] starting from the left that satisfies [f] or [None] if none
      exists. *)

val find_node_opt_r : ('a -> bool) -> 'a t -> 'a node option
  (** [find_node_opt_r f s] returns [Some x], where [x] is the first node of
      [s] starting from the right that satisfies [f] or [None] if none
      exists. *)

val find_node_l : ('a -> bool) -> 'a t -> 'a node
  (** [find_node_l f s] returns the first node of [s] starting from the left
      that satisfies [f] or raises [Not_found] if none exists. *)

val find_node_r : ('a -> bool) -> 'a t -> 'a node
  (** [find_node_r f s] returns the first node of [s] starting from the right
      that satisfies [f] or raises [Not_found] if none exists. *)
