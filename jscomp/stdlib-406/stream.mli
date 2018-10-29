(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*         Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt       *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Streams and parsers. *)

type 'a t
(** The type of streams holding values of type ['a]. *)

exception Failure
(** Raised by parsers when none of the first components of the stream
   patterns is accepted. *)

exception Error of string
(** Raised by parsers when the first component of a stream pattern is
   accepted, but one of the following components is rejected. *)


(** {1 Stream builders} *)

val from : (int -> 'a option) -> 'a t
(** [Stream.from f] returns a stream built from the function [f].
   To create a new stream element, the function [f] is called with
   the current stream count. The user function [f] must return either
   [Some <value>] for a value or [None] to specify the end of the
   stream.

   Do note that the indices passed to [f] may not start at [0] in the
   general case. For example, [[< '0; '1; Stream.from f >]] would call
   [f] the first time with count [2].
*)

val of_list : 'a list -> 'a t
(** Return the stream holding the elements of the list in the same
   order. *)

val of_string : string -> char t
(** Return the stream of the characters of the string parameter. *)

val of_bytes : bytes -> char t
(** Return the stream of the characters of the bytes parameter.
    @since 4.02.0 *)

val of_channel : in_channel -> char t
(** Return the stream of the characters read from the input channel. *)


(** {1 Stream iterator} *)

val iter : ('a -> unit) -> 'a t -> unit
(** [Stream.iter f s] scans the whole stream s, applying function [f]
   in turn to each stream element encountered. *)


(** {1 Predefined parsers} *)

val next : 'a t -> 'a
(** Return the first element of the stream and remove it from the
   stream. Raise {!Stream.Failure} if the stream is empty. *)

val empty : 'a t -> unit
(** Return [()] if the stream is empty, else raise {!Stream.Failure}. *)


(** {1 Useful functions} *)

val peek : 'a t -> 'a option
(** Return [Some] of "the first element" of the stream, or [None] if
   the stream is empty. *)

val junk : 'a t -> unit
(** Remove the first element of the stream, possibly unfreezing
   it before. *)

val count : 'a t -> int
(** Return the current count of the stream elements, i.e. the number
   of the stream elements discarded. *)

val npeek : int -> 'a t -> 'a list
(** [npeek n] returns the list of the [n] first elements of
   the stream, or all its remaining elements if less than [n]
   elements are available. *)

(**/**)

(* The following is for system use only. Do not call directly. *)

val iapp : 'a t -> 'a t -> 'a t
val icons : 'a -> 'a t -> 'a t
val ising : 'a -> 'a t

val lapp : (unit -> 'a t) -> 'a t -> 'a t
val lcons : (unit -> 'a) -> 'a t -> 'a t
val lsing : (unit -> 'a) -> 'a t

val sempty : 'a t
val slazy : (unit -> 'a t) -> 'a t

val dump : ('a -> unit) -> 'a t -> unit
