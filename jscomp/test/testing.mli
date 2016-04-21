(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*             Pierre Weis, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2006 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Testing auxilliaries. *)

val test : bool -> unit;;
(** [test e] tests that [e] evaluates to [true]. *)
val failure_test : ('a -> 'b) -> 'a -> string -> bool;;
(** [failure_test f x s] tests that [f x] raises the exception [Failure s]. *)

val test_raises_some_exc : ('a -> 'b) -> 'a -> bool;;
(** [test_raises_some_exc f x] tests that [f x] raises an exception. *)

val test_raises_this_exc : exn -> ('a -> 'b) -> 'a -> bool;;
(** [test_raises_this_exc exc f x] tests that [f x]
    raises the exception [exc]. *)

val test_raises_exc_p : (exn -> bool) -> ('a -> 'b) -> 'a -> bool;;
(** [test_raises_exc_p p f x] tests that [f x] raises an exception that
    verifies predicate [p]. *)

val scan_failure_test : ('a -> 'b) -> 'a -> bool;;
(** [scan_failure_test f x] tests that [f x] raises [Scanf.Scan_failure]. *)
