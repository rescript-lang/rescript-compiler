(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Berke Durak *)
(* Bool *)

(** Provides a datatype for representing boolean formulas and evaluation,
    iteration and map functions. *)

(** Public type for generic boolean formulas.  An empty conjunction [And[]] is true and
    an empty disjunction [Or[]] is false. *)
type 'a boolean =
    And of 'a boolean list
  | Or of 'a boolean list
  | Not of 'a boolean
  | Atom of 'a
  | True
  | False

val eval : ('a -> bool) -> 'a boolean -> bool
(** [eval g f] evaluates the boolean formula [f] using the values returned by [g] for the atoms. *)
val iter : ('a -> unit) -> 'a boolean -> unit
(** [iter g f] calls [g] over every atom of [f]. *)
val map : ('a -> 'b) -> 'a boolean -> 'b boolean
(** [map g f] replaces every atom of [f] by its image by [g]. *)
