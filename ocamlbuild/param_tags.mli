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


(* Original author: Romain Bardou *)

val declare: string -> (string -> unit) -> unit
  (** Declare a parameterized tag.

[declare "name" action]: [action "param"] will be executed (once) by [init]
if a tag of the form [name(param)] is [acknowledge]d.

A given tag may be declared several times with different actions. All actions
will be executed, in the order they were declared. *)

val acknowledge: Loc.source -> Loc.location option -> string -> unit
  (** Acknowledge a tag.

If the tag is of the form [X(Y)], and have been declared using [declare],
then the actions given using [declare] will be executed with [Y] as parameter
when [init] is executed. The action will only be called once per
acknowledged parameter. *)

val init: unit -> unit
  (** Initialize parameterized tags.
      
This will make effective all instantiations [foo(bar)] such that the
parametrized tag [foo] has been [declare]d and [foo(bar)] has been
[acknowledge]d after the last [init] call. *)

val partial_init: ?quiet:bool -> Loc.source -> Tags.t -> unit
(** Initialize a list of tags

This will make effective the instances [foo(bar)] appearing
in the given tag list, instead of those that have been
[acknowledged] previously. This is for system use only. *)

val make: Tags.elt -> string -> Tags.elt
  (** Make a parameterized tag instance.

Example: [make "package" "unix"]: return the tag ["package(unix)"] *)
