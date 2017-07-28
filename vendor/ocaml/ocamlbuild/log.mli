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


(* Original author: Nicolas Pouillard *)
(* Log *)

(** Module for modulating the logging output with the logging level. *)
include Signatures.LOG

(** Turn it to true to have a classic display of commands. *)
val classic_display : bool ref

(** See {Display.event}. *)
val event : ?pretend:bool -> string -> string -> Tags.t -> unit

(**/**)

(** Initialize the Log module given a log file name. *)
val init : string option -> unit

val finish : ?how:[`Success|`Error|`Quiet] -> unit -> unit
val display : (out_channel -> unit) -> unit
val update : unit -> unit
val mode : string -> bool

(** Wrap logging event so that only fire at the end of the compilation
    process, possibly depending on the termination status.

    The name is used to avoid printing the same hint/warning twice,
    even if [at_end] is called several times. Use different names for
    distinct events.
*)
val at_end : name:string -> ([> `Error | `Quiet ] -> unit) -> unit
val at_failure : name:string -> ([> `Error ] -> unit) -> unit
