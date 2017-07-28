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
type backtrace = private
  | Leaf of Pathname.t
  | Choice of backtrace list
  | Depth of Pathname.t * backtrace
  | Target of string * backtrace
exception Failed of backtrace
exception Circular of Pathname.t * Pathname.t list

val solve : Pathname.t -> unit
val solve_target : string -> Pathname.t list -> Pathname.t
