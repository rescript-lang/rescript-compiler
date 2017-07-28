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

val ocamldoc_c : Tags.t -> string -> string -> Command.t
val ocamldoc_l_dir : Tags.t -> string list -> string -> string -> Command.t
val ocamldoc_l_file : Tags.t -> string list -> string -> string -> Command.t

val ocamldep_command : string -> string -> Rule.action
val menhir_ocamldep_command : string -> string -> Rule.action
val menhir_modular_ocamldep_command : string -> string -> Rule.action
val menhir_modular : string -> string -> string -> Rule.action
val ocamlyacc : string -> Rule.action
val ocamllex : string -> Rule.action
val menhir : string -> Rule.action
val infer_interface : string -> string -> Rule.action
val document_ocaml_interf : string -> string -> Rule.action
val document_ocaml_implem : string -> string -> Rule.action
val document_ocaml_project :
  ?ocamldoc:(Tags.t -> string list -> string -> string -> Command.t) ->
  string -> string -> string -> Rule.action

val camlp4 : ?default:Command.spec -> Tags.elt -> Pathname.t -> Pathname.t -> Rule.action
