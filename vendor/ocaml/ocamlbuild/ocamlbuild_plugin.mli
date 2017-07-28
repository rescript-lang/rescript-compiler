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

include Ocamlbuild_pack.Signatures.PLUGIN
  with module Pathname = Ocamlbuild_pack.Pathname
   and module Outcome  = Ocamlbuild_pack.My_std.Outcome
   and module Tags     = Ocamlbuild_pack.Tags
   and module Command  = Ocamlbuild_pack.Command
