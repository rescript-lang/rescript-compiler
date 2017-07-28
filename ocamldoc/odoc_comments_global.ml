(***********************************************************************)
(*                                                                     *)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** The global variables used by the special comment parser.*)

let nb_chars = ref 0

let authors = ref ([] : string list)

let version = ref (None : string option)

let sees = ref ([] : string list)

let since = ref (None : string option)

let before = ref []

let deprecated = ref (None : string option)

let params = ref ([] : (string * string) list)

let raised_exceptions = ref ([] : (string * string) list)

let return_value = ref (None : string option)

let customs = ref []

let init () =
  nb_chars := 0;
  authors := [];
  version := None;
  sees := [];
  since := None;
  before := [];
  deprecated := None;
  params := [];
  raised_exceptions := [];
  return_value := None ;
  customs := []
