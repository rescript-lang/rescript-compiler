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

(** Analysis of source files. *)

(** This function builds the top modules from the analysis of the
   given list of source files.
   @param init is the list of modules already known from a previous analysis.
*)
val analyse_files :
    ?init: Odoc_module.t_module list ->
      Odoc_global.source_file list ->
        Odoc_module.t_module list

(** Dump of a list of modules into a file.
   @raise Failure if an error occurs.*)
val dump_modules : string -> Odoc_module.t_module list -> unit

(** Load of a list of modules from a file.
   @raise Failure if an error occurs.*)
val load_modules : string -> Odoc_module.t_module list
