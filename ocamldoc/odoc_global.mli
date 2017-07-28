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

(** Global variables. *)

(** The kind of source file in arguments. *)
type source_file =
    Impl_file of string
  | Intf_file of string
  | Text_file of string

(** The include_dirs in the OCaml compiler. *)
val include_dirs : string list ref

(** The merge options to be used. *)
val merge_options : Odoc_types.merge_option list ref

(** Classic mode or not. *)
val classic : bool ref

(** The optional file name to dump the collected information into.*)
val dump : string option ref

(** The list of information files to load. *)
val load : string list ref

(** We must sort the list of top modules or not.*)
val sort_modules : bool ref

(** We must not stop at the stop special comments. Default is false (we stop).*)
val no_stop : bool ref

(** We must raise an exception when we find an unknown @-tag. *)
val no_custom_tags : bool ref

(** We must remove the the first characters of each comment line, until the first asterisk '*'. *)
val remove_stars : bool ref

(** To keep the code while merging, when we have both .ml and .mli files for a module. *)
val keep_code : bool ref

(** To inverse implementation and interface files when merging. *)
val inverse_merge_ml_mli : bool ref

(** To filter module elements according to module type constraints. *)
val filter_with_module_constraints : bool ref

(** The list of module names to hide. *)
val hidden_modules : string list ref

(** The files to be analysed. *)
val files : source_file list ref
(** A counter for errors. *)
val errors : int ref

(** Indicate if a warning is an error. *)
val warn_error : bool ref

(** Print the given warning, adding it to the list of {!errors}
if {!warn_error} is [true]. *)
val pwarning : string -> unit

(** The file used by the generators outputting only one file. *)
val out_file : string ref

(** Verbose mode or not. *)
val verbose : bool ref

(** The optional file whose content can be used as intro text. *)
val intro_file : string option ref

(** The optional title to use in the generated documentation. *)
val title : string option ref

(** The directory where files have to be generated. *)
val target_dir : string ref

(** The flag which indicates if we must generate a table of contents. *)
val with_toc : bool ref

(** The flag which indicates if we must generate an index. *)
val with_index : bool ref

(** The flag which indicates if we must generate a header.*)
val with_header : bool ref

(** The flag which indicates if we must generate a trailer.*)
val with_trailer : bool ref
