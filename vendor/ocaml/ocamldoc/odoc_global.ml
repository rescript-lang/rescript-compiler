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

(* Tell ocaml compiler not to generate files. *)
let _ = Clflags.dont_write_files := true

open Clflags

type source_file =
    Impl_file of string
  | Intf_file of string
  | Text_file of string

let include_dirs = Clflags.include_dirs

let errors = ref 0

let warn_error = ref false

let pwarning s =
  if !Odoc_config.print_warnings then prerr_endline (Odoc_messages.warning^": "^s);
  if !warn_error then incr errors

let merge_options = ref ([] : Odoc_types.merge_option list)

let classic = Clflags.classic

let dump = ref (None : string option)

let load = ref ([] : string list)

let sort_modules = ref false

let no_custom_tags = ref false

let no_stop = ref false

let remove_stars = ref false

let keep_code = ref false

let inverse_merge_ml_mli = ref false

let filter_with_module_constraints = ref true

let hidden_modules = ref ([] : string list)

let files = ref []

let out_file = ref Odoc_messages.default_out_file

let verbose = Clflags.verbose

let target_dir = ref Filename.current_dir_name

let title = ref (None : string option)

let intro_file = ref (None : string option)

let with_header = ref true

let with_trailer = ref true

let with_toc = ref true

let with_index = ref true
