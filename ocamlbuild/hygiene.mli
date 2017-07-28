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
(* Hygiene *)

(** Module for checking that the source tree is not polluted by object files. *)

(** Sanity rules to abide.  Not to be confused with compilation rules. *)
type rule =
  Implies_not of pattern * pattern (** The rule [Implies_not(".mll",".ml")] is broken if there is a file [foo.mll]
                                       together with a file [foo.ml] int the same directory.  The second file can
                                       get sanitized. *)
| Not of pattern (* No files with suffix [pattern] will be tolerated. *)

(** Suffix matching is enough for the purpose of this module. *)
and pattern = suffix

(** And a suffix is a string. *)
and suffix = string

(** A warning is simply displayed.  A failures stops the compilation. *)
type penalty = Warn | Fail

(** This type is used to encode laws that will be checked by this module. *)
type law = {
  law_name : string; (** The name of the law that will be printed when it is violated. *)
  law_rules : rule list; (** Breaking any of these rules is breaking this law. *)
  law_penalty : penalty;  (** Breaking the law gives you either a warning or a failure. *)
}

(** [check ~sanitize laws entry] will scan the directory tree [entry] for violation to the given [laws].
    Any warnings or errors will be printed on the [stdout].  If [sanitize] is [Some fn], a shell script will be written
    into the file [fn] with commands to delete the offending files.  The command will return a pair [(fatal, penalties)]
    where [fatal] is [true] when serious hygiene violations have been spotted, and [penalties] is a list of laws and
    messages describing the offenses. *)
val check : ?sanitize:string -> law list -> bool Slurp.entry -> (law * string list) list
