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
exception Error of (string * Loc.location)

type conf_values =
  { plus_tags   : (string * Loc.location)  list;
    minus_tags  : (string * Loc.location) list }

type conf = (Glob.globber * conf_values) list

val ocamldep_output : Loc.source -> Lexing.lexbuf -> (string * string list) list
val space_sep_strings : Loc.source -> Lexing.lexbuf -> string list
val blank_sep_strings : Loc.source -> Lexing.lexbuf -> string list
val comma_sep_strings : Loc.source -> Lexing.lexbuf -> string list
val comma_or_blank_sep_strings : Loc.source -> Lexing.lexbuf -> string list
val trim_blanks : Loc.source -> Lexing.lexbuf -> string

(* Parse an environment path (i.e. $PATH).
   This is a colon separated string.
   Note: successive colons means an empty string.
   Example:
      ":aaa:bbb:::ccc:" -> [""; "aaa"; "bbb"; ""; ""; "ccc"; ""] *)
val parse_environment_path : Loc.source -> Lexing.lexbuf -> string list
(* Same one, for Windows (PATH is ;-separated) *)
val parse_environment_path_w : Loc.source -> Lexing.lexbuf -> string list

val conf_lines : string option -> Loc.source -> Lexing.lexbuf -> conf
val path_scheme : bool -> Loc.source -> Lexing.lexbuf ->
  [ `Word of string
  | `Var of (string * Glob.globber)
  ] list

val ocamlfind_query : Loc.source -> Lexing.lexbuf ->
  string * string * string * string * string * string

val tag_gen : Loc.source -> Lexing.lexbuf -> string * string option
