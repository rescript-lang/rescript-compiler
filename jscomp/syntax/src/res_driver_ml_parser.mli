(* This module represents a general interface to parse marshalled reason ast *)

(* extracts comments and the original string data from an ocaml file *)
val extract_ocaml_concrete_syntax :
  string -> (string * Location.t) list * Res_comment.t list
[@@live]

val parsing_engine : unit Res_driver.parsing_engine

val print_engine : Res_driver.print_engine
