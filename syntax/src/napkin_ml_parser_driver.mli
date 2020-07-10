(* This module represents a general interface to parse marshalled reason ast *)

(* extracts comments and the original string data from an ocaml file *)
val extractOcamlConcreteSyntax :
  string -> (string * Location.t) list * Napkin_comment.t list [@@live]

val parsingEngine : unit Napkin_driver.parsingEngine

val printEngine : Napkin_driver.printEngine
