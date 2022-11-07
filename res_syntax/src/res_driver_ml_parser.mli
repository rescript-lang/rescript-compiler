(* This module represents a general interface to parse marshalled reason ast *)

(* extracts comments and the original string data from an ocaml file *)
val extractOcamlConcreteSyntax :
  string -> (string * Location.t) list * Res_comment.t list
  [@@live]

val parsingEngine : unit Res_driver.parsingEngine

val printEngine : Res_driver.printEngine
