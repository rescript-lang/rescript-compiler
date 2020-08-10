(* This module represents a general interface to parse marshalled reason ast *)

(* extracts comments and the original string data from a reason file *)
val extractConcreteSyntax :
  string -> Res_token.Comment.t list * (string * Location.t) list

val parsingEngine : unit Res_driver.parsingEngine
