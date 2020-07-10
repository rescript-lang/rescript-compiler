val setup : filename:string -> Lexing.lexbuf [@@live]

val extractOcamlConcreteSyntax :
  string -> (string * Location.t) list * Napkin_comment.t list [@@live]

val parsingEngine : unit Napkin_driver.parsingEngine

val printEngine : Napkin_driver.printEngine
