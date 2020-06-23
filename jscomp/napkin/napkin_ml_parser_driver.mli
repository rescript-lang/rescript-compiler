
 module IO :
  sig val readFile : string -> string val readStdin : unit -> string end 
val setup : filename:string -> Lexing.lexbuf
val extractOcamlConcreteSyntax :
  string -> (string * Location.t) list * Napkin_comment.t list
val parsingEngine : unit Napkin_driver.parsingEngine
val printEngine : Napkin_driver.printEngine
