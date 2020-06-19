module IO : sig 
  val readFile : string -> string 
  val readStdin : unit -> string 
end
val isReasonDocComment : Napkin_comment.t -> bool
val extractConcreteSyntax :
  string -> Napkin_token.Comment.t list * (string * Location.t) list
val parsingEngine : unit Napkin_driver.parsingEngine
