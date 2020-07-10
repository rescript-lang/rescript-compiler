val isReasonDocComment : Napkin_comment.t -> bool [@@live]

val extractConcreteSyntax :
  string -> Napkin_token.Comment.t list * (string * Location.t) list [@@live]

val parsingEngine : unit Napkin_driver.parsingEngine
