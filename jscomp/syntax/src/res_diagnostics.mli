module Token = Res_token
module Grammar = Res_grammar

type t
type category
type report

val getStartPos : t -> Lexing.position [@@live] (* for playground *)
val getEndPos : t -> Lexing.position [@@live] (* for playground *)

val explain : t -> string [@@live] (* for playground *)

val unexpected : Token.t -> (Grammar.t * Lexing.position) list -> category
val expected : ?grammar:Grammar.t -> Lexing.position -> Token.t -> category
val uident : Token.t -> category
val lident : Token.t -> category
val unclosedString : category
val unclosedTemplate : category
val unclosedComment : category
val unknownUchar : Char.t -> category
val message : string -> category

val make : startPos:Lexing.position -> endPos:Lexing.position -> category -> t

val printReport : t list -> string -> unit
