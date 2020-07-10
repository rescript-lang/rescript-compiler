module Token = Napkin_token
module Grammar = Napkin_grammar 
type t
type category
type report

type reportStyle = Pretty | Plain
val parseReportStyle: string -> reportStyle

val getStartPos: t -> Lexing.position [@@live] (* for playground *)
val getEndPos: t -> Lexing.position [@@live] (* for playground *)

val unexpected: Token.t -> (Grammar.t * Lexing.position) list -> category
val expected:  ?grammar:Grammar.t -> Lexing.position -> Token.t -> category
val uident: Token.t -> category
val lident: Token.t -> category
val unclosedString: category
val unclosedTemplate: category
val unclosedComment: category
val unknownUchar: int -> category
val message: string -> category

val make:
  filename: string
  -> startPos: Lexing.position
  -> endPos: Lexing.position
  -> category
  -> t

val stringOfReport: style:reportStyle -> t list -> string -> string
