module Scanner = Napkin_scanner
module Token = Napkin_token
module Grammar = Napkin_grammar
module Reporting = Napkin_reporting
module Diagnostics = Napkin_diagnostics
module Comment = Napkin_comment
type mode = ParseForTypeChecker | Default

type regionStatus = Report | Silent

type t = {
  mode: mode;
  mutable scanner: Scanner.t;
  mutable token: Token.t;
  mutable startPos: Lexing.position;
  mutable endPos: Lexing.position;
  mutable prevEndPos: Lexing.position;
  mutable breadcrumbs: (Grammar.t * Lexing.position) list;
  mutable errors: Reporting.parseError list;
  mutable diagnostics: Diagnostics.t list;
  mutable comments: Comment.t list;
  mutable regions: regionStatus ref list;
}

val make: ?mode: mode -> string -> string -> t

val expect: ?grammar:Grammar.t -> Token.t -> t -> unit
val optional: t -> Token.t -> bool
val next: ?prevEndPos:Lexing.position -> t -> unit
val lookahead: t -> (t -> 'a) -> 'a
val err:
  ?startPos:Lexing.position ->
  ?endPos:Lexing.position ->
  t -> Diagnostics.category -> unit

val leaveBreadcrumb: t -> Grammar.t -> unit
val eatBreadcrumb: t -> unit

val beginRegion: t -> unit
val endRegion: t -> unit

val checkProgress:
  prevEndPos: Lexing.position ->
  result: 'a ->
  t ->
  'a option