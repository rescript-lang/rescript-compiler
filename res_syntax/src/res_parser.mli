module Scanner = Res_scanner
module Token = Res_token
module Grammar = Res_grammar
module Reporting = Res_reporting
module Diagnostics = Res_diagnostics
module Comment = Res_comment

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

val make : ?mode:mode -> string -> string -> t

val expect : ?grammar:Grammar.t -> Token.t -> t -> unit
val optional : t -> Token.t -> bool
val next : ?prevEndPos:Lexing.position -> t -> unit
val nextUnsafe : t -> unit (* Does not assert on Eof, makes no progress *)
val nextTemplateLiteralToken : t -> unit
val lookahead : t -> (t -> 'a) -> 'a
val err :
  ?startPos:Lexing.position ->
  ?endPos:Lexing.position ->
  t ->
  Diagnostics.category ->
  unit

val leaveBreadcrumb : t -> Grammar.t -> unit
val eatBreadcrumb : t -> unit

val beginRegion : t -> unit
val endRegion : t -> unit

val checkProgress : prevEndPos:Lexing.position -> result:'a -> t -> 'a option
