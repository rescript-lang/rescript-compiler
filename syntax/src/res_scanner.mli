type mode = Jsx | Diamond

type t = {
  filename: string;
  src: bytes;
  mutable err:
    startPos: Lexing.position
    -> endPos: Lexing.position
    -> Res_diagnostics.category
    -> unit;
  mutable ch: Char.t; (* current character *)
  mutable offset: int; (* character offset *)
  mutable rdOffset: int; (* reading offset (position after current character) *)
  mutable lineOffset: int; (* current line offset *)
  mutable lnum: int; (* current line number *)
  mutable mode: mode list;
}

val make: ?line:int -> filename:string -> bytes -> t

(* TODO: make this a record *)
val scan: t -> (Lexing.position * Lexing.position * Res_token.t)

val isBinaryOp: bytes -> int -> int -> bool

val setJsxMode: t -> unit
val setDiamondMode: t -> unit
val popMode: t -> mode -> unit

val reconsiderLessThan: t -> Res_token.t

val scanTemplateLiteralToken: t -> (Lexing.position * Lexing.position * Res_token.t)

val tryAdvanceQuotedString: t -> unit
