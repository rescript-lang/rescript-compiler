type mode = Template | Jsx | Diamond

type t = {
  filename: string;
  src: bytes;
  mutable err:
    startPos: Lexing.position
    -> endPos: Lexing.position
    -> Napkin_diagnostics.category
    -> unit;
  mutable ch: int; (* current character *)
  mutable offset: int; (* character offset *)
  mutable rdOffset: int; (* reading offset (position after current character) *)
  mutable lineOffset: int; (* current line offset *)
  mutable lnum: int; (* current line number *)
  mutable mode: mode list;
}

val make: ?line:int -> filename:string -> bytes -> t

(* TODO: make this a record *)
val scan: t -> (Lexing.position * Lexing.position * Napkin_token.t)

val isBinaryOp: bytes -> int -> int -> bool

val setTemplateMode: t -> unit
val setJsxMode: t -> unit
val setDiamondMode: t -> unit
val popMode: t -> mode -> unit

val reconsiderLessThan: t -> Napkin_token.t

val scanTemplateLiteralToken: t -> (Lexing.position * Lexing.position * Napkin_token.t)
