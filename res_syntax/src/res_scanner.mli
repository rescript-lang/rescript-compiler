type mode = Jsx | Diamond

type charEncoding

type t = {
  filename: string;
  src: string;
  srcLen: int;
  mutable err:
    startPos:Lexing.position ->
    endPos:Lexing.position ->
    Res_diagnostics.category ->
    unit;
  mutable ch: charEncoding; (* current character *)
  mutable offset: int; (* current byte offset *)
  mutable offset16: int;
      (* current number of utf16 code units since line start *)
  mutable lineOffset: int; (* current line offset *)
  mutable lnum: int; (* current line number *)
  mutable mode: mode list;
  mutable customInfix: Res_custom_infix.t;
}

val make : filename:string -> string -> t

(* TODO: make this a record *)
val scan : t -> Lexing.position * Lexing.position * Res_token.t

val isBinaryOp : string -> int -> int -> bool

val setJsxMode : t -> unit
val setDiamondMode : t -> unit
val popMode : t -> mode -> unit

val reconsiderLessThan : t -> Res_token.t

val scanTemplateLiteralToken :
  t -> Lexing.position * Lexing.position * Res_token.t
