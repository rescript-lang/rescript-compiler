type ('ast, 'diagnostics) parseResult = {
  filename: string; [@live]
  source: string;
  parsetree: 'ast;
  diagnostics: 'diagnostics;
  invalid: bool;
  comments: Napkin_comment.t list
}

type ('diagnostics) parsingEngine = {
  parseImplementation:
    forPrinter:bool -> filename:string
    -> (Parsetree.structure, 'diagnostics) parseResult;
  parseInterface:
    forPrinter:bool -> filename:string
    -> (Parsetree.signature, 'diagnostics) parseResult;
  stringOfDiagnostics: source:string -> filename:string -> 'diagnostics  -> string
}

type printEngine = {
  printImplementation:
    width: int
    -> filename: string
    -> comments: Napkin_comment.t list
    -> Parsetree.structure
    -> unit;
  printInterface:
    width: int
    -> filename: string
    -> comments: Napkin_comment.t list
    -> Parsetree.signature
    -> unit;
}

val parsingEngine: (Napkin_diagnostics.t list) parsingEngine

val printEngine: printEngine

(* Napkin implementation parsing compatible with ocaml pparse driver *)
val parse_implementation:
   string -> Parsetree.structure
[@@live]
[@@raises Location.Error]

(* Napkin interface parsing compatible with ocaml pparse driver *)
val parse_interface:
   string -> Parsetree.signature
[@@live]
[@@raises Location.Error]
