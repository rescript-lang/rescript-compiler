type ('ast, 'diagnostics) parseResult = {
  filename: string; [@live]
  source: string;
  parsetree: 'ast;
  diagnostics: 'diagnostics;
  invalid: bool;
  comments: Res_comment.t list;
}

type 'diagnostics parsingEngine = {
  parseImplementation:
    forPrinter:bool ->
    filename:string ->
    (Parsetree.structure, 'diagnostics) parseResult;
  parseInterface:
    forPrinter:bool ->
    filename:string ->
    (Parsetree.signature, 'diagnostics) parseResult;
  stringOfDiagnostics: source:string -> filename:string -> 'diagnostics -> unit;
}

val parseImplementationFromSource :
  forPrinter:bool ->
  displayFilename:string ->
  source:string ->
  (Parsetree.structure, Res_diagnostics.t list) parseResult
  [@@live]

val parseInterfaceFromSource :
  forPrinter:bool ->
  displayFilename:string ->
  source:string ->
  (Parsetree.signature, Res_diagnostics.t list) parseResult
  [@@live]

type printEngine = {
  printImplementation:
    width:int ->
    filename:string ->
    comments:Res_comment.t list ->
    Parsetree.structure ->
    unit;
  printInterface:
    width:int ->
    filename:string ->
    comments:Res_comment.t list ->
    Parsetree.signature ->
    unit;
}

val parsingEngine : Res_diagnostics.t list parsingEngine

val printEngine : printEngine

(* ReScript implementation parsing compatible with ocaml pparse driver. Used by the compiler. *)
val parse_implementation : string -> Parsetree.structure
  [@@live] [@@raises Location.Error]

(* ReScript interface parsing compatible with ocaml pparse driver. Used by the compiler *)
val parse_interface : string -> Parsetree.signature
  [@@live] [@@raises Location.Error]
