(*
  This CLI isn't used apart for this repo's testing purposes. The syntax
  itself is used by ReScript's compiler programmatically through various other apis.
*)

(*
  This is OCaml's Misc.ml's Color module. More specifically, this is
  ReScript's OCaml fork's Misc.ml's Color module:
  https://github.com/rescript-lang/ocaml/blob/92e58bedced8d7e3e177677800a38922327ab860/utils/misc.ml#L540

  The syntax's printing's coloring logic depends on:
    1. a global mutable variable that's set in the compiler: Misc.Color.color_enabled
    2. the colors tags supported by Misc.Color, e.g. style_of_tag, which Format
      tags like @{<error>hello@} use
    3. etc.

  When this syntax is programmatically used inside ReScript, the various
  Format tags like <error> and <dim> get properly colored depending on the
  above points.

  But when used by this cli file, that coloring logic doesn't render properly
  because we're compiling against vanilla OCaml 4.06 instead of ReScript's
  OCaml fork. For example, the vanilla compiler doesn't support the `dim`
  color (grey). So we emulate the right coloring logic by copy pasting how our
  forked OCaml compiler does it.
*)
module Color = struct
  (* use ANSI color codes, see https://en.wikipedia.org/wiki/ANSI_escape_code *)
  type color =
    | Black [@live]
    | Red
    | Green [@live]
    | Yellow
    | Blue [@live]
    | Magenta
    | Cyan
    | White [@live]
  ;;

  type style =
    | FG of color (* foreground *)
    | BG of color [@live] (* background *)
    | Bold
    | Reset
    | Dim

  let ansi_of_color = function
    | Black -> "0"
    | Red -> "1"
    | Green -> "2"
    | Yellow -> "3"
    | Blue -> "4"
    | Magenta -> "5"
    | Cyan -> "6"
    | White -> "7"

  let code_of_style = function
    | FG c -> "3" ^ ansi_of_color c
    | BG c -> "4" ^ ansi_of_color c
    | Bold -> "1"
    | Reset -> "0"
    | Dim -> "2"

  let ansi_of_style_l l =
    let s = match l with
      | [] -> code_of_style Reset
      | [s] -> code_of_style s
      | _ -> String.concat ";" (List.map code_of_style l)
    in
    "\x1b[" ^ s ^ "m"

  type styles = {
    error: style list;
    warning: style list;
    loc: style list;
  }

  let default_styles = {
    warning = [Bold; FG Magenta];
    error = [Bold; FG Red];
    loc = [Bold];
  }

  let cur_styles = ref default_styles
  (* let get_styles () = !cur_styles *)
  (* let set_styles s = cur_styles := s *)

  (* map a tag to a style, if the tag is known.
     @raise Not_found otherwise *)
  let style_of_tag s = match s with
    | "error" -> (!cur_styles).error
    | "warning" -> (!cur_styles).warning
    | "loc" -> (!cur_styles).loc
    | "info" -> [Bold; FG Yellow]
    | "dim" -> [Dim]
    | "filename" -> [FG Cyan]
    | _ -> raise Not_found
  [@@raises Not_found]

  let color_enabled = ref true

  (* either prints the tag of [s] or delegates to [or_else] *)
  let mark_open_tag ~or_else s =
    try
      let style = style_of_tag s in
      if !color_enabled then ansi_of_style_l style else ""
    with Not_found -> or_else s

  let mark_close_tag ~or_else s =
    try
      let _ = style_of_tag s in
      if !color_enabled then ansi_of_style_l [Reset] else ""
    with Not_found -> or_else s

  (* add color handling to formatter [ppf] *)
  let set_color_tag_handling ppf =
    let open Format in
    let functions = pp_get_formatter_tag_functions ppf () in
    let functions' = {functions with
      mark_open_tag=(mark_open_tag ~or_else:functions.mark_open_tag);
      mark_close_tag=(mark_close_tag ~or_else:functions.mark_close_tag);
    } in
    pp_set_mark_tags ppf true; (* enable tags *)
    pp_set_formatter_tag_functions ppf functions';
    (* also setup margins *)
    pp_set_margin ppf (pp_get_margin std_formatter());
    ()

  external isatty : out_channel -> bool = "caml_sys_isatty"

  (* reasonable heuristic on whether colors should be enabled *)
  let should_enable_color () =
    let term = try Sys.getenv "TERM" with Not_found -> "" in
    term <> "dumb"
    && term <> ""
    && isatty stderr

  type setting = Auto [@live] | Always [@live] | Never [@live]

  let setup =
    let first = ref true in (* initialize only once *)
    let formatter_l =
      [Format.std_formatter; Format.err_formatter; Format.str_formatter]
    in
    fun o ->
      if !first then (
        first := false;
        Format.set_mark_tags true;
        List.iter set_color_tag_handling formatter_l;
        color_enabled := (match o with
            | Some Always -> true
            | Some Auto -> should_enable_color ()
            | Some Never -> false
            | None -> should_enable_color ())
      );
      ()
end

(* command line flags *)
module ResClflags: sig
  val recover: bool ref
  val print: string ref
  val width: int ref
  val origin: string ref
  val files: string list ref
  val interface: bool ref
  val ppx: string ref

  val parse: unit -> unit
end = struct
  let recover = ref false
  let width = ref 100

  let files = ref []
  let addFilename filename = files := filename::(!files)

  let print = ref "res"
  let origin = ref "res"
  let interface = ref false
  let ppx = ref ""

  let usage = "\n**This command line is for the repo developer's testing purpose only. DO NOT use it in production**!\n\n" ^
  "Usage:\n  rescript <options> <file>\n\n" ^
  "Examples:\n" ^
  "  rescript myFile.res\n" ^
  "  rescript -parse ml -print res myFile.ml\n" ^
  "  rescript -parse res -print binary -interface myFile.resi\n\n" ^
  "Options are:"

  let spec = [
    ("-recover", Arg.Unit (fun () -> recover := true), "Emit partial ast");
    ("-parse", Arg.String (fun txt -> origin := txt), "Parse reasonBinary, ml or res. Default: res");
    ("-print", Arg.String (fun txt -> print := txt), "Print either binary, ml, ast, sexp or res. Default: res");
    ("-width", Arg.Int (fun w -> width := w), "Specify the line length for the printer (formatter)");
    ("-interface", Arg.Unit (fun () -> interface := true), "Parse as interface");
    ("-ppx", Arg.String (fun txt -> ppx := txt), "Apply a specific built-in ppx before parsing, none or jsx. Default: none");
  ]

  let parse () = Arg.parse spec addFilename usage
end

module CliArgProcessor = struct
  type backend = Parser: ('diagnostics) Res_driver.parsingEngine -> backend [@@unboxed]

  let processFile ~isInterface ~width ~recover ~origin ~target ~ppx filename =
    try
      let len = String.length filename in
      let processInterface =
        isInterface || len > 0 && (String.get [@doesNotRaise]) filename (len - 1) = 'i'
      in
      let parsingEngine =
        match origin with
        | "reasonBinary" -> Parser Res_driver_reason_binary.parsingEngine
        | "ml" -> Parser Res_driver_ml_parser.parsingEngine
        | "res" -> Parser Res_driver.parsingEngine
        | origin ->
          print_endline ("-parse needs to be either reasonBinary, ml or res. You provided " ^ origin);
          exit 1
      in
      let printEngine =
        match target with
        | "binary" -> Res_driver_binary.printEngine
        | "ml" -> Res_driver_ml_parser.printEngine
        | "ast" -> Res_ast_debugger.printEngine
        | "sexp" -> Res_ast_debugger.sexpPrintEngine
        | "res"  -> Res_driver.printEngine
        | target ->
          print_endline ("-print needs to be either binary, ml, ast, sexp or res. You provided " ^ target);
          exit 1
      in

      let forPrinter = match target with
      | "res" | "sexp" -> true
      | _ -> false
      in

      let Parser backend = parsingEngine in
      (* This is the whole purpose of the Color module above *)
      Color.setup None;
      if processInterface then
        let parseResult = backend.parseInterface ~forPrinter ~filename in
        if parseResult.invalid then begin
          backend.stringOfDiagnostics
            ~source:parseResult.source
            ~filename:parseResult.filename
            parseResult.diagnostics;
          if recover then
            printEngine.printInterface
              ~width ~filename ~comments:parseResult.comments parseResult.parsetree
          else exit 1
        end
        else
          let parsetree = match ppx with
            | "jsx" -> Reactjs_jsx_ppx_v3.rewrite_signature parseResult.parsetree
            | _ -> parseResult.parsetree
          in
          printEngine.printInterface
            ~width ~filename ~comments:parseResult.comments parsetree
      else
        let parseResult = backend.parseImplementation ~forPrinter ~filename in
        if parseResult.invalid then begin
          backend.stringOfDiagnostics
            ~source:parseResult.source
            ~filename:parseResult.filename
            parseResult.diagnostics;
          if recover then
            printEngine.printImplementation
              ~width ~filename ~comments:parseResult.comments parseResult.parsetree
          else exit 1
        end
        else
          let parsetree = match ppx with
            | "jsx" -> Reactjs_jsx_ppx_v3.rewrite_implementation parseResult.parsetree
            | _ -> parseResult.parsetree
          in
          printEngine.printImplementation
            ~width ~filename ~comments:parseResult.comments parsetree
    with
    | Failure txt ->
      prerr_string txt;
      prerr_newline();
      exit 1
    | _ -> exit 1
  [@@raises Invalid_argument, exit]
end


let [@raises Invalid_argument, exit] () =
  if not !Sys.interactive then begin
    ResClflags.parse ();
    match !ResClflags.files with
    | [] -> (* stdin *)
      CliArgProcessor.processFile
        ~isInterface:!ResClflags.interface
        ~width:!ResClflags.width
        ~recover:!ResClflags.recover
        ~target:!ResClflags.print
        ~origin:!ResClflags.origin
        ~ppx:!ResClflags.ppx
        ""
    | files ->
      List.iter (fun filename ->
        CliArgProcessor.processFile
          ~isInterface:!ResClflags.interface
          ~width:!ResClflags.width
          ~recover:!ResClflags.recover
          ~target:!ResClflags.print
          ~origin:!ResClflags.origin
          ~ppx:!ResClflags.ppx
          filename
        ) files
end
