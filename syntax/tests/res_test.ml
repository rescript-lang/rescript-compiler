module IO = Res_io

module Snapshot = struct
  (* If set to true, will always create a new snapshot file.
   * Previous snapshots will be overwritten *)
  let forceUpdate = ref false

  let take ~filename ~contents =
    (* snapshot filename, just append .snapshot *)
    let snapFilename = filename ^ ".snapshot" in
    let diff =
      (* create the file when snapshot doesn't exist or we're force updating *)
      if !forceUpdate || not (Sys.file_exists snapFilename) then (
        IO.writeFile ~filename:snapFilename ~contents;
        None
      ) else (
        (* snapshot file exists *)
        let snapContents = IO.readFile ~filename:snapFilename in
        (* check for equality, do diffing later if needed *)
        if contents = snapContents then None else Some snapContents
      )
    in
    match diff with
    | Some snapContents ->
      prerr_string ("❌ snapshot " ^ filename);
      prerr_newline();
      Res_diff.diffTwoStrings snapContents contents;
      exit 1
    | None ->
      print_endline (
        if !forceUpdate then
          "✍️  updated snapshot " ^ filename
        else
          "✅ snapshot " ^ filename
      )
end

let usage = "Usage: test.exe <options>\nOptions are:"

let spec = [
  ("-update-snapshot", Arg.Unit (fun () -> Snapshot.forceUpdate.contents <- true), "Update snapshots")
]

let () = Arg.parse spec (fun _ -> ()) usage

(* test printing of .res file*)
let () =
  let filename = "./tests/api/resSyntax.res" in
  let prettySource = Res_multi_printer.print `res ~input:filename in
  assert (
    prettySource = {|// test file

if true {
  Js.log("true")
} else {
  Js.log("false")
}
|}
  )

(* test printing of .resi file*)
let () =
  let filename = "./tests/api/resiSyntax.resi" in
  let prettySource = Res_multi_printer.print `res ~input:filename in
  assert (
    prettySource = {|// test interface file

let x: int
|}
  )

let refmtBinaryPath = "./lib/refmt.exe"

(* test printing of reason .re file *)
let () =
  let filename = "./tests/api/reasonSyntax.re" in
  let prettySource = Res_multi_printer.print (`refmt refmtBinaryPath) ~input:filename in
  assert (
    prettySource = {|// test .re file
let \"+++" = (a, b) => a + b

let unicode = "🙈 😅 🙌"

let d = `Sehr Schön` /* test */

let () = print_endline("foo")
|}
  )

(* test printing of reason .rei file *)
let () =
  let filename = "./tests/api/reiSyntax.rei" in
  let prettySource = Res_multi_printer.print (`refmt refmtBinaryPath) ~input:filename in
  assert (
    prettySource = {|// test .rei file
let x: int
|}
  )

(* test printing of ocaml .ml file *)
let () =
  let filename = "./tests/api/mlSyntax.ml" in
  let prettySource = Res_multi_printer.print `ml ~input:filename in
  assert (
    prettySource = {|/* test ml file */

let () = print_endline("hello world")

let unicode = "🙈 😅 🙌"

let d = `Sehr Schön`
|}
  )

(* test printing of ocaml .mli file *)
let () =
  let filename = "./tests/api/mliSyntax.mli" in
  let prettySource = Res_multi_printer.print `ml ~input:filename in
  assert (
    prettySource = {|/* test mli file */

let x: int

/* comment */
let y: float
|}
  )

let () = print_endline "✅ multi printer api tests"

module OutcomePrinterTests = struct
  let parseFile filename =
    let result = Res_driver.parsingEngine.parseImplementation ~forPrinter:false ~filename in

    if result.Res_driver.invalid then (
      Res_driver.parsingEngine.stringOfDiagnostics
        ~source:(result.source)
        ~filename:result.filename
        result.diagnostics;
      exit 1
    ) else
     result.Res_driver.parsetree


  let outcomeOfStructure structure =
    Lazy.force Res_outcome_printer.setup;

    Compmisc.init_path false;
    let env = Compmisc.initial_env () in
    try (
      let (_typedStructure, signature, _newenv) =
        Typemod.type_toplevel_phrase env structure in
      signature
      |> Printtyp.tree_of_signature
      |> (!Oprint.out_signature) Format.str_formatter;
      Format.flush_str_formatter()
    ) with
    | Typetexp.Error (_, _, err) ->
      Typetexp.report_error env Format.str_formatter err;
      prerr_string (Format.flush_str_formatter ());
      exit 1;
    | _ ->
      prerr_string "Unknown error while trying to print outcome tree";
      exit 1

  (* `tests/oprint/oprint.res` will be read into memory and typechecked.
   * The inferred signature (i.e. the type of the module `oprint.res`) will
   * then be converted to the outcome tree.
   * The outcome tree is printed to a string
   * and stored in a snapshot `tests/oprint/oprint.res.snapshot` *)
  let run () =
    let testFileName = "tests/oprint/oprint.res" in
    let printedOutcomeTree =
      parseFile testFileName |> outcomeOfStructure
    in
    Snapshot.take ~filename:testFileName ~contents:printedOutcomeTree
end

module ParserApiTest = struct
  let makeDefault () =
    let src = "   let x = 1\nlet y = 2\nlet z = 3" in
    let parser = Res_parser.make  src "test.res" in
    assert (parser.scanner.lnum == 1);
    assert (parser.scanner.lineOffset == 0);
    assert (parser.scanner.offset == 6);
    assert (parser.token = Res_token.Let);
    print_endline "✅ Parser make: initializes parser defaulting to line 1 "

  let seedLineNumber () =
    let src = "let x = 1\nlet y = 2\nlet z = 3" in
    let parser = Res_parser.make ~line:2 src "test.res" in
    assert (parser.scanner.lnum == 2);
    assert (parser.scanner.lineOffset == 0);
    assert (parser.scanner.offset == 3);
    assert (parser.token = Res_token.Let);
    print_endline "✅ Parser make: initializes parser with line set to 2"

  let run () =
    makeDefault();
    seedLineNumber()

end

let () = OutcomePrinterTests.run()
let () = ParserApiTest.run()
