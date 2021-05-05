module IO = Res_io

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
  let signatureToOutcome structure =
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
    | Typemod.Error (_, _, err) ->
      Typemod.report_error env Format.str_formatter err;
      prerr_string (Format.flush_str_formatter ());
      exit 1;
    | Typedecl.Error (_, err) ->
      Typedecl.report_error Format.str_formatter err;
      prerr_string (Format.flush_str_formatter ());
      exit 1;
    | e ->
      prerr_string
        ("Unknown error while trying to print outcome tree.\n" ^
        "We don't display all the outcome type errors; try adding the new case to the `try` pattern match.\n");
      raise e

  (* `tests/oprint/oprint.res` will be read into memory and typechecked.
   * The inferred signature (i.e. the type of the module `oprint.res`) will
   * then be converted to the outcome tree.
   * The outcome tree is printed to a string
   * and stored in a snapshot `tests/oprint/oprint.res.snapshot` *)
  let run () =
    let filename = "tests/oprint/oprint.res" in
    let result = Res_driver.parsingEngine.parseImplementation ~forPrinter:false ~filename in
    let signature = if result.Res_driver.invalid then (
      Res_driver.parsingEngine.stringOfDiagnostics
        ~source:(result.source)
        ~filename:result.filename
        result.diagnostics;
      exit 1
    ) else
      result.Res_driver.parsetree
    in
    IO.writeFile ~filename:"tests/oprint/expected/oprint.resi.txt" ~contents:(signatureToOutcome signature)
end

module ParserApiTest = struct
  let makeDefault () =
    let src = "   let x = 1\nlet y = 2\nlet z = 3" in
    let parser = Res_parser.make  src "test.res" in
    assert (parser.scanner.lnum == 1);
    assert (parser.scanner.lineOffset == 0);
    assert (parser.scanner.offset == 6);
    assert (parser.token = Res_token.Let);
    print_endline "✅ Parser make: initializes parser and checking offsets"

  let run () =
    makeDefault();

end

let () = OutcomePrinterTests.run()
let () = ParserApiTest.run()
