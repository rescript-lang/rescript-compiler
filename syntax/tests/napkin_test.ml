(* test printing of .res file*)
let () =
  let filename = "./tests/api/resSyntax.res" in
  let prettySource = Napkin_multi_printer.print `res ~input:filename in
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
  let prettySource = Napkin_multi_printer.print `res ~input:filename in
  assert (
    prettySource = {|// test interface file

let x: int
|}
  )

let refmtBinaryPath = "./lib/refmt.exe"

(* test printing of reason .re file *)
let () =
  let filename = "./tests/api/reasonSyntax.re" in
  let prettySource = Napkin_multi_printer.print (`refmt refmtBinaryPath) ~input:filename in
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
  let prettySource = Napkin_multi_printer.print (`refmt refmtBinaryPath) ~input:filename in
  assert (
    prettySource = {|// test .rei file
let x: int
|}
  )

(* test printing of ocaml .ml file *)
let () =
  let filename = "./tests/api/mlSyntax.ml" in
  let prettySource = Napkin_multi_printer.print `ml ~input:filename in
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
  let prettySource = Napkin_multi_printer.print `ml ~input:filename in
  assert (
    prettySource = {|/* test mli file */

let x: int

/* comment */
let y: float
|}
  )


let () = print_endline "Tests passed!"
