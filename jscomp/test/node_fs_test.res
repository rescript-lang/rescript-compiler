switch %node(__filename) {
| Some(f) => Node.Fs.readFileSync(f, #utf8) |> Js.log
| None => ()
}
