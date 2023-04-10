let f = str =>
  switch Node.test(str) {
  | (Node.String, s) => Js.log(("string", s))
  | (Node.Buffer, s) => Js.log(("buffer", Node.Buffer.isBuffer(s)))
  }

let () = {
  f(%raw(`"xx"`))
  f(%raw(`Buffer.from ('xx')`))
}
