let foo = dict =>
  switch dict {
  | @res.dictPattern {one: 1, two: "hello"} => Js.log("one")
  | _ => Js.log("not one")
  }
