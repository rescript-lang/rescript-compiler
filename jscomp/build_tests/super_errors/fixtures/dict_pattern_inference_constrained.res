let foo = dict =>
  switch dict {
  | @res.dictPattern {one: 1} =>
    let _: dict<string> = dict
    Js.log("one")
  | _ => Js.log("not one")
  }
