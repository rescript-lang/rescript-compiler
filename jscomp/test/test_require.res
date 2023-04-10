let () = switch %node(require) {
| None => ()
| Some(u) =>
  switch (%node(_module), Js.Undefined.toOption(u["main"])) {
  | (Some(x), Some(y)) if x === y => Js.log("is main")
  | (Some(_), Some(_))
  | (None, Some(_))
  | (None, None)
  | (Some(_), None) =>
    Js.log("not main")
  }
}
