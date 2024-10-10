let unsafelyUnwrapOption = x =>
  switch x {
  | Some(v) => v
  | None => raise(Invalid_argument("Passed `None` to unsafelyUnwrapOption"))
  }
