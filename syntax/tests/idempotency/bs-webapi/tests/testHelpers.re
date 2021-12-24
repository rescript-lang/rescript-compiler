let unsafelyUnwrapOption =
  fun | Some(v) => v
      | None => raise(Invalid_argument("Passed `None` to unsafelyUnwrapOption"));
