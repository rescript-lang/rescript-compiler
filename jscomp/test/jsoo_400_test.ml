let u () =
  match String.length "123" with
  | n -> 3 / 0
  | exception _ -> 42
