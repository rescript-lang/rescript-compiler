type t =
  | Foo of int * int * int

let show =
  function
  | Foo (0, 0, 0) -> "zeroes"
  | Foo (a, b, _) -> (string_of_int a) ^ (string_of_int b)