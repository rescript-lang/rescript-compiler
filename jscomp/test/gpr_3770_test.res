type t = Foo(int, int, int)

let show = x =>
  switch x {
  | Foo(0, 0, 0) => "zeroes"
  | Foo(a, b, _) => string_of_int(a) ++ string_of_int(b)
  }
