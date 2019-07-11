let f x = match x with `a -> "a" | `b -> "b" | `c -> "c"
let ff x = match x with "a" -> `a | "b" -> `b | "c" -> `c | _ -> assert false

let test x =
  match
    match x with "a" -> `a | "b" -> `b | "c" -> `c | _ -> assert false
  with
  | `a -> "a"
  | `b -> "b"
  | `c -> "c"

let test_poly = match `a with `a -> "a" | `b -> "b" | `c -> "c"
let c, d, e = (f `a, f `b, f `c)
