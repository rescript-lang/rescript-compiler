
type t =
  | A of (int * int * int)
  | B of int * int

let (A (a, _, b) | B (b, a)) = A (1, 2, 3)

let () = print_int a; print_int b
