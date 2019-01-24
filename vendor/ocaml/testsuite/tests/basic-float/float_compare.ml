
let compare_nan () =
  not (nan < 0.0)
[@@inline never]

let x = print_endline (string_of_bool (compare_nan ()))
