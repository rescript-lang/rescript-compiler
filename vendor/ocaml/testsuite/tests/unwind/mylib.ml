let foo1 f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 =
  f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10

let foo2 f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 =
  f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10

external func_with_10_params:
  int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> unit
  = "ml_func_with_10_params_bytecode" "ml_func_with_10_params_native"

let bar x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 =
  func_with_10_params x1 x2 x3 x4 x5 x6 x7 x8 x9 x10;
  func_with_10_params x1 x2 x3 x4 x5 x6 x7 x8 x9 x10

external perform_stack_walk: unit -> unit = "ml_perform_stack_walk"

let baz x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 =
  func_with_10_params x1 x2 x3 x4 x5 x6 x7 x8 x9 x10;
  func_with_10_params x1 x2 x3 x4 x5 x6 x7 x8 x9 x10;
  perform_stack_walk ()
