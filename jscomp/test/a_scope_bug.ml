(** FIXME: the inner [even] is printed as [even] while global [even] is printed
    as [even$1] *)
let rec odd z =
  let a =
    let even = z in
    let even = even * even in
    even + 4 + even in
  print_endline (string_of_int a) ;
  even 32

and even y = odd y
