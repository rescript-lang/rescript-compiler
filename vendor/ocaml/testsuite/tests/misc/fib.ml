let rec fib n =
  if n < 2 then 1 else fib(n-1) + fib(n-2)

let _ =
  let n =
    if Array.length Sys.argv >= 2
    then int_of_string Sys.argv.(1)
    else 30 in
  print_int(fib n); print_newline(); exit 0
