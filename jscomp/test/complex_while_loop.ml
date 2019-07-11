let f () =
  let n = ref 0 in
  while
    let rec fib = function 0 | 1 -> 1 | n -> fib (n - 1) + fib (n - 2) in
    fib !n > 10
  do
    print_endline (string_of_int !n) ;
    incr n
  done

let ff () =
  while
    let a = 3 in
    let b = a * a in
    a + b > 10
  do
    ()
  done
