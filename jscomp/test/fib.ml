let rec fib = function 0 | 1 -> 1 | n -> fib (n - 1) + fib (n - 2)

let fib2 n =
  let rec aux a b i = if n = i then a else aux b (a + b) (i + 1) in
  aux 1 1 0

let fib3 n =
  let a = ref 1 in
  let b = ref 1 in
  for i = 1 to n do
    let tmp = !a in
    a := !b ;
    b := !b + tmp
  done ;
  !a
