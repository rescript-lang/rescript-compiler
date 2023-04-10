let v = ref(0)

while {
  print_endline(string_of_int(v.contents))
  incr(v)
  v.contents < 10
} {
  ignore()
}
let rec fib = x =>
  switch x {
  | 0 | 1 => 1
  | n => fib(n - 1) + fib(n - 2)
  }

let x = ref(3)

while {
  let y = ref(3)
  print_endline(string_of_int(x.contents))
  incr(y)
  incr(x)
  fib(x.contents) + fib(x.contents) < 20
} {
  print_endline(string_of_int(3))
}
