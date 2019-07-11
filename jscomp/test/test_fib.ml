let rec fib = function 0 | 1 -> 1 | n -> fib (n - 1) + fib (n - 2)
let rec fib2 = function 1 | 2 -> 1 | n -> fib2 (n - 1) + fib2 (n - 2)

(* let n = List.length *)

let b = fib

let sum =
  let v = ref 0 in
  for i = 0 to 10 do
    v := !v + i
  done ;
  !v

let sumdown =
  let v = ref 0 in
  for i = 10 downto 0 do
    v := !v + i
  done ;
  !v

type list = Nil | Cons of int * list

let cons x y = Cons (x, y)

(* let cons2 (x,y) = Cons(x,y) (\* this seems to need be fixed *\) *)
let rec length x = match x with Nil -> 0 | Cons (_, y) -> 1 + length y
let rec map f x = match x with Nil -> Nil | Cons (x, y) -> Cons (f x, map f y)

let f x =
  let v = ref x in
  let sum = ref 0 in
  while !v > 0 do
    sum := !sum + !v ;
    decr v
  done ;
  !sum

let fib3 n =
  let rec fib_help a b n = if n > 0 then fib_help b (a + b) (n - 1) else a in
  fib_help 0 1 n
