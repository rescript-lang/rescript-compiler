(** List map *)
type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec map f = function
  | Nil -> Nil
  | Cons (x,xs) ->  Cons (f x [@bs], map f xs)


let rec fib = function
  | 1 | 2 -> 1
  | n -> fib (n - 1 )  + fib (n - 2)

