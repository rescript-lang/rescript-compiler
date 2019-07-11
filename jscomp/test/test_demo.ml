let f x = x + 1
let rec fib = function 1 | 2 -> 1 | n -> fib (n - 1) + fib (n - 2)

type intlist = Nil | Cons of int * intlist

let nil = Nil
let cons x y = Cons (x, y)
let rec map f = function Nil -> Nil | Cons (x, xs) -> Cons (f x, map f xs)

let sum n =
  let v = ref 0 in
  for i = 0 to n do
    v := !v + i
  done ;
  !v

let len = List.length

let f g x =
  let u = match g x with "aabb" -> 0 | "bbc" -> 1 | _ -> 2 in
  u + 3

[@@@warning "-37-8"]

let f g x =
  let u = match g x with "aabb" -> 0 | "bbc" -> 1 in
  u + 3

let v a () : unit =
  while ignore true ; a 3 do
    ignore (print_int 3)
  done

type cxt = A | B of int | C of int * string | D | E | F of string

let f (x : cxt) =
  let u =
    match x with A -> 0 | B _ -> 1 | C _ -> 2 | D -> 3 | E -> 4 | F _ -> 5
  in
  u + 3

let f g (x : cxt) =
  g @@ match x with A -> 0 | B _ -> 1 | C _ -> 2 | D -> 3 | E -> 4 | F _ -> 5

let f h g x = h @@ try g x with Not_found -> 0
let f x y z = x + y + z

let g x y =
  let u = x + y in
  fun z -> u + z

let g1 x y =
  let u = x + y in
  fun xx yy -> xx + yy + u

let x = g 3 5 6
let v = g1 3 4 6
