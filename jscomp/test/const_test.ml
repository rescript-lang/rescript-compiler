let f x = if true then x else x - 1
let ff x = match Some x with Some _ -> x | None -> 0

type t = A of int | B of int | C of int

let fff x = match A x with A x -> x | B _ -> 1 | C _ -> 2
let h x = match x with `A -> 0 | `B -> 1 | `C -> 2
let hh () = match "x" with "y" -> 1 | "z" -> 2 | "x" -> 3 | _ -> 4
let g = h `A
