type u = A of int | B of u | C of (int * int) | F of u * int | G | H

let f = function A _ -> 0 | B _ -> 1 | C _ -> 2 | F _ -> 3 | G -> 4 | H -> 5

type ('a, 'b) result = Left of 'a | Error of 'b

let bind x f = match x with Left x -> Left (f x) | Error _ as y -> y
