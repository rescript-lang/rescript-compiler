type expr =
  | Int of int
  | Var of string
  | Add of expr * expr
  | Mul of expr * expr

let rec ( +: ) f g = match f, g with 
  | Int n, Int m -> Int (n + m) 
  | Int 0, e | e, Int 0 -> e
  | f, Add(g, h) -> f +: g +: h 
  | f, g -> Add(f, g)

let rec ( *: ) f g = match f, g with 
  | Int n, Int m -> Int (n * m) 
  | Int 0, _ | _, Int 0 -> Int 0 
  | Int 1, e | e, Int 1 -> e 
  | f, Mul(g, h) -> f *: g *: h 
  | f, g -> Mul(f, g)

let rec simplify = function 
  | Int _ | Var _ as f -> f 
  | Add(f, g) -> simplify f +: simplify g 
  | Mul(f, g) -> simplify f *: simplify g