(* a polymorphic variant of test3.ml; found a real bug once *)
let test =
  let rec x = `A f
  and f = function
    | 0 -> 2
    | n -> match x with `A g -> g 0
  in
  assert (f 1 = 2)
