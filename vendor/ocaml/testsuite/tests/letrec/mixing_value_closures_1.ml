(* mixing values and closures may exercise interesting code paths *)
type t = A of (int -> int)
let test =
  let rec x = A f
  and f = function
    | 0 -> 2
    | n -> match x with A g -> g 0
  in assert (f 1 = 2)
