type _ nat =
    Zero : [`Zero] nat
  | Succ : 'a nat -> [`Succ of 'a] nat;;
type 'a pre_nat = [`Zero | `Succ of 'a];;
type aux =
  | Aux : [`Succ of [<[<[<[`Zero] pre_nat] pre_nat] pre_nat]] nat -> aux;;

let f (Aux x) =
  match x with
  | Succ Zero -> "1"
  | Succ (Succ Zero) -> "2"
  | Succ (Succ (Succ Zero)) -> "3"
  | Succ (Succ (Succ (Succ Zero))) -> "4"
;;
