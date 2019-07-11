[@@@bs.config {no_export= true}]

type value = Natural of int | Symbol of string
type context = InSum

type t =
  | Val of value
  | Neg of t
  | Sum of t list
  | Pow of t * t
  | Frac of t * t
  | Gcd of t * t

let rec is_number (expr : t) =
  match expr with
  | Val (Natural _) -> true
  | Neg a -> is_number a
  | Val (Symbol _) | Sum _ | Gcd _ | _ -> false

let sym x = Val (Symbol x)

let rec compare context state a b =
  (*Js.log {j|$a vs $b |j};*)
  match (a, b, context) with
  | Neg x, y, _ | x, Neg y, _ ->
      (*Js.log "";*)
      compare context state x y
  | Val _, Val _, _ -> 111
  | Gcd (na, da), Gcd (nb, db), _ | Frac (na, da), Frac (nb, db), _ -> (
      let denom = compare context state da db in
      match denom = 0 with
      | true -> compare context state na nb
      | false -> denom )
  | Sum _, y, _ when is_number y -> 1
  | x, Sum _, _ when is_number x -> -1
  | Pow _, _, _ -> -1
  | _, Pow _, _ -> 1
  | Sum _, _, _ -> -1
  | _, Sum _, _ -> 1
  | Gcd _, _, _ -> 1
  | _, Gcd _, _ -> -1
  | _ -> assert false

let a = Sum [sym "a"; Val (Natural 2)]
let b = sym "x"

type st = {complex: bool}

let empty () = {complex= true}
let _ = Js.log @@ compare InSum (empty ()) a b
