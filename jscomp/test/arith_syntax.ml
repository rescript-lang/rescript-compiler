type expression =
  | Numeral of float  (** non-negative integer constant *)
  | Plus of expression * expression  (** Addition [e1 + e2] *)
  | Minus of expression * expression  (** Difference [e1 - e2] *)
  | Times of expression * expression  (** Product [e1 * e2] *)
  | Divide of expression * expression  (** Quotient [e1 / e2] *)
  | Negate of expression  (** Opposite value [-e] *)
  | Variable of string

let rec str e =
  match e with
  | Numeral f -> string_of_float f
  | Plus (a, b) -> str a ^ "+" ^ str b
  | Minus (a, b) -> str a ^ "-" ^ str b
  | Times (a, b) -> str a ^ "*" ^ str b
  | Divide (a, b) -> str a ^ "/" ^ str b
  | Negate a -> "-" ^ str a
  | Variable s -> s
