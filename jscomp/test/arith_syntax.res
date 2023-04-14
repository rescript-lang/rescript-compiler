type rec expression =
  | @ocaml.doc(" non-negative integer constant ") Numeral(float)
  | @ocaml.doc(" Addition [e1 + e2] ") Plus(expression, expression)
  | @ocaml.doc(" Difference [e1 - e2] ") Minus(expression, expression)
  | @ocaml.doc(" Product [e1 * e2] ") Times(expression, expression)
  | @ocaml.doc(" Quotient [e1 / e2] ") Divide(expression, expression)
  | @ocaml.doc(" Opposite value [-e] ") Negate(expression)
  | Variable(string)

let rec str = e =>
  switch e {
  | Numeral(f) => string_of_float(f)
  | Plus(a, b) => str(a) ++ ("+" ++ str(b))
  | Minus(a, b) => str(a) ++ ("-" ++ str(b))
  | Times(a, b) => str(a) ++ ("*" ++ str(b))
  | Divide(a, b) => str(a) ++ ("/" ++ str(b))
  | Negate(a) => "-" ++ str(a)
  | Variable(s) => s
  }
