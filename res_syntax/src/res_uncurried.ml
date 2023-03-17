(* For parsing *)
let fromDotted ~dotted = function
  | Config.Legacy -> dotted
  | Swap -> not dotted
  | Uncurried -> true

(* For printing *)
let getDotted ~uncurried = function
  | Config.Legacy -> uncurried
  | Swap -> not uncurried
  | Uncurried -> false
