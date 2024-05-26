(* For parsing *)
let from_dotted ~dotted = function
  | Config.Legacy -> dotted
  | Swap -> not dotted
  | Uncurried -> true

(* For printing *)
let get_dotted ~uncurried = function
  | Config.Legacy -> uncurried
  | Swap -> not uncurried
  | Uncurried -> false
