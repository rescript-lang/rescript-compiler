(* For parsing *)
let from_dotted ~dotted = function
  | Config.Legacy -> dotted
  | Uncurried -> true

(* For printing *)
let get_dotted ~uncurried = function
  | Config.Legacy -> uncurried
  | Uncurried -> false
