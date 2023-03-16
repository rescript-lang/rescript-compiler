type config = Legacy | Swap | Uncurried

let init = ref Legacy

let isSwap = function
  | Legacy -> false
  | Swap -> true
  | Uncurried -> true

(* For parsing *)
let fromDotted ~dotted = function
  | Legacy -> dotted
  | Swap -> not dotted
  | Uncurried -> true

(* For printing *)
let getDotted ~uncurried = function
  | Legacy -> uncurried
  | Swap -> not uncurried
  | Uncurried -> false
