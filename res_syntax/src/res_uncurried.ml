type config = Legacy | Swap | Always

let init = ref Legacy

let isSwap = function
  | Legacy -> false
  | Swap -> true
  | Always -> true

(* For parsing *)
let fromDotted ~dotted = function
  | Legacy -> dotted
  | Swap -> not dotted
  | Always -> true

(* For printing *)
let getDotted ~uncurried = function
  | Legacy -> uncurried
  | Swap -> not uncurried
  | Always -> false
