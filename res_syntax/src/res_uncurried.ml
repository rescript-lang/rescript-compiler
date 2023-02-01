type config = Legacy | Default | Always

let init = ref Legacy

let isDefault = function
  | Legacy -> false
  | Default -> true
  | Always -> true

(* For parsing *)
let fromDotted ~dotted = function
  | Legacy -> dotted
  | Default -> not dotted
  | Always -> true

(* For printing *)
let getDotted ~uncurried = function
  | Legacy -> uncurried
  | Default -> not uncurried
  | Always -> false
