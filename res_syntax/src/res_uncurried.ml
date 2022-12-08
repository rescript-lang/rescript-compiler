type config = Legacy | Default

let init = Legacy

let isDefault = function
  | Legacy -> false
  | Default -> true

(* For parsing *)
let fromDotted ~dotted = function
  | Legacy -> dotted
  | Default -> not dotted

(* For printing *)
let getDotted ~uncurried = function
  | Legacy -> uncurried
  | Default -> not uncurried
