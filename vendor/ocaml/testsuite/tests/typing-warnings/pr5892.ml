open CamlinternalOO;;
type _ choice = Left : label choice | Right : tag choice;;
let f : label choice -> bool = function Left -> true;; (* warn *)
