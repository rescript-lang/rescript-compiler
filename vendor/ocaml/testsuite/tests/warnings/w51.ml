
let rec fact = function
  | 1 -> 1
  | n -> n * (fact [@tailcall]) (n-1)
;;
