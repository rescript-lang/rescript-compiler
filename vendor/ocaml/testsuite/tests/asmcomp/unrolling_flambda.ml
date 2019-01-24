
let rec f x =
  if x > 0 then f (x - 1)
  else 0
[@@inline]

let _ = f 0
