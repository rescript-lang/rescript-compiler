module L = List

let v x =
  let module H = List in
  let module U = L in
  (H.length x, U.length x)
