let a = -0.
let b = +0.

let _ =
  assert(not (a == b))

let f () =
  let a = -0. in
  let b = +0. in
  assert(not (a == b))
