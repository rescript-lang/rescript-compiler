let f x y =
  let x, y = (ref x, ref y) in
  (!x, !y)
