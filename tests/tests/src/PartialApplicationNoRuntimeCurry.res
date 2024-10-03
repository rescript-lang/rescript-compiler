@@uncurried

let add = x => (y, z) => x + y + z

let f = u => {
  let f = add(u)
  f(1, ...)
}
