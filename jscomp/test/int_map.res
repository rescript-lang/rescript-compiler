include Map.Make({
  type t = int
  let compare = (x: int, y) => compare(x, y)
})
