let compare = (x: int, y: int): int =>
  if x < y {
    -1
  } else if x == y {
    0
  } else {
    1
  }

let min = (x: int, y: int): int =>
  if x < y {
    x
  } else {
    y
  }

let max = (x: int, y: int): int =>
  if x > y {
    x
  } else {
    y
  }

external div: (int, int) => int = "%divint"

let div = (x: int, y: int) =>
  if y == 0 {
    raise(Division_by_zero)
  } else {
    div(x, y)
  }

external mod_: (int, int) => int = "%modint"

let mod_ = (x: int, y: int) =>
  if y == 0 {
    raise(Division_by_zero)
  } else {
    mod_(x, y)
  }
