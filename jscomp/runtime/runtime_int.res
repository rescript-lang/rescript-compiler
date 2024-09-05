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
