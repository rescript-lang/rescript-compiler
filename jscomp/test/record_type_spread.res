type x = {x: int}

type y = {
  y: int,
  ...x,
}

let getY = (v: y) => v.y
let getX = (v: y) => v.x

let v: y = {y: 3, x: 3}
