/*** [`Belt.Int`]()
    Utililites for Int
*/

@val external isNaN: int => bool = "isNaN"

external toFloat: int => float = "%identity"

external fromFloat: float => int = "%intoffloat"

@val external fromString: (string, @as(10) _) => int = "parseInt"

let fromString = i =>
  switch fromString(i) {
  | i if isNaN(i) => None
  | i => Some(i)
  }

@val external toString: int => string = "String"

external \"+": (int, int) => int = "%addint"

external \"-": (int, int) => int = "%subint"

external \"*": (int, int) => int = "%mulint"

external \"/": (int, int) => int = "%divint"
