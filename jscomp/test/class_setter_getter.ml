class type _y =
  object
    method height : int [@@bs.set {no_get}]
  end[@bs]

type y = _y Js.t

class type _y0 =
  object
    method height : int [@@bs.set] [@@bs.get {null}]
  end[@bs]

type y0 = _y0 Js.t

class type _y1 =
  object
    method height : int [@@bs.set] [@@bs.get {undefined}]
  end[@bs]

type y1 = _y1 Js.t

class type _y2 =
  object
    method height : int [@@bs.set] [@@bs.get {undefined; null}]
  end[@bs]

type y2 = _y2 Js.t

class type _y3 =
  object
    method height : int [@@bs.get {undefined; null}]
  end[@bs]

type y3 = _y3 Js.t
type yy2 = < height: int [@bs.get {undefined; null}] [@bs.set] > Js.t

let fff (x : yy2) = x ## height #= 2
let ff (x : y2) (z : yy2) = [x##height; z##height]
