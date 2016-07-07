
[@@@bs.config{bs_class_type}]

class type _y = object 
  method height : int [@@bs.set {no_get}]
end
type y = _y Js.t 
class type _y0 = object 
  method height : int [@@bs.set] [@@bs.get {null}]
end
type y0 = _y0 Js.t 

class type _y1 = object 
  method height : int [@@bs.set] [@@bs.get {undefined}]
end
type y1 = _y1 Js.t 

class type _y2 = object 
  method height : int [@@bs.set] [@@bs.get {undefined; null}]
end
type y2 = _y2 Js.t 

class type _y3 = object 
  method height : int  [@@bs.get {undefined ; null}]
end
type y3 = _y3 Js.t

