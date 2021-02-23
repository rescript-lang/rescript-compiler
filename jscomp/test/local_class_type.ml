


class type _u = object
  method height : int [@@bs.set]
end[@bs]

type u = _u 

let f (x : u) = 
  x##height #= 3

class type v = object
  method height : int
end

let h (x : v) = x##height
