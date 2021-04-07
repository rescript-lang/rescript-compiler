


class type u = object
  method height : int [@@bs.set]
end



let f (x : u) = 
  x##height #= 3

class type v = object
  method height : int
end

let h (x : v) = x##height
