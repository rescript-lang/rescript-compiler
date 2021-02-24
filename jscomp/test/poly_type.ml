


class type history = object
  method pushState : 'a . 'a -> string  -> unit
end [@bs]

let f (x : history ) =
  x##pushState  3 "x";
  x##pushState None "x"  
