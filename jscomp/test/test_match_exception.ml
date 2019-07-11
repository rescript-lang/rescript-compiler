let f g x = match g x with y -> y | exception Not_found -> 3
