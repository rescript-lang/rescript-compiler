let rec map f = function
    [] -> []
  | a::l -> let r = f a [@uncurry] in r :: map f l
