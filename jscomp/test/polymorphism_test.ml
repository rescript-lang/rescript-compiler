let rec map f = function
    [] -> []
  | a::l -> let r = f a [@fn] in r :: map f l
