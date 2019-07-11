let rec map f = function
  | [] -> []
  | a :: l ->
      let r = (f a [@bs]) in
      r :: map f l
