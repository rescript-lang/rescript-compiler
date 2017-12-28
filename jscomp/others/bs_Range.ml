

let rec forAll s f p =
  if s > f then true
  else
    p s [@bs] &&
    (forAll (s + 1) f p )
  

let rec forAllBy s f step p =
  if s > f then true
  else
    p s [@bs] &&
    (forAllBy (s + step) f step p )
    


let rec exists s f p =  
  if s > f then false
  else
    p s [@bs] ||
    (exists (s + 1) f p )
  

let rec existsBy s f step p =  
  if s > f then false
  else
    p s [@bs] ||
    (existsBy (s + step) f step p )
    