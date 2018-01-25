

let rec every s f p =
  if s > f then true
  else
    p s [@bs] &&
    (every (s + 1) f p )
  

let rec everyBy s f step p =
  if s > f then true
  else
    p s [@bs] &&
    (everyBy (s + step) f step p )
    


let rec some s f p =  
  if s > f then false
  else
    p s [@bs] ||
    (some (s + 1) f p )
  

let rec someBy s f step p =  
  if s > f then false
  else
    p s [@bs] ||
    (someBy (s + step) f step p )
    
