

let f g = fun [@bs] x -> g x 

let rec map f lst =
  match lst with
  | [] -> []
  | a :: rest ->
    f a [@bs] :: map f rest


let map (type u) (type v) (f : u -> v) (lst : u list) : v list =
  map (fun [@bs] x -> f x ) lst   
