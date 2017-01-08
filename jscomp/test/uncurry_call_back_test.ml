[@@@bs.config{no_export}]

let rec map_uncurry f a = 
    match a with 
    | [] -> []
    | x::xs -> f x [@bs] :: map_uncurry f xs

let map f a = map_uncurry (fun [@bs] x -> f x) a 

let u = map (fun x -> x + 1)  [1;2]  

let rec map2 f a = 
    match a with 
    | [] -> []
    | x::xs -> f x :: map2 f xs 

let u = map2 (fun x -> x + 1)     [1;2]