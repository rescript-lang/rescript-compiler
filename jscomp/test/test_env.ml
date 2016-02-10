let add x y = x + y


let add2 x y = x - y

let rec iter (f : _ -> unit) = function
  | [] -> ()
  | a::l -> f a; iter f l


