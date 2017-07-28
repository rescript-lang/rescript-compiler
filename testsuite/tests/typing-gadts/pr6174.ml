type _ t = C : ((('a -> 'o) -> 'o) -> ('b -> 'o) -> 'o) t
let f : type a o. ((a -> o) -> o) t -> (a -> o) -> o =
 fun C k -> k (fun x -> x);;
