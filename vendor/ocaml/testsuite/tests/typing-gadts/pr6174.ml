type _ t = C : ((('a -> 'o) -> 'o) -> ('b -> 'o) -> 'o) t
let f : type a o. ((a -> o) -> o) t -> (a -> o) -> o =
 fun C k -> k (fun x -> x);;
[%%expect{|
type _ t = C : ((('a -> 'o) -> 'o) -> ('b -> 'o) -> 'o) t
Line _, characters 24-25:
Error: This expression has type $0 but an expression was expected of type
         $1 = ($2 -> $1) -> $1
|}];;
