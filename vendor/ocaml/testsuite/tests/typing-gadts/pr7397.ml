type +'a t

class type a = object
 method b : b
end

and b = object
 method a : a
end

type _ response =
 | C : #a t response;;
[%%expect{|
type +'a t
class type a = object method b : b end
and b = object method a : a end
type _ response = C : #a t response
|}]

let f (type a) (a : a response) =
 match a with
 | C -> 0;;
[%%expect{|
val f : 'a response -> int = <fun>
|}]
