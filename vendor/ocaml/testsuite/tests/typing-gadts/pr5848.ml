module B : sig
 type (_, _) t = Eq: ('a, 'a) t
 val f: 'a -> 'b -> ('a, 'b) t
end
=
struct
 type (_, _) t = Eq: ('a, 'a) t
 let f t1 t2 = Obj.magic Eq
end;;

let of_type: type a. a -> a = fun x ->
  match B.f x 4 with
  | Eq -> 5
;;
[%%expect{|
module B :
  sig type (_, _) t = Eq : ('a, 'a) t val f : 'a -> 'b -> ('a, 'b) t end
Line _, characters 4-6:
Error: The GADT constructor Eq of type B.t must be qualified in this pattern.
|}];;
