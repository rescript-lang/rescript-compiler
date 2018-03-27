
include (
  struct

let t0 x f =
  x |. f  |. f |. f

let t1 x f =
  x |. f

let t2 x f g =
  x |. f |. g x x |. f x

 let t3 x f =
   x |. f ~h:1 ~x:2
end : sig

  val t0 : 'a -> ('a -> 'a) -> 'a
  val t1 : 'a -> ('a -> 'b) -> 'b
  val t2 : 'a -> ('a -> 'a -> 'b) -> (('a -> 'b) -> 'a -> 'a -> 'a) -> 'b
  val t3 : 'a -> ('a -> h:int -> x:int -> 'b) -> 'b
end
)

let f a b c =
  a |. (b,c)



let f1 a b c d =
  ( a b)  |. (c,d)

let f2 a b c d =
  let u,v = (a b) |. (c,d) in
  u + v

let f3 a b c d =
  ( a b)
  |. (c, d)
  |. (fun (u,v) -> u + v)

let f4 a b  c =
   a |. (b c , b c )
