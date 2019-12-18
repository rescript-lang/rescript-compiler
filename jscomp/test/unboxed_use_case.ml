

type r = { f : 'a . ('a -> 'a ) } [@@unboxed]


let map_pair r (p1, p2) = (r.f p1, r.f p2)


let u = { f = fun x -> x }


let sample = map_pair u (3, true)


type _ para = C
type t = H : _ para -> t [@@unboxed]


type any =
  | Any : 'a -> any   [@@unboxed]


let hi =  [|Any 3; Any 2 ; Any "x"|]

