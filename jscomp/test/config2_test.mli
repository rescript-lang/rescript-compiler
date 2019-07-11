class type v =
  object
    method hey : int -> int -> int
  end[@bs]

class type v2 =
  object
    method hey : int -> int -> int
  end[@bs]

type vv = [%bs.obj: < hey: int -> int -> int [@bs] > ]
type vv2 = [%bs.obj: < hey: int -> int -> int [@bs] > ]

val test_v : v Js.t -> int
val test_vv : vv -> int
