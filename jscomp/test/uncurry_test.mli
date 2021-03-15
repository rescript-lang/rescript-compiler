type ('a0, 'a1) t = ('a0 -> 'a1 [@bs])
val f0 : (unit -> int [@bs])
val f1 : ('a -> 'a [@bs])
val f2 : ('a -> 'b -> 'a * 'b [@bs])
val f3 : ('a -> 'b -> 'c -> 'a * 'b * 'c [@bs])
val f4 : ('a -> 'b -> 'c -> 'd -> 'a * 'b * 'c * 'd [@bs])
val f5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'a * 'b * 'c * 'd * 'e [@bs])
val f6 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'a * 'b * 'c * 'd * 'e * 'f [@bs])
val f7 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a * 'b * 'c * 'd * 'e * 'f * 'g
   [@bs])
val f8 :
  ('a ->
   'b ->
   'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h
   [@bs])
val f9 :
  ('a ->
   'b ->
   'c ->
   'd ->
   'e -> 'f -> 'g -> 'h -> 'i -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i
   [@bs])
val f10 :
  ('a ->
   'b ->
   'c ->
   'd ->
   'e ->
   'f ->
   'g -> 'h -> 'i -> 'j -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j
   [@bs])
val f11 :
  ('a ->
   'b ->
   'c ->
   'd ->
   'e ->
   'f ->
   'g ->
   'h ->
   'i -> 'j -> 'k -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k
   [@bs])
val f12 :
  ('a ->
   'b ->
   'c ->
   'd ->
   'e ->
   'f ->
   'g ->
   'h ->
   'i ->
   'j ->
   'k -> 'l -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l
   [@bs])
val f13 :
  ('a ->
   'b ->
   'c ->
   'd ->
   'e ->
   'f ->
   'g ->
   'h ->
   'i ->
   'j ->
   'k ->
   'l -> 'm -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm
   [@bs])
val f14 :
  ('a ->
   'b ->
   'c ->
   'd ->
   'e ->
   'f ->
   'g ->
   'h ->
   'i ->
   'j ->
   'k ->
   'l ->
   'm ->
   'n -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm * 'n
   [@bs])
val f15 :
  ('a ->
   'b ->
   'c ->
   'd ->
   'e ->
   'f ->
   'g ->
   'h ->
   'i ->
   'j ->
   'k ->
   'l ->
   'm ->
   'n ->
   'o ->
   'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm * 'n * 'o
   [@bs])
val f16 :
  ('a ->
   'b ->
   'c ->
   'd ->
   'e ->
   'f ->
   'g ->
   'h ->
   'i ->
   'j ->
   'k ->
   'l ->
   'm ->
   'n ->
   'o ->
   'p ->
   'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm * 'n * 'o *
   'p
   [@bs])
val f17 :
  ('a ->
   'b ->
   'c ->
   'd ->
   'e ->
   'f ->
   'g ->
   'h ->
   'i ->
   'j ->
   'k ->
   'l ->
   'm ->
   'n ->
   'o ->
   'p ->
   'q ->
   'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm * 'n * 'o *
   'p * 'q
   [@bs])
val f18 :
  ('a ->
   'b ->
   'c ->
   'd ->
   'e ->
   'f ->
   'g ->
   'h ->
   'i ->
   'j ->
   'k ->
   'l ->
   'm ->
   'n ->
   'o ->
   'p ->
   'q ->
   'r ->
   'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm * 'n * 'o *
   'p * 'q * 'r
   [@bs])
val f19 :
  ('a ->
   'b ->
   'c ->
   'd ->
   'e ->
   'f ->
   'g ->
   'h ->
   'i ->
   'j ->
   'k ->
   'l ->
   'm ->
   'n ->
   'o ->
   'p ->
   'q ->
   'r ->
   's ->
   'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm * 'n * 'o *
   'p * 'q * 'r * 's
   [@bs])
val f20 :
  ('a ->
   'b ->
   'c ->
   'd ->
   'e ->
   'f ->
   'g ->
   'h ->
   'i ->
   'j ->
   'k ->
   'l ->
   'm ->
   'n ->
   'o ->
   'p ->
   'q ->
   'r ->
   's ->
   't ->
   'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm * 'n * 'o *
   'p * 'q * 'r * 's * 't
   [@bs])
val f21 :
  ('a ->
   'b ->
   'c ->
   'd ->
   'e ->
   'f ->
   'g ->
   'h ->
   'i ->
   'j ->
   'k ->
   'l ->
   'm ->
   'n ->
   'o ->
   'p ->
   'q ->
   'r ->
   's ->
   't ->
   'u ->
   'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm * 'n * 'o *
   'p * 'q * 'r * 's * 't * 'u
   [@bs])
val f22 :
  ('a ->
   'b ->
   'c ->
   'd ->
   'e ->
   'f ->
   'g ->
   'h ->
   'i ->
   'j ->
   'k ->
   'l ->
   'm ->
   'n ->
   'o ->
   'p ->
   'q ->
   'r ->
   's ->
   't ->
   'u ->
   'v ->
   'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm * 'n * 'o *
   'p * 'q * 'r * 's * 't * 'u * 'v
   [@bs])

val xx : unit -> 'a [@bs]