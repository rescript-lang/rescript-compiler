val x : bool
val f : string [@@bs.inline "hello"]
val f1 : string [@@bs.inline {j|a|j}]
val f2 : string [@@bs.inline {j|中文|j}]

module N : sig
  val f3 : string [@@bs.inline {j|中文|j}]
end

module N1 () : sig
  val f4 : string [@@bs.inline {j|中文|j}]
end

val h : string
val hh : string
val f5 : bool [@@bs.inline true]
val f6 : int [@@bs.inline 1]
