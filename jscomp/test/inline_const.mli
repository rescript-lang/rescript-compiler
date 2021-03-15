val x : bool 

val f : string [@@bs.inline "hello"]

val f1 : 
  string 
[@@bs.inline {j|a|j}]

val f2 :   
  string 
[@@bs.inline {j|中文|j}]

module N : sig   
  val f3 : string [@@bs.inline {j|中文|j} ]
end 


module N1 : functor () -> sig 
  val f4 : string 
  [@@bs.inline {j|中文|j}]
  val xx : float [@@bs.inline 3e-6]
end 

val h : string
val hh : string

val f5 : bool [@@bs.inline true ]

val f6 : int [@@bs.inline 1]

(* val f7 : bool [@@bs.inline 1L] *)

val v : int64 [@@bs.inline 100L]
val u : int64 [@@bs.inline 1L ] 
