val x : bool 

val f : string [@@inline "hello"]

val f1 : 
  string 
  [@@inline {j|a|j}]

val f2 :   
  string 
  [@@inline {j|中文|j}]

module N : sig   
  val f3 : string [@@inline {j|中文|j} ]
end 


module N1 : functor () -> sig 
  val f4 : string 
  [@@inline {j|中文|j}]
end 

val h : string
val hh : string