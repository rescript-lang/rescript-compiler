

let x = true [@@inline]

let f = "hello" [@@inline]

let f1 = {j|a|j} [@@inline]

let f2 = {j|中文|j} [@@inline]

module N : sig 
  val f3 : string [@@inline {j|中文|j} ]
end = struct 
  let f3 = {j|中文|j} [@@inline]
end 

module N1 = functor () -> struct 
  let f4 = {j|中文|j} [@@inline]
end 
let h = f 

let hh = f ^ f 