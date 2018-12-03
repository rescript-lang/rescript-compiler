

let v = Caml_utils.repeat 100 "x" 

module M ( U : sig val f : int -> string -> string [@bs] end ) = 
struct
  let v = U.f 100 "x" [@bs]
end


let f = fun [@bs] () -> 3 


let u = f () [@bs]

let (+>) = fun [@bs]  a (h : _ -> int [@bs]) -> h a [@bs]

let u h = 3 +> h  [@bs]

    