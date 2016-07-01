

let v = Caml_utils.repeat 100 "x" [@fn]

module M ( U : sig val f : int -> string -> string [@fn] end ) = 
struct
  let v = U.f 100 "x" [@fn]
end


let f = fun [@fn] () -> 3 


let u = f () [@fn]
