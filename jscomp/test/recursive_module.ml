let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 


module  rec Int32 : sig
  type t
  type buffer
  external buffer : t -> buffer = "buffer" [@@js.get]
  external get : t -> int -> int  = "" [@@js.get_index]
  external set : t -> int -> int -> unit = "" [@@js.set_index]
  external create : int array -> t = "Int32Array" [@@js.new]
  external of_buffer : buffer -> t = "Int32Array" [@@js.new]
end = Int32

module Xx : sig
  val f : int -> int -> int 
end = struct 
  external f : int -> int -> int = "hfiehi"
end 

let uuu = Xx.f 


module  rec Int3 : sig
  val u : int -> int 
end = Int3

module rec Inta : sig 
  val a : int lazy_t
end = struct 
  let a = lazy (Lazy.force Intb.a)
end 
and Intb : sig 
  val a : int lazy_t
end = struct 
  let a = lazy (Lazy.force Inta.a + 1)
end 

;; 
eq __LOC__ 
  (-1)
  (try Lazy.force (Intb.a) with Lazy.Undefined -> -1)

module A = struct   
  module rec Inta : sig 
    val a : int lazy_t
  end = struct 
    let a = lazy (Lazy.force Intb.a + 1)
  end 
  and Intb : sig 
    val a : int lazy_t
  end = struct 
    let a = lazy 2
  end 
end 

;; eq __LOC__ (Lazy.force A.Inta.a)  3
(* expect raise Undefined_recursive_module *)
;; eq __LOC__ 4
 (try ignore (Int3.u 3); 3
  with Undefined_recursive_module _ -> 4)


let () = 
  Mt.from_pair_suites __MODULE__ !suites