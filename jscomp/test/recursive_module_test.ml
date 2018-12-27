let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites
let add suite = 
  suites := suite :: !suites


module  rec Int3 : sig
  val u : int -> int 
end = Int3


module Fact = struct
  module type S = sig
    val fact : int -> int
  end
  module rec M : S= struct
    let fact n =
      if n <= 1 then 1
      else n * (M.fact (n - 1))
  end
  include M
end


let () = 
  eq __LOC__ 120 (Fact.fact 5)


let () = 
  add (__LOC__, (fun _ -> Mt.ThrowAny (fun _ -> ignore (Int3.u 3))))


let () = Mt.from_pair_suites __MODULE__ !suites
