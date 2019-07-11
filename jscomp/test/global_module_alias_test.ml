let suites : Mt.pair_suites ref = ref []
let test_id = ref 0

let eq loc x y =
  incr test_id ;
  suites :=
    (loc ^ " id " ^ string_of_int !test_id, fun _ -> Mt.Eq (x, y)) :: !suites

module A = List
module B = A
module C = B
module D = C
module E = D
module F = E

(* module alias is reolved B -> 0, any point to B will be ponited to A after
   type checking *)
module type S = module type of List

let v = ref 0

module Make (U : S) = struct
  let () = incr v ; incr v ; incr v

  include U
end

let f () =
  let () = incr v ; incr v ; incr v in
  let module G = F in
  (* local module is not module alias *)
  let module H = G in
  (module Make (H) : S)

let () = eq __LOC__ (C.length [1; 2]) 2

module H = Make (Make (Make (Make (F))))

let () = eq __LOC__ !v 12

let g () =
  let module A0 = List in
  (* since module alias compiled to no code, so whenever {[ let module A0 =
     Global_module ]} happens we should just unpack it *)
  let module A1 = A0 in
  let module A2 = A1 in
  let module A3 = A2 in
  A3.length [1; 2; 3; 4]

let xx () =
  let module A0 = List in
  (* since module alias compiled to no code, so whenever {[ let module A0 =
     Global_module ]} happens we should just unpack it *)
  let module A1 = A0 in
  let module A2 = A1 in
  let module A3 = A2 in
  (module Make (A3) : S)

let () = eq __LOC__ (g ()) 4

let () =
  let module V = (val xx () : S) in
  eq __LOC__ (V.length [1; 2; 3]) 3 ;
  eq __LOC__ !v 15 ;
  let module H = (val f () : S) in
  eq __LOC__ (H.length [1; 2]) 2 ;
  eq __LOC__ !v 21

;;
Mt.from_pair_suites __MODULE__ !suites
