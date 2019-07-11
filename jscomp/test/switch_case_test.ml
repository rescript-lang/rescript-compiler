let suites : Mt.pair_suites ref = ref []
let test_id = ref 0

let eq loc x y =
  incr test_id ;
  suites :=
    (loc ^ " id " ^ string_of_int !test_id, fun _ -> Mt.Eq (x, y)) :: !suites

let f x =
  match x with
  | "xx'''" -> 0
  | "xx\"" -> 1
  | {|xx\"|} -> 2
  | {|xx\""|} -> 3
  | _ -> 4

let () =
  eq __LOC__ (f "xx'''") 0 ;
  eq __LOC__ (f "xx\"") 1 ;
  eq __LOC__ (f {|xx\"|}) 2 ;
  eq __LOC__ (f {|xx\""|}) 3

let () = Mt.from_pair_suites __MODULE__ !suites
