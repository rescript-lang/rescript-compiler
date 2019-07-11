let suites : Mt.pair_suites ref = ref []
let test_id = ref 0

let eq loc x y =
  incr test_id ;
  suites :=
    (loc ^ " id " ^ string_of_int !test_id, fun _ -> Mt.Eq (x, y)) :: !suites

let f x = ignore x
let ff x = ignore (Js.log x)
let () = eq __LOC__ (f 3) ()
let () = Mt.from_pair_suites __MODULE__ !suites
