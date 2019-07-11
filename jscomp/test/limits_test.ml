let suites : Mt.pair_suites ref = ref []
let test_id = ref 0

let eq loc x y =
  incr test_id ;
  suites :=
    (loc ^ " id " ^ string_of_int !test_id, fun _ -> Mt.Eq (x, y)) :: !suites

let () =
  eq __LOC__ max_int [%bs.raw "2147483647"] ;
  eq __LOC__ min_int [%bs.raw "-2147483648"] ;
  eq __LOC__ Int32.max_int [%bs.raw "2147483647"] ;
  eq __LOC__ Int32.min_int [%bs.raw "-2147483648"]

let () = Mt.from_pair_suites __MODULE__ !suites
