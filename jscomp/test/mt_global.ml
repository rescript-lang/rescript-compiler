let collect_eq test_id suites loc x y =
  incr test_id ;
  suites :=
    (loc ^ " id " ^ string_of_int !test_id, fun _ -> Mt.Eq (x, y)) :: !suites

let collect_neq test_id suites loc x y =
  incr test_id ;
  suites :=
    (loc ^ " id " ^ string_of_int !test_id, fun _ -> Mt.Neq (x, y)) :: !suites

let collect_approx test_id suites loc x y =
  incr test_id ;
  suites :=
    (loc ^ " id " ^ string_of_int !test_id, fun _ -> Mt.Approx (x, y))
    :: !suites
