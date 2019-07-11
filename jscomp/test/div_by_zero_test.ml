let suites : Mt.pair_suites ref = ref []
let test_id = ref 0

let eq loc x y =
  incr test_id ;
  suites :=
    (loc ^ " id " ^ string_of_int !test_id, fun _ -> Mt.Eq (x, y)) :: !suites

let add suite = suites := suite :: !suites

let () =
  add (__LOC__, fun _ -> ThrowAny (fun _ -> ignore (3 / 0))) ;
  add (__LOC__, fun _ -> ThrowAny (fun _ -> ignore (3 mod 0))) ;
  add (__LOC__, fun _ -> ThrowAny (fun _ -> ignore (Int32.div 3l 0l))) ;
  add (__LOC__, fun _ -> ThrowAny (fun _ -> ignore (Int32.rem 3l 0l))) ;
  add (__LOC__, fun _ -> ThrowAny (fun _ -> ignore (Int64.div 3L 0L))) ;
  add (__LOC__, fun _ -> ThrowAny (fun _ -> ignore (Int64.rem 3L 0L)))

let div x y = (x / y) + 3
let () = Mt.from_pair_suites __MODULE__ !suites
