let suites : Mt.pair_suites ref = ref []
let test_id = ref 0

let eq loc x y =
  incr test_id ;
  suites :=
    (loc ^ " id " ^ string_of_int !test_id, fun _ -> Mt.Eq (x, y)) :: !suites

let () =
  eq __LOC__ Js.Null.empty Js.Null.empty ;
  ( match Js.Types.classify Js.Null.empty with
  | JSNull -> eq __LOC__ true true
  | _ -> eq __LOC__ true false ) ;
  eq __LOC__ true (Js.Types.test Js.Null.empty Null)

;;
Mt.from_pair_suites __LOC__ !suites
