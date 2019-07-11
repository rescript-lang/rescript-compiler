(*;; if bool_of_string "x" then "" else "" *)

let suites : Mt.pair_suites ref = ref []
let test_id = ref 0

let eq loc x y =
  incr test_id ;
  suites :=
    (loc ^ " id " ^ string_of_int !test_id, fun _ -> Mt.Eq (x, y)) :: !suites

let v = ref 3
let update () = incr v ; true

;;
if update () then "" else ""
;;
eq __LOC__ !v 4
;;
Mt.from_pair_suites __MODULE__ !suites
