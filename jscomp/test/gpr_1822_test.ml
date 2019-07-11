let suites : Mt.pair_suites ref = ref []
let test_id = ref 0

let eq loc x y =
  incr test_id ;
  suites :=
    (loc ^ " id " ^ string_of_int !test_id, fun _ -> Mt.Eq (x, y)) :: !suites

type shape = Circle of int | Rectangle of int * int

let myShape = Circle 10

let area =
  match myShape with
  | Circle r -> float_of_int (r * r) *. 3.14
  | Rectangle (w, h) -> float_of_int (w * h)

;;
eq __LOC__ area 314.
;;
Mt.from_pair_suites __MODULE__ !suites
