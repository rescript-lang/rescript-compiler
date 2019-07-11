let suites : Mt.pair_suites ref = ref []
let test_id = ref 0

let eq loc x y =
  incr test_id ;
  suites :=
    (loc ^ " id " ^ string_of_int !test_id, fun _ -> Mt.Eq (x, y)) :: !suites

type t

external makeDate : unit -> t = "Date" [@@bs.new]

let f () =
  let x = makeDate () in
  let y = makeDate () in
  (y > x, y < x, true)

(* y > x *)
let a0, a1, a2 = f ()

;;
Js.log2 a0 a1
;;
eq __LOC__ a2 true
;;
Mt.from_pair_suites __MODULE__ !suites
