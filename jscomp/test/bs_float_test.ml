let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites loc x y ~test_id ~suites
let b loc x  = Mt.bool_suites loc x ~test_id ~suites
let throw loc x = Mt.throw_suites ~test_id ~suites loc x
let neq loc x y =
  incr test_id ;
  suites :=
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Neq(x,y))) :: !suites

module F = Belt.Float

let () =
  eq __LOC__ (F.fromInt 1) 1.0;
  eq __LOC__ (F.fromInt (-1)) (-1.0)

let () =
  eq __LOC__ (F.toInt 1.0) 1;
  eq __LOC__ (F.toInt 1.3) 1;
  eq __LOC__ (F.toInt 1.7) 1;
  eq __LOC__ (F.toInt (-1.0)) (-1);
  eq __LOC__ (F.toInt (-1.5)) (-1);
  eq __LOC__ (F.toInt (-1.7)) (-1)

let () =
  eq __LOC__ (F.fromString "1") (Some 1.0);
  eq __LOC__ (F.fromString "-1") (Some (-1.0));
  eq __LOC__ (F.fromString "1.7") (Some 1.7);
  eq __LOC__ (F.fromString "-1.0") (Some (-1.0));
  eq __LOC__ (F.fromString "-1.5") (Some (-1.5));
  eq __LOC__ (F.fromString "-1.7") (Some (-1.7));
  eq __LOC__ (F.fromString "not a float") None

let () =
  eq __LOC__ (F.toString 1.0) "1";
  eq __LOC__ (F.toString (-1.0)) "-1";
  eq __LOC__ (F.toString (-1.5)) "-1.5"

let () =
  let open F in
  eq __LOC__ (2.0 + 3.0) 5.0;
  eq __LOC__ (2.0 - 3.0) (-1.0);
  eq __LOC__ (2.0 * 3.0) 6.0;
  eq __LOC__ (3.0 / 2.0) 1.5;

;; Mt.from_pair_suites __LOC__ !suites
