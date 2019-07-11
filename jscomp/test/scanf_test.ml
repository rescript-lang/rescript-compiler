let suites : Mt.pair_suites ref = ref []
let test_id = ref 0
let eq f (a, b) = Mt_global.collect_eq test_id suites f a b

let () =
  eq __LOC__ (Scanf.sscanf "32 31" "%d %d" (fun x y -> x + y), 63) ;
  eq __LOC__
    ( Scanf.sscanf "12306459064359371967" "%Lu" (fun i -> i)
    , -6140285009350179649L )

let () = Mt.from_pair_suites __MODULE__ !suites
