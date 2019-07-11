let suites : Mt.pair_suites ref = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y

module K = Array.Floatarray

let () =
  let len = 5 in
  let v = K.create len in
  for i = 0 to len - 1 do
    K.unsafe_set v i 0.
  done ;
  K.set v 2 0x1.fp3 ;
  eq __LOC__ (K.length v, K.unsafe_get v 2, K.get v 1) (len, 0x1.fp3, 0.)

let () = Mt.from_pair_suites __MODULE__ !suites
