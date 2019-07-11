let f () =
  let v = ref 0 in
  let acc = ref 0 in
  let rec loop n : int =
    if !v > n then !acc
    else (
      acc := !acc + !v ;
      incr v ;
      loop n ) in
  loop 10

let suites = Mt.[("sum", fun _ -> Eq (55, f ()))]

;;
Mt.from_pair_suites __MODULE__ suites
