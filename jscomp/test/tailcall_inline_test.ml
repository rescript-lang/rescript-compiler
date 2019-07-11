let f () =
  let rec f acc n = if n > 0 then f (acc + n) (n - 1) else acc in
  let len = 10 in
  let v = Array.make len 0 in
  for i = 0 to len - 1 do
    v.(i) <- f 0 i
  done ;
  v

let suites =
  Mt.
    [ ("acc", fun _ -> Eq (f (), [|0; 1; 3; 6; 10; 15; 21; 28; 36; 45|]))
    ; ("array_to_list", fun _ -> Eq ([1; 2; 3], Array.to_list [|1; 2; 3|])) ]

;;
Mt.from_pair_suites __MODULE__ suites
