
let test () = 
  let v = ref 0 in
  let rec f n acc = 
    if n = 0 then acc ()
    else f (n - 1) (fun _ -> v := !v + n; acc ()) in
  f 10 (fun _ -> ());
  !v

let test_closure () = 
  let n = 6 in 
  let v = ref 0 in
  let arr = Array.make n (fun  x -> x ) in
  for i = 0 to n - 1 do 
    arr.(i) <- fun _ -> i 
  done;
  Array.iter (fun  i -> v:= !v  + i 0) arr;
  !v 

let test_closure2 () = 
  let n = 6 in 
  let v = ref 0 in
  let arr = Array.make n (fun  x -> x ) in
  for i = 0 to n - 1 do
    let j = i + i in
    arr.(i) <- fun _ -> j
  done;
  Array.iter (fun  i -> v:= !v  + i 0) arr;
  !v 




;; Mt.from_pair_suites __MODULE__  Mt.[
"cps_test_sum", (fun _ -> 
  Eq (55, (test())));
 "cps_test_closure", (fun _ -> 
   Eq (15, (test_closure ())));
  "cps_test_closure2", (fun _ -> 
    Eq (30, (test_closure2 ())));
                       
]
