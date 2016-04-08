

let a = 
  let v = ref 3 in 
  let action () =  incr v in
  let  f h   = 
    (fun x y -> h x y) (action (); 3) in 
  ignore @@ f (+);
  ignore @@ f (+);
  !v 

let b = 
  let v = ref 3 in 
  let action () =  incr v in
  let  f h   = 
    (fun x y -> h x y)  (action (); 3) in 
  ignore @@ f (+);
  ignore @@ f (+);
  !v 

let c = 
  let v = ref 3 in 
  let action () =  incr v in
  let  f h   = 
    (fun x y -> h x y)  2 (action (); 3) in 
  ignore @@ f (+);
  ignore @@ f (+);
  !v 

(* ;; Printf.printf "%d%d%d\n" a b c  *)

;; Mt.from_pair_suites __FILE__ Mt.[
  "partial", (fun _ -> Eq((5,5,5), (a,b,c)))
]


