
let length _  =  3 

(* Test name collision *)
;; Mt.from_pair_suites __MODULE__ [
  "list_length", (fun _ -> Eq(List.length [1;2], 2))  ;
  "length", (fun _ -> Eq(length [1;2],  3 ))
]
