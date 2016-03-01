
let f (u : nativeint) v = u > v  
let v   = 
  Int64.add (Int64.of_int32 Int32.max_int ) Int64.one
let h = Int64.neg v  
let a  = Int64.of_int32 2147483647l

let suites = Mt.[
    "add_one", (fun _ -> Eq (v, 2147483648L));
    "add_2", (fun _ -> Eq(4294967294L, Int64.(add a a )));
    "to_int32", (fun _ -> Eq(3l, Int64.to_int32 3L));
    "to_int", (fun _ -> Eq(3, Int64.to_int 3L));
    "of_int", (fun _ -> Eq(3L, Int64.of_int 3))
]

;; Mt.from_pair_suites __FILE__ suites
