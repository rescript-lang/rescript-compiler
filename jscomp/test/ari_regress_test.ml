let f x = (+) x 
let g = f 3 4 

let h = ref 0 

let gg x y = let u = x + y in fun z -> u + z

let g1 x y = let u  = x + y in let () = incr h in  fun xx yy -> xx + yy + u
let x = gg 3 5 6

let v = g1 3 4 6

let suites = Mt.[
    "curry", (fun _ -> 
        Eq(g, 7)        
      );
    "curry2", (fun _ -> 
        Eq(14, (v 1|>ignore; v 1) ));    
    "curry3", (fun _ -> Eq(x, 14));
    __LOC__, (fun _ -> Eq(!h,1))
]

;; Mt.from_pair_suites __MODULE__ suites
