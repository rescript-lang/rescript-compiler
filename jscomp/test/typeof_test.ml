



let suites = Mt.[
    "int_type", (fun _ -> Eq(Js.typeof 3, "number") );
    "string_type", (fun _ -> Eq(Js.typeof "x", "string"))    
]

;; Mt.from_pair_suites __FILE__ suites 
