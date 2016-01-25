

external typeof : 'a -> string = "js_typeof"

let suites = Mt.[
    "int_type", (fun _ -> Eq(typeof 3, "number") );
    "string_type", (fun _ -> Eq(typeof "x", "string"))    
]

;; Mt.from_pair_suites __FILE__ suites 
