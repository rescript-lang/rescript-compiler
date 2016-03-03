(* Int64.bits_of_float 1.0*)
let one_float = 4607182418800017408L 

let suites = Mt.[
    "one", (fun _ -> Eq (Int64.bits_of_float 1.0, one_float));
    "two", (fun _ -> Eq (Int64.float_of_bits one_float, 1.0))
]

;; Mt.from_pair_suites __FILE__ suites
