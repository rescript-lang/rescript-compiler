
let v = [%bs.raw{|Number.EPSILON?Number.EPSILON:2.220446049250313e-16|}]

let suites = Mt.[
    "epsilon", (fun _ -> Eq(epsilon_float,  v));
    "raw_epsilon", (fun _ -> Eq(2.220446049250313e-16,v ))
  ]

;; Mt.from_pair_suites __MODULE__ suites
