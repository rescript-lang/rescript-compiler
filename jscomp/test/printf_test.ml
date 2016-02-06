


let suites = Mt.[
    "sprintf_simple", (fun _ -> Eq("3232",  Printf.sprintf "%s%d" "32" 32))
  ]


;; Mt.from_pair_suites __FILE__ suites
