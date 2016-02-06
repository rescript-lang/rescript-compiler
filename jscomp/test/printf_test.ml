
let print_pair fmt (a,b) = Format.fprintf fmt "(%d,%d)" a b

let suites = Mt.[
    "sprintf_simple", (fun _ -> Eq("3232",  Printf.sprintf "%s%d" "32" 32));
    "print_pair", (fun _ -> Eq("(1,2)", Format.asprintf "%a" print_pair (1,2)))    
  ]


;; Mt.from_pair_suites __FILE__ suites
