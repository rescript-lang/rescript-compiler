
let print_pair fmt (a,b) = Format.fprintf fmt "(%d,%d)" a b

let suites = Mt.[
    "sprintf_simple", (fun _ -> Eq("3232",  Printf.sprintf "%s%d" "32" 32));
    "print_asprintf", (fun _ -> Eq("xx", Format.asprintf "xx" ));
    "print_pair", (fun _ -> Eq("(1,2)", Format.asprintf "%a" print_pair (1,2)))    
  ]

let v = Format.asprintf "xx" 
;; Mt.from_pair_suites __MODULE__ suites
