



let suites = Mt.[
    "normal", (fun _ -> Eq ( Lam_methname.process "xx", (Unknown None, "xx")));
    "js", (fun _ -> Eq ( Lam_methname.process "xx__js", (Js None, "xx")));
    "js_set", (fun _ -> Eq ( Lam_methname.process "xx__w", (Js_write, "xx")));
    "js_none", (fun _ -> Eq ( Lam_methname.process "xx__", (Js None, "xx")));
    "js1", (fun _ -> Eq ( Lam_methname.process "xx__js_1", (Js (Some 1), "xx")));
    "js2", (fun _ -> Eq ( Lam_methname.process "xx__w", (Js_write , "xx")));
    "js3", (fun _ -> Eq ( Lam_methname.process "xx__2", (Js (Some 2) , "xx")));
    "ml1", (fun _ -> Eq ( Lam_methname.process "xx__2_ml", (Ml (Some 2) , "xx")));
    "index", (fun _ -> Eq ( Lam_methname.process "index__", (Js_read_index , "index")));
    "set_index", (fun _ -> Eq ( Lam_methname.process "index__w", (Js_write_index , "index")));
]
;; Mt.from_pair_suites __FILE__ suites
