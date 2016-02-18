

let suites = Mt.[
  "basic", (fun _ -> 
     Eq((Ext_filename.node_relative_path 
      "./a/b.c"
      "./a/u/g.c"), "./u/g")
           )
]
;; Mt.from_pair_suites __FILE__ suites
