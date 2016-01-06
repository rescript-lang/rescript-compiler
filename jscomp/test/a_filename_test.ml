open Mt

let suites = [
  "basic", (fun _ -> 
    assert_equal (Ext_filename.node_relative_path 
      "./a/b.c"
      "./a/u/g.c") "./u/g"
           )
]
;; from_suites __FILE__ suites
