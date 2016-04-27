

let suites = Mt.[
  "basic", (fun _ -> 
     Eq((Ext_filename.node_relative_path 
      "./a/b.c"
      "./a/u/g.c"), "./u/g")
           );
  "node", (fun _ -> 
      Eq(Ext_filename.node_relative_path
           "./a/b.c"
           "xxxghsoghos/ghsoghso/node_modules/buckle-stdlib/list.js",
         "buckle-stdlib/list.js"         
        )
    );

  "node2", (fun _ -> 
      Eq(Ext_filename.node_relative_path
           "./a/b.c"
           "xxxghsoghos/ghsoghso/node_modules//buckle-stdlib/list.js",
         "buckle-stdlib/list.js"         
        )
    );
  "node3", (fun _ -> 
      Eq(Ext_filename.node_relative_path
           "./a/b.c"
           "xxxghsoghos/ghsoghso/node_modules/./buckle-stdlib/list.js",
         "buckle-stdlib/list.js"         
        )
    )


]
;; Mt.from_pair_suites __FILE__ suites
