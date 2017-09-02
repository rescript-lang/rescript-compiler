let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites

let test = Ext_filename_test.node_relative_path true 
let () =
  eq __LOC__ 
    (let (//) = Ext_filename_test.combine in 
     ("/tmp"// "subdir/file.txt",
      "/tmp"// "/a/tmp.txt",
      "/a/tmp.txt" // "subdir/file.txt"      
    ))
    ("/tmp/subdir/file.txt",
     "/a/tmp.txt",
     "/a/tmp.txt/subdir/file.txt"      
    )

  ;    
  eq __LOC__
    (test
       (`File "./a/b.c")
       (`File "./a/u/g.c")) "./u/g.c";

  eq __LOC__ 
    (test
       (`File "./a/b.c")
       (`File "xxxghsoghos/ghsoghso/node_modules/buckle-stdlib/list.js"))
    "buckle-stdlib/list.js" ;

  eq __LOC__ 
    (test
           (`File "./a/b.c")
           (`File "xxxghsoghos/ghsoghso/node_modules//buckle-stdlib/list.js"))
         "buckle-stdlib/list.js" ;

  eq __LOC__ 
    (test
           (`File "./a/b.c")
           (`File "xxxghsoghos/ghsoghso/node_modules/./buckle-stdlib/list.js"))
    "buckle-stdlib/list.js"    ;

  eq __LOC__ 
    (test
           (`File "./a/c.js")
           (`File "./a/b"))
    "./b"         ;
  eq __LOC__ 
    (test
           (`File "./a/c")
           (`File "./a/b.js"))
    "./b.js"         ;
  eq __LOC__
    (test
           (`Dir "./a/")
           (`File "./a/b.js"))
    "./b.js";
  eq __LOC__ 
    (Ext_filename_test.get_extension "a.txt"
    )
    ".txt";
  eq __LOC__ 
    (Ext_filename_test.get_extension "a"
    )
    "";
  eq __LOC__ 
    (Ext_filename_test.get_extension ".txt"
    )
    ".txt";

  eq __LOC__
    (Array.map Ext_filename_test.normalize_absolute_path
       [|
         "/gsho/./..";
         "/a/b/../c../d/e/f";
         "/a/b/../c/../d/e/f";
         "/gsho/./../..";
         "/a/b/c/d";
         "/a/b/c/d/";
         "/a/";
         "/a";
         "/a.txt/";
         "/a.txt"
       |])
     [|
       "/";
       "/a/c../d/e/f";
       "/a/d/e/f";
       "/";
       "/a/b/c/d" ;
       "/a/b/c/d";
       "/a";
       "/a";
       "/a.txt";
       "/a.txt"
     |] 
  ;

  

;; Mt.from_pair_suites __FILE__ !suites
