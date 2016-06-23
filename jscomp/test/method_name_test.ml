let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = 
  incr test_id ; 
  suites := 
    (loc ^" id " ^ (string_of_int !test_id), (fun _ -> Mt.Eq(x,y))) :: !suites


let f x i file v = 
  x##case i ;
  x##case_set (i,v);
  x##_open file;
  x##open_ file ;
  x##_MAX_LENGTH_

let ff x i v = 
  x##make_config ;
  x##make_config_;
  x##make_config_set v ;
  x##case_unsafe i ;
  x##__open_ 3
  (* x##__open 32; *)
  (* x##case_setUnsafe (i,v) *)
(* do we need polymorphism over [case_set]
   I can only think of [case_set] will have one type
   ['key -> 'value -> void ]
   unlike [case] which may have different return types
*)


let u = [%bs.obj { _Content'type = "x" }]

let h = [%bs.obj { open_ = 3 ; end_ = 32 } ]

let hg x = 
  x##open_ + x ##end_

let () = eq __LOC__ 35 (hg  h)

let () = Mt.from_pair_suites __FILE__ !suites
