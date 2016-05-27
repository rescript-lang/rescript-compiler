

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
  (* x##case_setUnsafe (i,v) *)
(* do we need polymorphism over [case_set]
   I can only think of [case_set] will have one type
   ['key -> 'value -> void ]
   unlike [case] which may have different return types
*)
