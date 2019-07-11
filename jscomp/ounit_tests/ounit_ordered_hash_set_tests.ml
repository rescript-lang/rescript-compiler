let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))
let ( =~ ) = OUnit.assert_equal

let suites =
  __FILE__
  >::: [ ( __LOC__
         >:: fun _ ->
         let a = [|"a"; "b"; "c"|] in
         Ordered_hash_set_string.(to_sorted_array (of_array a)) =~ a )
       ; ( __LOC__
         >:: fun _ ->
         let a = Array.init 1000 (fun i -> string_of_int i) in
         Ordered_hash_set_string.(to_sorted_array (of_array a)) =~ a )
       ; ( __LOC__
         >:: fun _ ->
         let a = [|"a"; "b"; "c"; "a"; "d"|] in
         Ordered_hash_set_string.(to_sorted_array (of_array a))
         =~ [|"a"; "b"; "c"; "d"|] )
       ; ( __LOC__
         >:: fun _ ->
         let b = Array.init 500 (fun i -> string_of_int i) in
         let a = Array.append b b in
         Ordered_hash_set_string.(to_sorted_array (of_array a)) =~ b )
       ; ( __LOC__
         >:: fun _ ->
         let h = Ordered_hash_set_string.create 1 in
         Ordered_hash_set_string.(to_sorted_array h) =~ [||] ;
         Ordered_hash_set_string.add h "1" ;
         print_endline
           ( "\n" ^ __LOC__ ^ "\n"
           ^ Ext_util.stats_to_string (Ordered_hash_set_string.stats h) ) ;
         Ordered_hash_set_string.(to_sorted_array h) =~ [|"1"|] )
       ; ( __LOC__
         >:: fun _ ->
         let h = Ordered_hash_set_string.create 1 in
         let count = 3000 in
         for i = 0 to count - 1 do
           Ordered_hash_set_string.add h (string_of_int i)
         done ;
         print_endline
           ( "\n" ^ __LOC__ ^ "\n"
           ^ Ext_util.stats_to_string (Ordered_hash_set_string.stats h) ) ;
         Ordered_hash_set_string.(to_sorted_array h)
         =~ Array.init count (fun i -> string_of_int i) )
       ; ( __LOC__
         >:: fun _ ->
         let h = Ordered_hash_set_string.create 1 in
         let count = 1000_000 in
         for i = 0 to count - 1 do
           Ordered_hash_set_string.add h (string_of_int i)
         done ;
         for i = 0 to count - 1 do
           OUnit.assert_bool "exists"
             (Ordered_hash_set_string.mem h (string_of_int i))
         done ;
         for i = 0 to count - 1 do
           OUnit.assert_equal
             (Ordered_hash_set_string.rank h (string_of_int i))
             i
         done ;
         OUnit.assert_equal
           (Ordered_hash_set_string.fold
              (fun key rank acc ->
                assert (string_of_int rank = key) ;
                acc + 1)
              h 0)
           count ;
         Ordered_hash_set_string.iter
           (fun key rank -> assert (string_of_int rank = key))
           h ;
         OUnit.assert_equal (Ordered_hash_set_string.length h) count ;
         print_endline
           ( "\n" ^ __LOC__ ^ "\n"
           ^ Ext_util.stats_to_string (Ordered_hash_set_string.stats h) ) ;
         Ordered_hash_set_string.clear h ;
         OUnit.assert_equal (Ordered_hash_set_string.length h) 0 )
       ; ( __LOC__
         >:: fun _ ->
         let count = 1000_000 in
         let h = Ordered_hash_set_string.create count in
         for i = 0 to count - 1 do
           Ordered_hash_set_string.add h (string_of_int i)
         done ;
         for i = 0 to count - 1 do
           OUnit.assert_bool "exists"
             (Ordered_hash_set_string.mem h (string_of_int i))
         done ;
         for i = 0 to count - 1 do
           OUnit.assert_equal
             (Ordered_hash_set_string.rank h (string_of_int i))
             i
         done ;
         OUnit.assert_equal
           (Ordered_hash_set_string.fold
              (fun key rank acc ->
                assert (string_of_int rank = key) ;
                acc + 1)
              h 0)
           count ;
         Ordered_hash_set_string.iter
           (fun key rank -> assert (string_of_int rank = key))
           h ;
         OUnit.assert_equal (Ordered_hash_set_string.length h) count ;
         print_endline
           ( "\n" ^ __LOC__ ^ "\n"
           ^ Ext_util.stats_to_string (Ordered_hash_set_string.stats h) ) ;
         Ordered_hash_set_string.clear h ;
         OUnit.assert_equal (Ordered_hash_set_string.length h) 0 )
       ; ( __LOC__
         >:: fun _ ->
         Ordered_hash_set_string.to_sorted_array
           (Ordered_hash_set_string.of_array [||])
         =~ [||] ;
         Ordered_hash_set_string.to_sorted_array
           (Ordered_hash_set_string.of_array [|"1"|])
         =~ [|"1"|] )
       ; ( __LOC__
         >:: fun _ ->
         OUnit.assert_raises Not_found (fun _ ->
             Ordered_hash_set_string.choose_exn
               (Ordered_hash_set_string.of_array [||])) )
       ; ( __LOC__
         >:: fun _ ->
         let count = 1000 in
         let v =
           Ordered_hash_set_string.of_array
             (Array.init count (fun i -> string_of_int i)) in
         for i = 0 to count - 1 do
           Ordered_hash_set_string.replace v (string_of_int i)
             (string_of_int i ^ Ext_string.single_colon)
         done ;
         OUnit.assert_equal (Ordered_hash_set_string.length v) count ;
         OUnit.assert_equal
           (Ordered_hash_set_string.to_sorted_array v)
           (Array.init count (fun i ->
                string_of_int i ^ Ext_string.single_colon)) ) ]
