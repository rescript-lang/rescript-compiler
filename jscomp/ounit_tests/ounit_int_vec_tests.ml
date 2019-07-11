let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))
let ( =~ ) = OUnit.assert_equal

let suites =
  __FILE__
  >::: [ ( __LOC__
         >:: fun _ ->
         OUnit.assert_bool __LOC__
           (Int_vec_util.mem 3 (Int_vec.of_list [1; 2; 3])) ;
         OUnit.assert_bool __LOC__
           (not @@ Int_vec_util.mem 0 (Int_vec.of_list [1; 2])) ;
         let v = Int_vec.make 100 in
         OUnit.assert_bool __LOC__ (not @@ Int_vec_util.mem 0 v) ;
         Int_vec.push v 0 ;
         OUnit.assert_bool __LOC__ (Int_vec_util.mem 0 v) )
       ; ( __LOC__
         >:: fun _ ->
         let u = Int_vec.make 100 in
         Int_vec.push u 1 ;
         OUnit.assert_bool __LOC__ (not @@ Int_vec_util.mem 0 u) ;
         Int_vec.push u 0 ;
         OUnit.assert_bool __LOC__ (Int_vec_util.mem 0 u) ) ]
