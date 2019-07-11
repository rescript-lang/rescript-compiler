let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))
let ( =~ ) = OUnit.assert_equal

let test_sorted_strict arr =
  let v = Int_map.of_array arr |> Int_map.to_sorted_array in
  let arr_copy = Array.copy arr in
  Array.sort (fun ((a : int), _) (b, _) -> compare a b) arr_copy ;
  v =~ arr_copy

let suites =
  __MODULE__
  >::: [ ( __LOC__
         >:: fun _ ->
         [(1, "1"); (2, "2"); (12, "12"); (3, "3")]
         |> Int_map.of_list |> Int_map.keys
         |> OUnit.assert_equal [1; 2; 3; 12] )
       ; ( __LOC__
         >:: fun _ ->
         OUnit.assert_equal (Int_map.cardinal Int_map.empty) 0 ;
         OUnit.assert_equal
           ( [(1, "1"); (2, "2"); (12, "12"); (3, "3")]
           |> Int_map.of_list |> Int_map.cardinal )
           4 )
       ; ( __LOC__
         >:: fun _ ->
         let v =
           [(1, "1"); (2, "2"); (12, "12"); (3, "3")]
           |> Int_map.of_list |> Int_map.to_sorted_array in
         Array.length v =~ 4 ;
         v =~ [|(1, "1"); (2, "2"); (3, "3"); (12, "12")|] )
       ; ( __LOC__
         >:: fun _ ->
         test_sorted_strict [||] ;
         test_sorted_strict [|(1, "")|] ;
         test_sorted_strict [|(2, ""); (1, "")|] ;
         test_sorted_strict [|(2, ""); (1, ""); (3, "")|] ;
         test_sorted_strict [|(2, ""); (1, ""); (3, ""); (4, "")|] )
       ; ( __LOC__
         >:: fun _ ->
         Int_map.cardinal
           (Int_map.of_array (Array.init 1000 (fun i -> (i, i))))
         =~ 1000 )
       ; ( __LOC__
         >:: fun _ ->
         let count = 1000 in
         let a = Array.init count (fun x -> x) in
         let v = Int_map.empty in
         let u =
           let v =
             Array.fold_left
               (fun acc key ->
                 Int_map.adjust acc key (fun v ->
                     match v with None -> 1 | Some v -> succ v))
               v a in
           Array.fold_left
             (fun acc key ->
               Int_map.adjust acc key (fun v ->
                   match v with None -> 1 | Some v -> succ v))
             v a in
         Int_map.iter u (fun _ v -> v =~ 2) ;
         Int_map.cardinal u =~ count ) ]
