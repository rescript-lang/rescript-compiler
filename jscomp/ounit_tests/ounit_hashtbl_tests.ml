let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))
let ( =~ ) = OUnit.assert_equal

let suites =
  __FILE__
  >::: [ (* __LOC__ >:: begin fun _ -> *)
         (* let h = String_hashtbl.create 0 in *)
         (* let accu key = *)
         (* String_hashtbl.replace_or_init h key succ 1 in *)
         (* let count = 1000 in *)
         (* for i = 0 to count - 1 do *)
         (* Array.iter accu [|"a";"b";"c";"d";"e";"f"|] *)
         (* done; *)
         (* String_hashtbl.length h =~ 6; *)
         (* String_hashtbl.iter (fun _ v -> v =~ count ) h *)
         (* end; *)
         ( "add semantics "
         >:: fun _ ->
         let h = String_hashtbl.create 0 in
         let count = 1000 in
         for j = 0 to 1 do
           for i = 0 to count - 1 do
             String_hashtbl.add h (string_of_int i) i
           done
         done ;
         String_hashtbl.length h =~ 2 * count )
       ; ( "replace semantics"
         >:: fun _ ->
         let h = String_hashtbl.create 0 in
         let count = 1000 in
         for j = 0 to 1 do
           for i = 0 to count - 1 do
             String_hashtbl.replace h (string_of_int i) i
           done
         done ;
         String_hashtbl.length h =~ count ) ]
