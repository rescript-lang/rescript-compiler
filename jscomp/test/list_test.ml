open Mt

let list_suites = [
 "length" , (fun _ -> 
   assert (1 = List.length [0,1,2,3,4]) (* This is tuple haha*)
            );
 "length2" , (fun _ -> 
   assert (5 = List.length [0;1;2;3;4]) (* This is tuple haha*)
            )    ;
  "long_length", (fun _ -> 
    let v = 100000 in
    assert_equal v 
      (List.length 
         (Array.to_list (Array.init v (fun _ -> 0))))
                 );
  "sort", (fun _ -> 
    assert_equal (List.sort (fun (x : int) y -> Pervasives.compare x y) [4;1;2;3]) 
      [1;2;3;4]
          )
]
  
let () = 
  begin
    from_suites "List_suites" list_suites
  end
