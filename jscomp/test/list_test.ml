

let list_suites = Mt.[
 "length" , (fun _ -> 
   Eq (1 , List.length [0,1,2,3,4]) (* This is tuple haha*)
            );
 "length2" , (fun _ -> 
   Eq (5 , List.length [0;1;2;3;4]) (* This is tuple haha*)
            )    ;
  "long_length", (fun _ -> 
    let v = 30_000 in
    Eq (v ,
      (List.length 
         (Array.to_list (Array.init v (fun _ -> 0))))
                 ));
  "sort", (fun _ -> 
    Eq ((List.sort (fun (x : int) y -> Pervasives.compare x y) [4;1;2;3]), 
      [1;2;3;4])
          );
  __LOC__, (fun _ -> 
    Eq (true, List.mem 3 [1;2;3])
  )        ;
  __LOC__, (fun _ -> 
    Eq (false, List.mem 4 [1;2;3])
  );        
  __LOC__, (fun _ -> 
    Eq (9, List.assoc 4 [(1,2);(4,9)])
  );
]
  
let () = 
  begin
    Mt.from_pair_suites __MODULE__ list_suites
  end
