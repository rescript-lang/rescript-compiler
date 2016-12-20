let ((>::),
    (>:::)) = OUnit.((>::),(>:::))

let (=~) = OUnit.assert_equal 

let suites = 
  __MODULE__ >:::
  [
    __LOC__ >:: begin fun _ -> 
      [1,"1"; 2,"2"; 12,"12"; 3, "3"]
      |> Int_map.of_list 
      |> Int_map.keys 
      |> OUnit.assert_equal [1;2;3;12]
    end
    ;
    __LOC__ >:: begin fun _ -> 
      OUnit.assert_equal (Int_map.cardinal Int_map.empty) 0 ;
      OUnit.assert_equal ([1,"1"; 2,"2"; 12,"12"; 3, "3"]
      |> Int_map.of_list|>Int_map.cardinal )  4
      
    end;
    __LOC__ >:: begin fun _ ->
      Int_map.cardinal (Int_map.of_array (Array.init 1000 (fun i -> (i,i))))
      =~ 1000
    end;
    __LOC__ >:: begin fun _ -> 
      let count = 1000 in 
      let a = Array.init count (fun x -> x ) in 
      let v = Int_map.empty in
      let u = 
        begin 
          let v = Array.fold_left (fun acc key -> Int_map.adjust key (fun _ -> 1) (succ) acc ) v a   in 
          Array.fold_left (fun acc key -> Int_map.adjust key (fun _ -> 1) (succ) acc ) v a  
          end
        in  
       Int_map.iter (fun _ v -> v =~ 2 ) u   ;
       Int_map.cardinal u =~ count
    end
  ]
