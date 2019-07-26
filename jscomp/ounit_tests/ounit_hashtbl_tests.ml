let ((>::),
     (>:::)) = OUnit.((>::),(>:::))

let (=~) = OUnit.assert_equal ~printer:Ext_obj.dump


let suites = 
  __FILE__
  >:::[
    (* __LOC__ >:: begin fun _ ->  *)
    (*   let h = String_hashtbl.create 0 in  *)
    (*   let accu key = *)
    (*     String_hashtbl.replace_or_init h key   succ 1 in  *)
    (*   let count = 1000 in  *)
    (*   for i = 0 to count - 1 do      *)
    (*     Array.iter accu  [|"a";"b";"c";"d";"e";"f"|]     *)
    (*   done; *)
    (*   String_hashtbl.length h =~ 6; *)
    (*   String_hashtbl.iter (fun _ v -> v =~ count ) h *)
    (* end; *)

    "add semantics " >:: begin fun _ -> 
      let h = String_hashtbl.create 0 in 
      let count = 1000 in 
      for j = 0 to 1 do  
        for i = 0 to count - 1 do                 
          String_hashtbl.add h (string_of_int i) i 
        done
      done ;
      String_hashtbl.length h =~ 2 * count 
    end; 
    "replace semantics" >:: begin fun _ -> 
      let h = String_hashtbl.create 0 in 
      let count = 1000 in 
      for j = 0 to 1 do  
        for i = 0 to count - 1 do                 
          String_hashtbl.replace h (string_of_int i) i 
        done
      done ;
      String_hashtbl.length h =~  count 
    end; 
    
    __LOC__ >:: begin fun _ ->
      let h = String_hashtbl.create 0 in 
      let count = 10 in 
      for i = 0 to count - 1 do 
        String_hashtbl.replace h (string_of_int i) i
      done; 
      let xs = String_hashtbl.to_list h (fun k _ -> k) in 
      let ys = List.sort compare xs  in 
      ys =~ ["0";"1";"2";"3";"4";"5";"6";"7";"8";"9"]
    end
  ]
