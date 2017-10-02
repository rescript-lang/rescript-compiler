let ((>::),
     (>:::)) = OUnit.((>::),(>:::))

let (=~) = OUnit.assert_equal
let suites = 
  __FILE__
  >:::
  [
    __LOC__ >:: begin fun _ -> 
      OUnit.assert_equal
        (Ext_list.flat_map (fun x -> [x;x]) [1;2]) [1;1;2;2] 
    end;
    __LOC__ >:: begin fun _ -> 
      OUnit.assert_equal
        (Ext_list.flat_map_acc (fun x -> [x;x]) [3;4] [1;2]) [1;1;2;2;3;4] 
    end;
    __LOC__ >:: begin fun _ ->
      OUnit.assert_equal (
        Ext_list.flat_map_acc (fun x -> if x mod 2 = 0 then [true] else [])
          [false;false] [1;2]
      )  [true;false;false]
    end;
    __LOC__ >:: begin fun _ -> 
      OUnit.assert_equal (
        Ext_list.map_append  
          (fun x -> string_of_int x) 
          [0;1;2] 
          ["1";"2";"3"]

      )
        ["0";"1";"2"; "1";"2";"3"]
    end;

    __LOC__ >:: begin fun _ -> 
      let (a,b) = Ext_list.take 3 [1;2;3;4;5;6] in 
      OUnit.assert_equal (a,b)
        ([1;2;3],[4;5;6]);
      OUnit.assert_equal (Ext_list.take 1 [1])
        ([1],[])  
    end;

    __LOC__ >:: begin fun _ -> 
      OUnit.assert_equal (Ext_list.assoc_by_int None 1 [2,"x"; 3,"y"; 1, "z"]) "z"
    end;
    __LOC__ >:: begin fun _ -> 
      OUnit.assert_raise_any
        (fun _ -> Ext_list.assoc_by_int None 11 [2,"x"; 3,"y"; 1, "z"])
    end ;
    __LOC__ >:: begin fun _ -> 
      OUnit.assert_equal
        (Ext_list.length_compare [0;0;0] 3) `Eq ;
      OUnit.assert_equal
        (Ext_list.length_compare [0;0;0] 1) `Gt ;   
      OUnit.assert_equal
        (Ext_list.length_compare [0;0;0] 4) `Lt ;   
      OUnit.assert_equal
        (Ext_list.length_compare [] (-1)) `Gt ;   
      OUnit.assert_equal
        (Ext_list.length_compare [] (0)) `Eq ;          
    end;
    __LOC__ >:: begin fun _ -> 
      OUnit.assert_bool __LOC__ 
        (Ext_list.length_larger_than_n 1 [1;2] [1]);
      OUnit.assert_bool __LOC__ 
        (Ext_list.length_larger_than_n 0 [1;2] [1;2]);
      OUnit.assert_bool __LOC__ 
        (Ext_list.length_larger_than_n 2 [1;2] [])

    end;

    __LOC__ >:: begin fun _ ->
      OUnit.assert_bool __LOC__
        (Ext_list.length_ge [1;2;3] 3 );
      OUnit.assert_bool __LOC__
        (Ext_list.length_ge [] 0 );
      OUnit.assert_bool __LOC__
        (not (Ext_list.length_ge [] 1 ));

    end;

    __LOC__ >:: begin fun _ ->
      let (=~) = OUnit.assert_equal ~printer:(
          (
            Format.asprintf "%a"
              (fun fmt (b,l) ->
                 Format.fprintf fmt "(%b,%a)" b 
                   (Format.pp_print_list
                    ~pp_sep:(fun fmt () -> 
                      Format.pp_print_space fmt ()
                    )
                      Format.pp_print_int
                   ) l

              ) 
          ) 

        ) in 

      let f = Ext_list.exclude_with_val  in 
      f  (fun x -> x = 1) [1;2;3] =~ (true,[2;3]);
      f (fun x -> x = 4) [1;2;3] =~ (false, [1;2;3]);
      f (fun x -> x = 2) [1;2;3;2] =~ (true, [1;3]);
      f (fun x -> x = 2) [1;2;2;3;2] =~ (true, [1;3]);
      f (fun x -> x = 2) [2;2;2] =~ (true, []);
      f (fun x -> x = 3) [2;2;2] =~ (false, [2;2;2])
    end ;

  ]