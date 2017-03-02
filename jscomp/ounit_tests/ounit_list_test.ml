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
        Ext_list.map_acc ["1";"2";"3"] (fun x -> string_of_int x) [0;1;2] 

      )
        ["0";"1";"2"; "1";"2";"3"]
    end;

    __LOC__ >:: begin fun _ -> 
      let (a,b) = Ext_list.take 3 [1;2;3;4;5;6] in 
      OUnit.assert_equal (a,b)
        ([1;2;3],[4;5;6])
    end;

    __LOC__ >:: begin fun _ -> 
      OUnit.assert_equal (Ext_list.assoc_by_int None 1 [2,"x"; 3,"y"; 1, "z"]) "z"
    end;
    __LOC__ >:: begin fun _ -> 
      OUnit.assert_raise_any
        (fun _ -> Ext_list.assoc_by_int None 11 [2,"x"; 3,"y"; 1, "z"])
    end 

  ]