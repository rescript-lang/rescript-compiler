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
  ]