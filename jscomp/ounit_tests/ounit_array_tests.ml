let ((>::),
    (>:::)) = OUnit.((>::),(>:::))

let (=~) = OUnit.assert_equal
let suites = 
    __FILE__
    >:::
    [
     __LOC__ >:: begin fun _ ->
        Ext_array.find_and_split 
            [|"a"; "b";"c"|]
            Ext_string.equal "--" =~ `No_split
     end;
    __LOC__ >:: begin fun _ ->
        Ext_array.find_and_split 
            [|"a"; "b";"c";"--"|]
            Ext_string.equal "--" =~ `Split ([|"a";"b";"c"|],[||])
     end;
     __LOC__ >:: begin fun _ ->
        Ext_array.find_and_split 
            [|"--"; "a"; "b";"c";"--"|]
            Ext_string.equal "--" =~ `Split ([||], [|"a";"b";"c";"--"|])
     end;
    __LOC__ >:: begin fun _ ->
        Ext_array.find_and_split 
            [| "u"; "g"; "--"; "a"; "b";"c";"--"|]
            Ext_string.equal "--" =~ `Split ([|"u";"g"|], [|"a";"b";"c";"--"|])
     end;
    __LOC__ >:: begin fun _ ->
        Ext_array.reverse [|1;2|] =~ [|2;1|];
        Ext_array.reverse [||] =~ [||]  
    end     ;
    __LOC__ >:: begin fun _ -> 
        Ext_array.of_list_map succ [] =~ [||];
        Ext_array.of_list_map succ [1]  =~ [|2|];
        Ext_array.of_list_map succ [1;2;3]  =~ [|2;3;4|];
    end; 
    __LOC__ >:: begin fun _ -> 
        Ext_array.to_list_map_acc
        (fun x -> if x mod 2 = 0 then Some x else None )
        [|1;2;3;4;5;6|] [1;2;3]
        =~ [2;4;6;1;2;3]
    end;
    __LOC__ >:: begin fun _ -> 
        Ext_array.to_list_map_acc
        (fun x -> if x mod 2 = 0 then Some x else None )
        [|1;2;3;4;5;6|] []
        =~ [2;4;6]
    end;

    __LOC__ >:: begin fun _ -> 
    OUnit.assert_bool __LOC__ 
        (Ext_array.for_all2_no_exn
        (=)
        [|1;2;3|]
        [|1;2;3|]
        )
    end;
    __LOC__ >:: begin fun _ -> 
    OUnit.assert_bool __LOC__
    (Ext_array.for_all2_no_exn
    (=) [||] [||]
    );
    OUnit.assert_bool __LOC__
    (not @@ Ext_array.for_all2_no_exn
    (=) [||] [|1|]
    )
    end
    ;
    __LOC__ >:: begin fun _ -> 
    OUnit.assert_bool __LOC__
    (not (Ext_array.for_all2_no_exn
        (=)
        [|1;2;3|]
        [|1;2;33|]
        ))
    end
    ]