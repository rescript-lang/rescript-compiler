let ((>::),
    (>:::)) = OUnit.((>::),(>:::))

let (=~) = OUnit.assert_equal

let printer_int_array = fun xs -> 
    String.concat ","
    (List.map string_of_int @@ Array.to_list xs )

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
        let (=~) = OUnit.assert_equal ~printer:printer_int_array in 
        let k = Ext_array.of_list_map in 
        k succ [] =~ [||];
        k succ [1]  =~ [|2|];
        k succ [1;2;3]  =~ [|2;3;4|];
        k succ [1;2;3;4]  =~ [|2;3;4;5|];
        k succ [1;2;3;4;5]  =~ [|2;3;4;5;6|];
        k succ [1;2;3;4;5;6]  =~ [|2;3;4;5;6;7|];
        k succ [1;2;3;4;5;6;7]  =~ [|2;3;4;5;6;7;8|];
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