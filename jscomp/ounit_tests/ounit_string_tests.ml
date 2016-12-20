let ((>::),
    (>:::)) = OUnit.((>::),(>:::))

let (=~) = OUnit.assert_equal    




let suites = 
    __FILE__ >::: 
    [
        __LOC__ >:: begin fun _ ->
            OUnit.assert_bool "not found " (Ext_string.rindex_neg "hello" 'x' < 0 )
        end;

        __LOC__ >:: begin fun _ -> 
            Ext_string.rindex_neg "hello" 'h' =~ 0 ;
            Ext_string.rindex_neg "hello" 'e' =~ 1 ;
            Ext_string.rindex_neg "hello" 'l' =~ 3 ;
            Ext_string.rindex_neg "hello" 'l' =~ 3 ;
            Ext_string.rindex_neg "hello" 'o' =~ 4 ;
        end;

        __LOC__ >:: begin fun _ -> 
            OUnit.assert_bool "empty string" (Ext_string.rindex_neg "" 'x' < 0 )
        end;

        __LOC__ >:: begin fun _ -> 
            OUnit.assert_bool __LOC__
            (Ext_string.for_all_range "xABc"~start:1
            ~finish:3 (function 'A' .. 'Z' -> true | _ -> false));
            OUnit.assert_bool __LOC__
            (not (Ext_string.for_all_range "xABc"~start:1
            ~finish:4 (function 'A' .. 'Z' -> true | _ -> false)));
            OUnit.assert_bool __LOC__
            ( (Ext_string.for_all_range "xABc"~start:1
            ~finish:2 (function 'A' .. 'Z' -> true | _ -> false)));
            OUnit.assert_bool __LOC__
            ( (Ext_string.for_all_range "xABc"~start:1
            ~finish:1 (function 'A' .. 'Z' -> true | _ -> false)));
            OUnit.assert_bool __LOC__
            ( (Ext_string.for_all_range "xABc"~start:1
            ~finish:0 (function 'A' .. 'Z' -> true | _ -> false)));
        end;

        __LOC__ >:: begin fun _ -> 
            OUnit.assert_bool __LOC__ @@
             List.for_all Ext_string.is_valid_source_name
            ["x.ml"; "x.mli"; "x.re"; "x.rei"; "x.mll"; 
            "A_x.ml"; "ab.ml"; "a_.ml"; "a__.ml";
            "ax.ml"];
            OUnit.assert_bool __LOC__ @@ not @@
                List.exists Ext_string.is_valid_source_name
                [".re"; ".rei";"..re"; "..rei"; "..ml"; ".mll~"; 
                "...ml"; "_.mli"; "_x.ml"; "__.ml"; "__.rei"; 
                ".#hello.ml"; ".#hello.rei"
                ]
        end
    ]