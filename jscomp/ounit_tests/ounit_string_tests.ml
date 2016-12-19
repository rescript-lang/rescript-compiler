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
        end
    ]