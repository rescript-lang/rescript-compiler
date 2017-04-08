let ((>::),
     (>:::)) = OUnit.((>::),(>:::))

let (=~) a b = 
    OUnit.assert_equal ~cmp:Ext_string.equal a b 

let suites = 
    __FILE__
    >:::
    [
        __LOC__ >:: begin fun _ ->
            Ast_utf8_string.transform Location.none {|x|} =~ {|x|}
        end;
        __LOC__ >:: begin fun _ ->
            Ast_utf8_string.transform Location.none "a\nb" =~ {|a\nb|}
        end
    ]