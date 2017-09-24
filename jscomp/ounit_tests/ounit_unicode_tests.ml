let ((>::),
     (>:::)) = OUnit.((>::),(>:::))

let (=~) a b = 
    OUnit.assert_equal ~cmp:Ext_string.equal a b 

(** Test for single line *)
let (==~) a b =
  OUnit.assert_equal
    (Ast_utf8_string_interp.transform_test a
     |> List.filter (fun x -> not @@ Ast_utf8_string_interp.empty_segment x)
     |> Ext_list.map (fun 
      ({start = {offset = a}; finish = {offset = b}; kind ; content }
       : Ast_utf8_string_interp.segment) -> 
      a,b,kind,content
      )
    )
    b 

let (==*) a b =
  let segments = 
    Ast_utf8_string_interp.transform_test a
     |> List.filter (fun x -> not @@ Ast_utf8_string_interp.empty_segment x)
     |> Ext_list.map (fun 
      ({start = {lnum=la; offset = a}; finish = {lnum = lb; offset = b}; kind ; content } 
        : Ast_utf8_string_interp.segment) -> 
      la,a,lb,b,kind,content
      )
   in 
   OUnit.assert_equal segments b 
let suites = 
    __FILE__
    >:::
    [
        __LOC__ >:: begin fun _ ->
            Ast_utf8_string.transform_test {|x|} =~ {|x|}
        end;
        __LOC__ >:: begin fun _ ->
            Ast_utf8_string.transform_test "a\nb" =~ {|a\nb|}
        end;
        __LOC__ >:: begin fun _ ->
            Ast_utf8_string.transform_test
            "\\n" =~ "\\n"
        end;
        __LOC__ >:: begin fun _ ->
          Ast_utf8_string.transform_test
            "\\\\\\b\\t\\n\\v\\f\\r\\0\\$" =~
          "\\\\\\b\\t\\n\\v\\f\\r\\0\\$"
        end;

        __LOC__ >:: begin fun _ ->
           match Ast_utf8_string.transform_test
             {|\|} with
           | exception Ast_utf8_string.Error(offset,_) ->
            OUnit.assert_equal offset 1
           | _ -> OUnit.assert_failure __LOC__
        end ;
         __LOC__ >:: begin fun _ ->
           match Ast_utf8_string.transform_test
             {|你\|} with
           | exception Ast_utf8_string.Error(offset,_) ->
            OUnit.assert_equal offset 2
           | _ -> OUnit.assert_failure __LOC__
        end ;
         __LOC__ >:: begin fun _ ->
           match Ast_utf8_string.transform_test
             {|你BuckleScript,好啊\uffff\|} with
           | exception Ast_utf8_string.Error(offset,_) ->
            OUnit.assert_equal offset 23
           | _ -> OUnit.assert_failure __LOC__
        end ;

        __LOC__ >:: begin fun _ ->
          "hie $x hi 你好" ==~
            [
              0,4, String, "hie ";
              4,6, Var, "x";
              6,12,String, " hi 你好"
            ]
        end;
        __LOC__ >:: begin fun _ ->
          "x" ==~
          [0,1, String, "x"]
        end;

        __LOC__ >:: begin fun _ ->
          "" ==~
          []
        end;
        __LOC__ >:: begin fun _ ->
          "你好" ==~
          [0,2,String, "你好"]
        end;
        __LOC__ >:: begin fun _ ->
          "你好$x" ==~
          [0,2,String, "你好";
           2,4,Var, "x";

          ]
        end
        ;
        __LOC__ >:: begin fun _ ->
          "你好$this" ==~
          [
            0,2,String, "你好";
            2,7,Var, "this";
          ]
        end
        ;
        __LOC__ >:: begin fun _ ->
          "你好$(this)" ==~
          [
            0,2,String, "你好";
            2,9,Var, "this"
          ];

          "你好$this)" ==~
          [
             0,2,String, "你好";
             2,7,Var, "this";
             7,8,String,")"
          ];
          {|\xff\xff你好 $x |} ==~
          [
            0,11,String, {|\xff\xff你好 |};
            11,13, Var, "x";
            13,14, String, " "
          ];
          {|\xff\xff你好 $x 不吃亏了buckle $y $z = $sum|}
          ==~
          [(0, 11, String,{|\xff\xff你好 |} );
           (11, 13, Var, "x");
           (13, 25, String,{| 不吃亏了buckle |} );
           (25, 27, Var, "y");
           (27, 28, String, " ");
           (28, 30, Var, "z");
           (30, 33, String, " = ");
           (33, 37, Var, "sum");
           ]
        end
        ;
        __LOC__ >:: begin fun _ ->
          "你好 $(this_is_a_var)  x" ==~
          [
            0,3,String, "你好 ";
            3,19,Var, "this_is_a_var";
            19,22, String, "  x"
          ]
        end
        ;

        __LOC__ >:: begin fun _ ->
        "hi\n$x\n" ==*
        [
          0,0,1,0,String, "hi\\n";
          1,0,1,2,Var, "x" ;
          1,2,2,0,String,"\\n"
        ];
        "$x" ==*
        [0,0,0,2,Var,"x"];
        

        "\n$x\n" ==*
        [
          0,0,1,0,String,"\\n";
          1,0,1,2,Var,"x";
          1,2,2,0,String,"\\n"
        ]
        end;

        __LOC__ >:: begin fun _ -> 
        "\n$(x_this_is_cool) " ==*
        [
          0,0,1,0,String, "\\n";
          1,0,1,17,Var, "x_this_is_cool";
          1,17,1,18,String, " "
        ]
        end;
        __LOC__ >:: begin fun _ -> 
        " $x + $y = $sum " ==*
        [
          0,0,0,1,String , " ";
          0,1,0,3,Var, "x";
          0,3,0,6,String, " + ";
          0,6,0,8,Var, "y";
          0,8,0,11,String, " = ";
          0,11,0,15,Var, "sum";
          0,15,0,16,String, " "
        ]
        end;
        __LOC__ >:: begin fun _ -> 
        "中文 | $a " ==*
        [
          0,0,0,5,String, "中文 | ";
          0,5,0,7,Var, "a";
          0,7,0,8,String, " "
        ]
        end
        ;
        __LOC__ >:: begin fun _ ->
          {|Hello \\$world|} ==*
          [
            0,0,0,8,String,"Hello \\\\";
            0,8,0,14,Var, "world"
          ]
        end
        ;
        __LOC__ >:: begin fun _ -> 
          {|$x)|} ==*
          [
            0,0,0,2,Var,"x";
            0,2,0,3,String,")"
          ]
        end;
        __LOC__ >:: begin fun _ ->
          match Ast_utf8_string_interp.transform_test {j| $( ()) |j}
          with 
          |exception Ast_utf8_string_interp.Error
              ({lnum = 0; offset = 1; byte_bol = 0},
               {lnum = 0; offset = 6; byte_bol = 0}, Invalid_syntax_of_var " (")
            -> OUnit.assert_bool __LOC__ true 
          | _ -> OUnit.assert_bool __LOC__ false 
        end
        ;
        __LOC__ >:: begin fun _ -> 
          match Ast_utf8_string_interp.transform_test {|$()|}
          with 
          | exception Ast_utf8_string_interp.Error ({lnum = 0; offset = 0; byte_bol = 0},
                             {lnum = 0; offset = 3; byte_bol = 0}, Invalid_syntax_of_var "")
            -> OUnit.assert_bool __LOC__ true 
          | _ -> OUnit.assert_bool __LOC__ false
        end
        ;
        __LOC__ >:: begin fun _ ->
          match Ast_utf8_string_interp.transform_test {|$ ()|}
          with 
          | exception Ast_utf8_string_interp.Error 
              ({lnum = 0; offset = 0; byte_bol = 0},
               {lnum = 0; offset = 1; byte_bol = 0}, Invalid_syntax_of_var "")
            -> OUnit.assert_bool __LOC__ true 
          | _ -> OUnit.assert_bool __LOC__ false
        end ;
        __LOC__ >:: begin fun _ -> 
          match Ast_utf8_string_interp.transform_test {|$()|} with 
          | exception Ast_utf8_string_interp.Error 
              ({lnum = 0; offset = 0; byte_bol = 0},
               {lnum = 0; offset = 3; byte_bol = 0}, Invalid_syntax_of_var "")
            -> OUnit.assert_bool __LOC__ true
          | _ -> OUnit.assert_bool __LOC__ false 
        end
        ;
        __LOC__ >:: begin fun _ -> 
          match Ast_utf8_string_interp.transform_test {|$(hello world)|} with 
          | exception Ast_utf8_string_interp.Error 
              ({lnum = 0; offset = 0; byte_bol = 0},
               {lnum = 0; offset = 14; byte_bol = 0}, Invalid_syntax_of_var "hello world")
            -> OUnit.assert_bool __LOC__ true
          | _ -> OUnit.assert_bool __LOC__ false 
        end


        ;
        __LOC__ >:: begin fun _ -> 
          match Ast_utf8_string_interp.transform_test {|$( hi*) |} with 
          | exception Ast_utf8_string_interp.Error 
              ({lnum = 0; offset = 0; byte_bol = 0},
               {lnum = 0; offset = 7; byte_bol = 0}, Invalid_syntax_of_var " hi*")
            -> 
            OUnit.assert_bool __LOC__ true
          | _ -> OUnit.assert_bool __LOC__ false 
        end;
        __LOC__ >:: begin fun _ -> 
          match Ast_utf8_string_interp.transform_test {|xx $|} with 
          | exception Ast_utf8_string_interp.Error 
              ({lnum = 0; offset = 3; byte_bol = 0},
               {lnum = 0; offset = 3; byte_bol = 0}, Unterminated_variable)
            -> 
            OUnit.assert_bool __LOC__ true 
          | _ -> OUnit.assert_bool __LOC__ false
        end ;

        __LOC__ >:: begin fun _ ->
          match Ast_utf8_string_interp.transform_test {|$(world |}; with 
          | exception Ast_utf8_string_interp.Error 
              ({lnum = 0; offset = 0; byte_bol = 0},
               {lnum = 0; offset = 9; byte_bol = 0}, Unmatched_paren)
            -> 
            OUnit.assert_bool __LOC__ true 
          | _ -> OUnit.assert_bool __LOC__ false
        end
    ]
