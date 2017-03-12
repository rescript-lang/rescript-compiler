

(* https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt
*)

let ((>::),
    (>:::)) = OUnit.((>::),(>:::))

let rec print_es6_string_list = function
| [] -> ()
| (Ast_utf8_string.Text s::nl) -> print_string "Text "; print_endline (s^";"); print_es6_string_list nl
| (Delim s::nl) -> print_string "Delim "; print_endline (s^";"); print_es6_string_list nl

let (=~) = OUnit.assert_equal
let suites = 
    __FILE__
    >:::
    [
        __LOC__ >:: begin fun _ -> 
            Ext_utf8.decode_utf8_string
            "hello 你好，中华民族 hei" =~
            [104; 101; 108; 108; 111; 32; 20320; 22909; 65292; 20013; 21326; 27665; 26063; 32; 104; 101; 105]
        end ;
        __LOC__ >:: begin fun _ -> 
            Ext_utf8.decode_utf8_string
            "" =~ []
        end;
        __LOC__ >:: begin fun _ ->
            Ext_string.append "Hell" 'o' =~ "Hello"
        end;
        __LOC__ >:: begin fun _ ->
            Ast_utf8_string.consume_text "Hello $world" 0 =~ ("Hello ", 6)
        end;
        __LOC__ >:: begin fun _ -> 
            let s, new_index = Ast_utf8_string.consume_text "Hello \\$world" 0 in
            let _ = s =~ "Hello \\$world" in
            let _ = new_index =~ String.length "Hello \\$world" in ()
        end;
        __LOC__ >:: begin fun _ ->
            Ast_utf8_string.consume_text "" 0 =~ ("", 0)
        end;
        __LOC__ >:: begin fun _ -> 
            Ast_utf8_string.consume_delim "" 0 =~ (Some "", 0)
        end;
        __LOC__ >:: begin fun _ ->
            Ast_utf8_string.consume_delim "$x" 0 =~ (Some "x", 2)
        end;
        __LOC__ >:: begin fun _ ->
            Ast_utf8_string.consume_delim "$(x)" 0 =~ (Some "x", 4)
        end;
        __LOC__ >:: begin fun _ -> 
            Ast_utf8_string.consume_delim "hello world" 0 =~ (None, 0)
        end;
        __LOC__ >:: begin fun _ -> 
            Ast_utf8_string.consume_delim "$(hello" 0 =~ (None, 7)
        end;
        __LOC__ >:: begin fun _ ->
            Ast_utf8_string.consume_delim "$x)" 0 =~ (None, 3)
        end;
        __LOC__ >:: begin fun _ ->
            Ast_utf8_string.consume_delim "$(hello world)" 0 =~ (None, 8)
        end;
        __LOC__ >:: begin fun _ ->
            let l = Ast_utf8_string.split_es6_string "Hello World" in
            l =~ [Text "Hello World"]
        end;
        __LOC__ >:: begin fun _ ->
            let l = Ast_utf8_string.split_es6_string "Hello $name" in
            l =~ [Text "Hello "; Delim "name"]
        end;
        __LOC__ >:: begin fun _ ->
            let l = Ast_utf8_string.split_es6_string "$x is my name" in
            l =~ [Delim "x"; Text " is my name"]
        end;
        __LOC__ >:: begin fun _ ->
            let l = Ast_utf8_string.split_es6_string "$(country) is beautiful" in
            l =~ [Delim "country"; Text " is beautiful"]
        end;
        __LOC__ >:: begin fun _ ->
            let l = Ast_utf8_string.split_es6_string "hello $x_1, welcome to $(x_2)" in
            l =~ [Text "hello "; Delim "x_1"; Text ", welcome to "; Delim "x_2"]
        end;
    ]