let ((>::),
    (>:::)) = OUnit.((>::),(>:::))

open Ext_js_regex

let suites =
    __FILE__
    >:::
    [
        "test_empty_string" >:: begin fun _ ->
        let b = js_regex_checker "" in
        OUnit.assert_equal b false
        end;
        "test_normal_regex" >:: begin fun _ ->
        let b = js_regex_checker "/abc/" in
        OUnit.assert_equal b true
        end;
        "test_wrong_regex_last" >:: begin fun _ ->
        let b = js_regex_checker "/abc" in 
        OUnit.assert_equal b false
        end;
        "test_regex_with_flag" >:: begin fun _ ->
        let b = js_regex_checker "/ss/ig" in
        OUnit.assert_equal b true
        end;
        "test_regex_with_invalid_flag" >:: begin fun _ ->
        let b = js_regex_checker "/ss/j" in
        OUnit.assert_equal b false
        end;
        "test_regex_invalid_regex" >:: begin fun _ ->
        let b = js_regex_checker "abc/i" in 
        OUnit.assert_equal b false
        end;
        "test_regex_empty_pattern" >:: begin fun _  ->
        let b = js_regex_checker "//" in 
        OUnit.assert_equal b true
        end;
        "test_regex_with_utf8" >:: begin fun _ ->
        let b = js_regex_checker "/😃/" in
        OUnit.assert_equal b true
        end;
        "test_regex_repeated_flags" >:: begin fun _ ->
        let b = js_regex_checker "/abc/gg" in
        OUnit.assert_equal b false
        end;
    ]