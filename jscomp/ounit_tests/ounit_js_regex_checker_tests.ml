let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

open Ext_js_regex

let suites =
  __FILE__
  >::: [ ( "test_empty_string"
         >:: fun _ ->
         let b = js_regex_checker "" in
         OUnit.assert_equal b false )
       ; ( "test_normal_regex"
         >:: fun _ ->
         let b = js_regex_checker "/abc/" in
         OUnit.assert_equal b true )
       ; ( "test_wrong_regex_last"
         >:: fun _ ->
         let b = js_regex_checker "/abc" in
         OUnit.assert_equal b false )
       ; ( "test_regex_with_flag"
         >:: fun _ ->
         let b = js_regex_checker "/ss/ig" in
         OUnit.assert_equal b true )
       ; ( "test_regex_with_invalid_flag"
         >:: fun _ ->
         let b = js_regex_checker "/ss/j" in
         OUnit.assert_equal b false )
       ; ( "test_regex_invalid_regex"
         >:: fun _ ->
         let b = js_regex_checker "abc/i" in
         OUnit.assert_equal b false )
       ; ( "test_regex_empty_pattern"
         >:: fun _ ->
         let b = js_regex_checker "//" in
         OUnit.assert_equal b true )
       ; ( "test_regex_with_utf8"
         >:: fun _ ->
         let b = js_regex_checker "/😃/" in
         OUnit.assert_equal b true )
       ; ( "test_regex_repeated_flags"
         >:: fun _ ->
         let b = js_regex_checker "/abc/gg" in
         OUnit.assert_equal b false ) ]
