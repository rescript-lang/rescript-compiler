



module Int_array = Resize_array.Make(struct type t = int let null = 0 end);;
let v = Int_array.init 10 (fun i -> i);;

let ((>::),
    (>:::)) = OUnit.((>::),(>:::))


let (=~) x y = OUnit.assert_equal ~cmp:(Int_array.equal  (fun (x: int) y -> x=y)) x y
let (=~~) x y 
  = 
  OUnit.assert_equal ~cmp:(Int_array.equal  (fun (x: int) y -> x=y)) x (Int_array.of_array y) 

let suites = 
  __FILE__ >:::
  [
    Ounit_vec_test.suites;
    Ounit_json_tests.suites;
    Ounit_path_tests.suites;
    Ounit_array_tests.suites;    
    Ounit_scc_tests.suites;
    Ounit_list_test.suites;
    Ounit_hash_set_tests.suites;
    Ounit_union_find_tests.suites;
    Ounit_bal_tree_tests.suites;
    Ounit_hash_stubs_test.suites;
    Ounit_map_tests.suites;
    Ounit_ordered_hash_set_tests.suites;
    Ounit_hashtbl_tests.suites;
    Ounit_string_tests.suites;
    Ounit_topsort_tests.suites;
    (* Ounit_sexp_tests.suites; *)
    Ounit_int_vec_tests.suites;
    Ounit_ident_mask_tests.suites;
    Ounit_cmd_tests.suites;
    Ounit_ffi_error_debug_test.suites;
    Ounit_js_regex_checker_tests.suites;
    Ounit_utf8_test.suites;
    Ounit_unicode_tests.suites;
    Ounit_bsb_regex_tests.suites;
    Ounit_bsb_pkg_tests.suites
  ]
let _ = 
  OUnit.run_test_tt_main suites
