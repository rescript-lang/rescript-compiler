



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
  ]
let _ = 
  OUnit.run_test_tt_main suites
