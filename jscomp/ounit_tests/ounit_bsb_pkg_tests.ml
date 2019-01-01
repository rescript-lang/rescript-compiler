

let ((>::),
     (>:::)) = OUnit.((>::),(>:::))

let printer_string = fun x -> x 
let (=~) = OUnit.assert_equal  ~printer:printer_string  



let scope_test s (a,b,c)= 
  match Bsb_pkg_types.extract_pkg_name_and_file s with 
  | Scope(a0,b0),c0 -> 
    a =~ a0 ; b =~ b0 ; c =~ c0
  | Global _,_ -> OUnit.assert_failure __LOC__

let global_test s (a,b) = 
  match Bsb_pkg_types.extract_pkg_name_and_file s with 
  | Scope _, _ -> 
    OUnit.assert_failure __LOC__
  | Global a0, b0-> 
    a=~a0; b=~b0

let s_test0 s (a,b)=     
  match Bsb_pkg_types.string_as_package s with 
  | Scope(name,scope) -> 
      a =~ name ; b =~scope 
  | _ -> OUnit.assert_failure __LOC__     

let s_test1 s a =     
  match Bsb_pkg_types.string_as_package s with 
  | Global x  -> 
      a =~ x
  | _ -> OUnit.assert_failure __LOC__       
  
let suites = 
  __FILE__ >::: [
    __LOC__ >:: begin fun _ -> 
      scope_test "@hello/hi"
        ("hi", "@hello","");

      scope_test "@hello/hi/x"
        ("hi", "@hello","x");

      
      scope_test "@hello/hi/x/y"
        ("hi", "@hello","x/y");  
  end ;
  __LOC__ >:: begin fun _ -> 
    global_test "hello"
      ("hello","");
    global_test "hello/x"
      ("hello","x");  
    global_test "hello/x/y"
      ("hello","x/y")    
  end ;
  __LOC__ >:: begin fun _ -> 
    s_test0 "@x/y" ("y","@x");
    s_test0 "@x/y/z" ("y/z","@x");
    s_test1 "xx" "xx";
    s_test1 "xx/yy/zz" "xx/yy/zz"
  end
  ]