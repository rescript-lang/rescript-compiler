



let suites = Mt.[
    "int_type", (fun _ -> Eq(Js.typeof 3, "number") );
    "string_type", (fun _ -> Eq(Js.typeof "x", "string"));

    "number_gadt_test", (fun _ -> Neq (Js.Types.test 3 Number, Js.null ))  ;  
    "boolean_gadt_test", (fun _ -> Neq (Js.Types.test Js.true_ Boolean, Js.null ))    ;

    (* assert.notDeepEqual(undefined,null) raises ..*)
    "undefined_gadt_test", (fun _ -> Eq (Js.Types.test Js.undefined Undefined != Js.null, true ))    ;
    (* "null_gadt_test", (fun _ -> Neq (Js.Types.test Js.null  Js.Null, Js.null )); *)
    (* there ['a Js.null] is one case that the value is already null  '*)

    "string_gadt_test", (fun _ -> Neq (Js.Types.test "3" String, Js.null ));    
    "string_gadt_test_neg", (fun _ -> Eq (Js.Types.test 3 String, Js.null ));    
    "function_gadt_test", (fun _ -> Neq (Js.Types.test (fun  x -> x ) Function, Js.null )) ;
    "object_gadt_test", (fun _ -> Neq (Js.Types.test [%bs.obj{x = 3}] Object, Js.null ))    
]

;; Mt.from_pair_suites __FILE__ suites 
