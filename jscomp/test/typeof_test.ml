



let suites = Mt.[
    "int_type", (fun _ -> Eq(Js.typeof 3, "number") );
    "string_type", (fun _ -> Eq(Js.typeof "x", "string"));

    "number_gadt_test", (fun _ -> Eq(Js.Types.test 3 Number, true ))  ;  
    "boolean_gadt_test", (fun _ -> Eq (Js.Types.test Js.true_ Boolean, true ))    ;

    (* assert.notDeepEqual(undefined,null) raises ..*)
    "undefined_gadt_test", (fun _ -> Eq (Js.Types.test Js.undefined Undefined, true ))    ;
    (* "null_gadt_test", (fun _ -> Neq (Js.Types.test Js.null  Js.Null, Js.null )); *)
    (* there ['a Js.null] is one case that the value is already null  '*)

    "string_gadt_test", (fun _ -> Eq (Js.Types.test "3" String, true ));    
    "string_gadt_test_neg", (fun _ -> Eq (Js.Types.test 3 String, false ));    
    "function_gadt_test", (fun _ -> Eq (Js.Types.test (fun  x -> x ) Function,  true)) ;
    "object_gadt_test", (fun _ -> Eq (Js.Types.test [%bs.obj{x = 3}] Object, true ))    
]

;; Mt.from_pair_suites __FILE__ suites 
