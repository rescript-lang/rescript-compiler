
type  u  = A of int | B of int * bool | C of int 

let function_equal_test = try ((fun x -> x + 1) = (fun x -> x + 2)) with
                         | Invalid_argument "equal: functional value" -> true
                         | _ -> false

let small64 = 44444444444L
let big64 = 0x0800000000000000L

let suites = ref Mt.[
    __LOC__ , (fun _ -> Eq(true, None < Some 1)); 
    "option2", (fun _ -> Eq(true, Some 1 < Some 2));
    __LOC__, (fun _ -> Eq(true, [1] > [])); 
    "listeq", (fun _ -> Eq(true, [1;2;3] = [1;2;3]));
    "listneq", (fun _ -> Eq(true, [1;2;3] > [1;2;2]));
    "custom_u", (fun _ -> Eq(true, ( A 3  ,  B (2,false) , C 1)  > ( A 3, B (2,false) , C 0 )));
    "custom_u2", (fun _ -> Eq(true, ( A 3  ,  B (2,false) , C 1)  = ( A 3, B (2,false) , C 1 )));
    "function", (fun _ -> Eq(true, function_equal_test));
    __LOC__ , begin fun _ -> 
        Eq(true, None < Some 1) 
    end;
    (*JS WAT 
        {[
            0 < [1]
            true 
            0 < [1,30]
            false 
        ]}
    *)
    __LOC__, begin fun _ -> 
        Eq(true, None < Some [|1;30|] )
    end; 
    __LOC__, begin fun _ -> 
        Eq(true,  Some [|1;30|] > None  ) 
    end; 
    __LOC__ , begin fun _ -> 
        Eq(true, [2;6;1;1;2;1;4;2;1] < [2;6;1;1;2;1;4;2;1;409])
    end;
    __LOC__ , begin fun _ -> 
        Eq(true, [1] < [1;409])
    end;
    __LOC__ , begin fun _ -> 
        Eq(true, [] < [409]) 
    end;
    __LOC__ , begin fun _ -> 
        Eq(true,  [2;6;1;1;2;1;4;2;1;409] > [2;6;1;1;2;1;4;2;1])
    end;
    
    __LOC__, begin fun _ -> 
        Eq(false, None = Some [|1;30|] )
    end; 
    __LOC__, begin fun _ -> 
        Eq(false,  Some [|1;30|] = None  )
    end; 
    __LOC__ , begin fun _ -> 
        Eq(false, [2;6;1;1;2;1;4;2;1] = [2;6;1;1;2;1;4;2;1;409])
    end;
    __LOC__ , begin fun _ -> 
        Eq(false,  [2;6;1;1;2;1;4;2;1;409] = [2;6;1;1;2;1;4;2;1])
    end;

    "cmp_id", (fun _ -> Eq (compare [%bs.obj {x=1; y=2}] [%bs.obj {x=1; y=2}], 0));
    "cmp_val", (fun _ -> Eq (compare [%bs.obj {x=1}] [%bs.obj {x=2}], -1));
    "cmp_val2", (fun _ -> Eq (compare [%bs.obj {x=2}] [%bs.obj {x=1}], 1));
    "cmp_empty", (fun _ -> Eq (compare [%bs.raw "{}"] [%bs.raw "{}"], 0));
    "cmp_empty2", (fun _ -> Eq (compare [%bs.raw "{}"] [%bs.raw "{x:1}"], -1));
    "cmp_swap", (fun _ -> Eq (compare [%bs.obj {x=1; y=2}] [%bs.obj {y=2; x=1}], 0));
    "cmp_size", (fun _ -> Eq (compare [%bs.raw "{x:1}"] [%bs.raw "{x:1, y:2}"], -1));
    "cmp_size2", (fun _ -> Eq (compare [%bs.raw "{x:1, y:2}"] [%bs.raw "{x:1}"], 1));
    "cmp_order", (fun _ -> Eq (compare [%bs.obj {x=0; y=1}] [%bs.obj {x=1; y=0}], -1));
    "cmp_order2", (fun _ -> Eq (compare [%bs.obj {x=1; y=0}] [%bs.obj {x=0; y=1}], 1));
    "cmp_in_list", (fun _ -> Eq (compare [[%bs.obj {x=1}]] [[%bs.obj {x=2}]], -1));
    "cmp_in_list2", (fun _ -> Eq (compare [[%bs.obj {x=2}]] [[%bs.obj {x=1}]], 1));
    "cmp_with_list", (fun _ -> Eq (compare [%bs.obj {x=[0]}] [%bs.obj {x=[1]}], -1));
    "cmp_with_list2", (fun _ -> Eq (compare [%bs.obj {x=[1]}] [%bs.obj {x=[0]}], 1));
    "eq_id", (fun _ -> Ok ([%bs.obj {x=1; y=2}] = [%bs.obj {x=1; y=2}]));
    "eq_val", (fun _ -> Eq ([%bs.obj {x=1}] = [%bs.obj {x=2}], false));
    "eq_val2", (fun _ -> Eq ([%bs.obj {x=2}] = [%bs.obj {x=1}], false));
    "eq_empty", (fun _ -> Eq ([%bs.raw "{}"] = [%bs.raw "{}"], true));
    "eq_empty2", (fun _ -> Eq ([%bs.raw "{}"] = [%bs.raw "{x:1}"], false));
    "eq_swap", (fun _ -> Ok ([%bs.obj {x=1; y=2}] = [%bs.obj {y=2; x=1}]));
    "eq_size", (fun _ -> Eq ([%bs.raw "{x:1}"] = [%bs.raw "{x:1, y:2}"], false));
    "eq_size2", (fun _ -> Eq ([%bs.raw "{x:1, y:2}"] = [%bs.raw "{x:1}"], false));
    "eq_in_list", (fun _ -> Eq ([[%bs.obj {x=1}]] = [[%bs.obj {x=2}]], false));
    "eq_in_list2", (fun _ -> Eq ([[%bs.obj {x=2}]] = [[%bs.obj {x=2}]], true));
    "eq_with_list", (fun _ -> Eq ([%bs.obj {x=[0]}] = [%bs.obj {x=[0]}], true));
    "eq_with_list2", (fun _ -> Eq ([%bs.obj {x=[0]}] = [%bs.obj {x=[1]}], false));

    __LOC__ , begin fun _ ->
        Eq(compare Js.null (Js.Null.return [3]), -1)
    end;
    __LOC__ , begin fun _ ->
        Eq(compare (Js.Null.return [3]) Js.null, 1)
    end;
    __LOC__ , begin fun _ ->
        Eq(compare Js.null (Js.Null.return 0), -1)
    end;
    __LOC__ , begin fun _ ->
        Eq(compare (Js.Null.return 0) Js.null, 1)
    end;
    __LOC__ , begin fun _ ->
        Eq(compare Js.Nullable.undefined (Js.Nullable.return 0), -1)
    end;
    __LOC__ , begin fun _ ->
        Eq(compare (Js.Nullable.return 0) Js.Nullable.undefined, 1)
    end;

    "cmp_int64a", (fun _ -> Eq(compare 1L 44444444444L, -1));
    "cmp_int64b", (fun _ -> Eq(compare 44444444444L 1L, 1));
    "cmp_int64c", (fun _ -> Eq(compare small64 big64, -1));
    "cmp_int64d", (fun _ -> Eq(compare big64 small64, 1));
    ]
;;


let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 

;; eq __LOC__ true (Some 1 > None)
;; eq __LOC__ true ([] < [1])
;; eq __LOC__ false (None > Some 1)
;; eq __LOC__ false (None > Some [|1;30|])
;; eq __LOC__ false (Some [|1;30|] < None)

let () = Mt.from_pair_suites __MODULE__ !suites
