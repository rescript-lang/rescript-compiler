
type  u  = A of int | B of int * bool | C of int 

let suites = Mt.[
    "option", (fun _ -> Eq(true, None < Some 1));
    "option2", (fun _ -> Eq(true, Some 1 < Some 2));
    "list0", (fun _ -> Eq(true, [1] > []));
    "listeq", (fun _ -> Eq(true, [1;2;3] = [1;2;3]));
    "listneq", (fun _ -> Eq(true, [1;2;3] > [1;2;2]));
    "custom_u", (fun _ -> Eq(true, ( A 3  ,  B (2,false) , C 1)  > ( A 3, B (2,false) , C 0 )));
    "custom_u2", (fun _ -> Eq(true, ( A 3  ,  B (2,false) , C 1)  = ( A 3, B (2,false) , C 1 )));
]
;;


Mt.from_pair_suites __FILE__ suites
