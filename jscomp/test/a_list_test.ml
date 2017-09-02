

let suites = Mt.[
  "drop", (fun _ -> 
    Eq (Ext_list_test.drop 3 [0;1;2], []));
  "drop1", (fun _ -> 
    Eq (Ext_list_test.drop 2 [0;1;2], [2]));
  "flat_map", (fun _ -> 
    Eq( [0; 0; 1; 1; 0],
        Ext_list_test.flat_map (fun x -> if x mod 2 ==0 then [0] else [1;1]) [0;0;3;0]
              ))
]

open Mt
;; from_pair_suites __FILE__ suites
