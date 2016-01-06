open Mt 
let split = Ext_string.split

let split_by = Ext_string.split_by

let suites = 
  ["split" , (fun _ -> 
    assert_equal (split ~keep_empty:true "hihi" 'i') ["h";"h";""]);
   "split_non_empty", (fun _ -> 
    assert_equal (split  "hihi" 'i') ["h";"h"]);
   "splitempty", (fun _ -> 
    assert_equal (split ~keep_empty:true "" 'i') [""]);
   "split_normal", (fun _ ->
    assert_equal (split ~keep_empty:true "h i i" ' ') ["h"; "i"; "i"]
                   );
   "split_by", (fun _ ->
     assert_equal (List.filter (fun s -> s <> "")
                     (split_by  (fun x -> x = ' ' || x = '\t')
                     "h hgso hgso \t hi" 
                     )) ["h"; "hgso"; "hgso"; "hi"]
               )
 ]

;; from_suites __FILE__ suites
