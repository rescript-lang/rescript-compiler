
let split = Ext_string_test.split

let split_by = Ext_string_test.split_by


let suites = 
  Mt.["split" , (fun _ -> 
    Eq (split ~keep_empty:true "hihi" 'i', ["h";"h";""]));
   "split_non_empty", (fun _ -> 
    Eq (split  "hihi" 'i', ["h";"h"]));
   "split_empty", (fun _ -> 
    Eq (split ~keep_empty:true "" 'i', []));
   "split_normal", (fun _ ->
    Eq (split ~keep_empty:true "h i i" ' ', ["h"; "i"; "i"]
                   ));
   "split_by", (fun _ ->
     Eq (List.filter (fun s -> s <> "")
                     (split_by  (fun x -> x = ' ' || x = '\t')
                     "h hgso hgso \t hi" 
                     ), ["h"; "hgso"; "hgso"; "hi"]
               ))
 ]

;; Mt.from_pair_suites __FILE__ suites
