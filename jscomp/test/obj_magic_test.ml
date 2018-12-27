


let empty_backtrace  = Obj.obj (Obj.new_block Obj.abstract_tag 0)

let is_block x = (Obj.is_block @@ Obj.repr x)
 
let suites = Mt.[
"is_block_test1", (fun _ -> Eq (false, is_block 3));
"is_block_test2", (fun _ -> Eq (true, is_block [3]));
"is_block_test3", (fun _ -> Eq(true, is_block "x"));
"is_block_test4", (fun _ -> Eq (false, is_block 3.0))
]
(** 

   TODO: Semantics difference 
   {[
   Obj.is_block (Obj.repr "x");;
   true   
   ]}

   {[
     Obj.is_block (Obj.repr 32.0);;
     true
   ]}   
*)

;; Mt.from_pair_suites __MODULE__ suites
