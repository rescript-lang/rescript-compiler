[@@@bs.config {
  flags = [|
    "-drawlambda"
  |]
}]


(* let empty_backtrace  = Obj.obj (Obj.new_block Obj.abstract_tag 0) *)

let is_block x = (Js.typeof  (Obj.repr x) <> "number")
 
let suites = Mt.[
"is_block_test1", (fun _ -> Eq (false, is_block 3));
"is_block_test2", (fun _ -> Eq (true, is_block [3]));
"is_block_test3", (fun _ -> Eq(true, is_block "x"));
"is_block_test4", (fun _ -> Eq (false, is_block 3.0))
]

;; Mt.from_pair_suites __MODULE__ suites
