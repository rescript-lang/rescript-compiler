module Test (Queue : module type of Queue) = struct

let to_array q = 
  let v = Array.make (Queue.length q ) 0 in
  ignore @@ Queue.fold (fun i e -> v.(i)<-e; i + 1 ) 0 q ; 
  v
external dump : 'a array -> unit = "" [@@bs.val "console.log"] [@@bs.splice]

let queue_1 x =
  let q = Queue.create () in
  (* dump [|q|]; *)
  Array.iter (fun x -> Queue.add x q) x ;
  (* dump [|q|];  *)
  to_array q 

(** TODO: Note it needs need recursive values support *)
(* let _ = queue_1 [|38|] *)
end 


module T1 = Test(Queue)
module T2 = Test(Queue_402)
open Mt

let suites = Mt.[
  __LOC__, (fun _ ->
    let x = [|3;4;5;2|] in
    Eq(x, T1.queue_1 x));
  __LOC__, (fun _ ->
    let x = [|3;4;5;2|] in
    Eq(x, T2.queue_1 x));  
]

;; from_pair_suites __MODULE__ suites
