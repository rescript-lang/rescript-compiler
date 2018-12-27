

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
let suites = Mt.[
  "simple push", (fun _ ->
    let x = [|3;4;5;2|] in
    Eq(x, queue_1 x))
]

open Mt

;; from_pair_suites __MODULE__ suites
