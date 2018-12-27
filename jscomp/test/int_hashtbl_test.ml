open Hashtbl

module type S = Hashtbl.S with type key = int

(*
let to_list (module H : S) (tbl : 'a H.t) = 
  H.fold (fun k v acc -> (k,v)::acc) tbl []
*)

let f (module H : Hashtbl.S with type key = int) = 
  (* let module Hashtbl = (val Hashtbl) in *)
  let tbl  = H.create 17 in
  H.add tbl 1 '1';
  H.add tbl 2 '2';
  List.sort (fun ((a : int),_) (b,_) -> compare a b ) 
  @@ H.fold (fun k v acc -> (k,v)::acc) tbl []

let g (module H : S) count = 
  let tbl  = H.create 17 in
  for i  = 0 to count do  
    H.replace tbl (i * 2) (string_of_int i)
  done;
  for i  = 0 to count do  
    H.replace tbl (i * 2) (string_of_int i)
  done;
  let v = H.fold (fun k v acc -> (k,v)::acc) tbl []  in
  let v = List.sort (fun (x, _)  ((y : int), _) -> compare x y)  @@ v in
  Array.of_list v
  
module Int_hash = 
  Hashtbl.Make( struct type t = int 
    let hash x = Hashtbl.hash x
    let equal (x : int) y =  x = y
 end)

let suites = Mt.[

  "simple", (fun _ -> Eq ([1,'1';2,'2'], f (module Int_hash )));
  "more_iterations", 
  (fun _ -> 
    let count = 1000 in
    Eq( Array.init (count + 1) (fun i -> (2 * i, string_of_int i) ), 
        g (module Int_hash) count))
]
    
;; Mt.from_pair_suites __MODULE__ suites
