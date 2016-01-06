open Hashtbl


let to_list tbl = fold (fun k v acc -> (k,v)::acc) tbl []


let f () = 
  let tbl  = Hashtbl.create 17 in
  add tbl 1 '1';
  add tbl 2 '2';
  List.sort (fun ((a : int),_) (b,_) -> compare a b ) @@  to_list tbl 

let g count = 
  let tbl  = Hashtbl.create 17 in
  for i  = 0 to count do  
    replace tbl (i * 2) (string_of_int i)
  done;
  for i  = 0 to count do  
    replace tbl (i * 2) (string_of_int i)
  done;
  let v = to_list tbl  in
  let v = List.sort (fun (x, _)  ((y : int), _) -> compare x y)  @@ v in
  Array.of_list v
  

let suites = Mt.[
  "simple", (fun _ -> Eq ([1,'1';2,'2'], f ()));
  "more_iterations", 
  (fun _ -> 
    let count = 1000 in
    Eq( Array.init (count + 1) (fun i -> (2 * i, string_of_int i) ), g count))
]
    
;; Mt.from_pair_suites __FILE__ suites
