
type elt = int
type  v = {
  mutable index : int;
  mutable lowlink : int ;
  mutable onstack : bool;
  next : elt list ;    
  data : elt 
}

(** 
   [int] as data for this algorithm
   Pros:
   1. Easy to eoncode algorithm (especially given that the capacity of node is known)
   2. Algorithms itself are much more efficient
   3. Node comparison semantics is clear
   4. Easy to print output
   Cons:
   1. post processing input data  
 *)
let min_int (x : int) y = if x < y then x else y  
let graph vs e =
  let index = ref 0 in 
  let s = Stack.create () in
  let rec scc v  =
    v.index <- !index ; 
    v.lowlink <- !index ; 
    index := !index + 1 ; 
    Stack.push v s; 
    v.onstack <- true;

    v.next 
    |> List.iter (fun w  ->
        let w = Int_map.find  w e in 
        if w.index < 0 then
          begin  
            scc w;
            v.lowlink <- min_int v.lowlink w.lowlink
          end  
        else if w.onstack then 
          v.lowlink <- min_int v.lowlink w.lowlink 
      ) ; 
    if v.lowlink = v.index then
      begin
        print_endline "Cycle" ;  
        let curr = ref (Stack.pop s) in
        (!curr).onstack <- false;
        (* print_endline (!curr).data ; *)    
        while !curr != v do
          curr := Stack.pop s ;
          (!curr).onstack <- false  ;
          (* print_endline (!curr).data ; *)
        done
      end   
  in
  List.iter (fun v -> if v.index < 0 then scc v) vs


(*
let test  (input : (string * string list) list) = 
    let vs = 
        input 
        |> List.map (fun (data,next) -> {index = -1 ; lowlink = -1 ; onstack = false; next ; data }) 
    in
    let e =
        List.fold_left2 (fun acc (x,_) y -> String_map.add x y acc) String_map.empty input vs in 
    graph vs e

let drive () = 
    test [
        "a", ["b" ; "c"];
        "b" , ["c" ; "d"];
        "c", [ "b"];
        "d", [];
    ]              
*)    