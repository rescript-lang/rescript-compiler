(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)
 
type elt = int


type  node = {
  mutable index : int;
  mutable lowlink : int ;
  (* mutable onstack : bool; *)
  data : elt ;
  next : Int_vec.t ;    

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

let graph  e =
  let index = ref 0 in 
  let s = Stack.create () in

  (* collect output *)
  let output = Queue.create () in 
  let node_numes = Array.length e in 
  let on_stack_array = Array.make node_numes false in
  let rec scc v  =
    let new_index = !index + 1 in 
    index := new_index ;
    Stack.push v.data s; 
    v.index <- new_index ;
    v.lowlink <- new_index ;
    on_stack_array. (v.data) <- true ; 
    (* v.onstack <- true; *)

    v.next 
    |> Int_vec.iter (fun w  ->
        let w = e.(w) in 
        if w.index < 0 then
          begin  
            scc w;
            v.lowlink <- min_int v.lowlink w.lowlink
          end  
        else if (* w.onstack *) on_stack_array.(w.data) then 
          v.lowlink <- min_int v.lowlink w.lowlink 
      ) ; 
    if v.lowlink = v.index then
      begin
        (* TODO: if we use stack as vector we can do batch update here *)
        let curr_vec = Int_vec.make 4 in 

        let curr_ele = Stack.pop s in
        let curr = ref curr_ele in

        on_stack_array.(curr_ele) <- false;
        Int_vec.push curr_vec  curr_ele; 

        while !curr != v.data do
          let curr_ele = Stack.pop s in
          curr :=  curr_ele ;

          on_stack_array.(curr_ele) <- false ; 
          Int_vec.push curr_vec curr_ele
        done;
        Queue.push curr_vec  output
      end   
  in

  Array.iter (fun v -> if v.index < 0 then scc v) e;
  output 

let graph_check v = 
  let v = graph v in 
  Queue.length v, 
  Queue.fold (fun acc x -> Int_vec.length x :: acc ) [] v  
