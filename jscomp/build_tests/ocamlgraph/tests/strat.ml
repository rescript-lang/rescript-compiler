(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2007                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* $Id:$ *)

open Graph


module V = struct
  type t = bool * int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end


module G = Persistent.Digraph.Concrete(V)


module P = struct
  type vertex = V.t
  type t = vertex * (vertex list) * (vertex -> bool)

  let get_initial (i, _, _) = i
  let is_final (_, f, _) v = List.mem v f

  let turn (_, _, f) v = f v
end


module S = struct
  type vertex = V.t
  type t = vertex -> vertex

  let empty =
    fun _ -> raise (Invalid_argument "Strategy definition")

  let next f v = f v

  let add s v v' =
    fun e -> if V.equal e v then v' else next s e
end


module A = Strat.Algo(G)(P)(S);;


(* Match game : n matches in line, two players. Each player takes
   one, two or three matches in order. The player who takes the
   last match loses. *)


(* States are given by the remaining number of matches
   and the player whose turn it is to play.
   Edges are the possible moves. *)
let rec trans_aux g (j, n) =
  if n = 0 then g
  else
    if n = 1 then
      let g = G.add_edge g (j, n) (not j, n - 1) in
	(if j then trans_aux g (not j, n) else trans_aux g (not j, n - 1))
    else
      if n = 2 then
	let g = G.add_edge g (j, n) (not j, n - 1) in
	let g = G.add_edge g (j, n) (not j, n - 2) in 
	  (if j then trans_aux g (not j, n) else trans_aux g (not j, n - 1))
      else
	let g = G.add_edge g (j, n) (not j, n - 1) in
	let g = G.add_edge g (j, n) (not j, n - 2) in
	let g = G.add_edge g (j, n) (not j, n - 3) in
	  (if j then trans_aux g (not j, n) else trans_aux g (not j, n - 1));;
let trans n = trans_aux G.empty (true, n);;
let p n = ((true, n), [(true, 0)], fun (b, _) -> b);;


(* ex n = ((true, _), _) if there is a winning strategy for the
   first player to play. In this case, it provides
   a strategy. *)
let ex n =
  let t = trans n in
    (A.strategyA t (p n), t);;

let n1 = 15;;
let n2 = 13;;

let ex1 = ex n1;;
let ex2 = ex n2;;


(* Printing on the standard out channel ;
   simply uncomment to see the results. *)
(*
let rec couple_of_strat g s =
  let f v l =
    try
      let v' = S.next s v in (v, v') :: l
    with Invalid_argument _ -> l
  in
    G.fold_vertex f g [];;

let string_of_couple ((_, i1), (_, i2)) =
  "(" ^ (string_of_int i1) ^ ", " ^ (string_of_int i2) ^ ") ";;

let rec string_of_couple_list l = match l with
    [] -> ""
  | e :: l' -> (string_of_couple e) ^ (string_of_couple_list l');;

print_newline();;
print_string ("For " ^ (string_of_int n1) ^ " matches, is there a winning strategy for the first player ?");;
print_newline();;
print_string (string_of_bool (fst (fst ex1)));;
print_string " --- ";;
print_string (string_of_couple_list (couple_of_strat (snd ex1) (snd (fst ex1))));;
print_newline();; print_newline();;
print_string ("For " ^ (string_of_int n2) ^ " matches, is there a winning strategy for the first player ?");;
print_newline();;
print_string (string_of_bool (fst (fst ex2)));;
print_newline();; print_newline();;
*)
