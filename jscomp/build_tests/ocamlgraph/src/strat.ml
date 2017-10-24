(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2010                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* Signature for graphs *)
module type G = sig

  type t

  module V : Sig.ORDERED_TYPE

  type vertex = V.t

  val mem_vertex : t -> vertex -> bool

  val succ : t -> vertex -> vertex list

  val fold_vertex : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_succ : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
end


(* Signature for graph add-ons: an initial vertex, final vertices
   and membership of vertices to either true or false,
   i.e. first or second player *)
module type PLAYER = sig

  type t
  type vertex

  val get_initial : t -> vertex
  val is_final : t -> vertex -> bool

  val turn : t -> vertex -> bool

end


(* Signature for strategies : for a given state, the strategy tells
   which state to go to *)
module type STRAT = sig

  type t
  type vertex

  val empty : t
  val add : t -> vertex -> vertex -> t

  val next : t -> vertex -> vertex
  (* Raises Invalid_argument if vertex's image is not defined *)

end


(* Implements strategy algorithms on graphs *)
module Algo (G : G) (P : PLAYER with type vertex = G.vertex)
    (S : STRAT with type vertex = G.vertex) :
sig

  (* coherent_player g p returns true iff
     the completion p is coherent w.r.t.
     the graph g *)
  val coherent_player : G.t -> P.t -> bool

  (* coherent_strat g s returns true iff
     the strategy s is coherent w.r.t.
     the graph g *)
  val coherent_strat : G.t -> S.t -> bool

  (* game g p a b returns true iff a wins in g
     given the completion p (i.e. the game
     goes through a final state). *)
  val game : G.t -> P.t -> S.t -> S.t -> bool

  (* strategy g p s returns true iff s wins in g
     given the completion p, whatever strategy
     plays the other player. *)
  val strategy : G.t -> P.t -> S.t -> bool

  (* strategyA g p returns true iff there
     exists a winning stragegy for the true
     player. In this case, the winning
     strategy is provided. *)
  val strategyA : G.t -> P.t -> (bool * S.t)
end = struct

  module SetV = Set.Make (G.V)


  let rec eq l1 l2 = match l1, l2 with
      [], [] -> true
    | e1 :: l1', e2 :: l2' ->
      (e1 = e2) && (eq l1' l2')
    | _ -> false

  let rec eq_mem i l1 l2 = match l1, l2 with
      [], [] -> (true, false)
    | e1 :: l1', e2 :: l2' ->
      if e1 = e2 then
        if e1 = i then (eq l1' l2', true)
        else eq_mem i l1' l2'
      else (false, false)
    | _ -> (false, false)

  let puit g v = match G.succ g v with
      [] -> true
    | _ -> false


  let get_finals g p =
    let f a l =
      if P.is_final p a then a :: l
      else l
    in G.fold_vertex f g []


  let coherent_player g p =
    G.mem_vertex g (P.get_initial p)


  let coherent_strat g s =
    let f v b =
      try
        let v' = S.next s v in
        b && (G.mem_vertex g v')
      with Invalid_argument _ -> true
    in
    G.fold_vertex f g true


  let game _ p a b =

    let rec game_aux l pi =
      let continue x =
        try
          game_aux (SetV.add pi l) (S.next x pi)
        with Invalid_argument _ -> false
      in
      (P.is_final p pi) ||
      (if SetV.mem pi l then false
       else
       if P.turn p pi then continue a
       else continue b)

    in
    game_aux SetV.empty (P.get_initial p)


  let attract1 g p s l =
    let f v l1 =
      if not (List.mem v l1) then
        if P.turn p v then
          try
            if List.mem (S.next s v) l1 then v :: l1
            else l1
          with Invalid_argument _ -> l1
        else
        if puit g v then l1
        else
        if G.fold_succ (fun v' b -> b && (List.mem v' l1)) g v true
        then v :: l1
        else l1
      else l1
    in
    G.fold_vertex f g l


  let strategy g p s =

    let rec strategy_aux l1 l2 =
      let (b1, b2) = eq_mem (P.get_initial p) l1 l2 in
      if b1 then b2
      else strategy_aux (attract1 g p s l1) l1

    in
    let finaux = get_finals g p in
    strategy_aux (attract1 g p s finaux) finaux


  let attract g p (l, l') =
    let f v (l1, l1') =
      if not (List.mem v l1) then
        if P.turn p v then
          let f' v' l2 =
            (match l2 with
               [] ->
               if List.mem v' l1 then [v']
               else []
             | _ -> l2) in
          (match G.fold_succ f' g v [] with
             [] -> (l1, l1')
           | v' :: _ -> (v :: l1, S.add l1' v v' ))
        else
        if puit g v then (l1, l1')
        else
        if G.fold_succ (fun v' b -> b && (List.mem v' l1)) g v true
        then (v :: l1, l1')
        else (l1, l1')
      else (l1, l1')
    in
    G.fold_vertex f g (l, l')


  let strategyA g p =

    let rec strategyA_aux l1 l2 f =
      let (b1, b2) = eq_mem (P.get_initial p) l1 l2 in
      if b1 then (b2, f)
      else
        let (new_l1, new_f) = attract g p (l1, f) in
        strategyA_aux new_l1 l1 new_f

    in
    let finaux = get_finals g p in
    let (l, r) = attract g p (finaux, S.empty) in
    strategyA_aux l finaux r;;

end
