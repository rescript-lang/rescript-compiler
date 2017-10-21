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

(* Copyright (c) 2010 - 2011 Technische Universitaet Muenchen
 * Markus W. Weissmann <markus.weissmann@in.tum.de>
 * Florian Pichlmeier <florian.pichlmeier@in.tum.de>
 * All rights reserved. *)

(* Minimal graph signature for leader list algorithm *)
module type G = sig
  type t
  module V : Sig.COMPARABLE
  val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
  val succ : t -> V.t -> V.t list
  val pred : t -> V.t -> V.t list
end

module Make
    (G : G) =
struct
  module S = Set.Make(G.V)

  let leader_lists g root =
    (* partition all vertices into two Sets *)
    let partition_vertices f g =
      G.fold_vertex
        (fun n (s1, s2) ->
           if f n then (S.add n s1, s2) else (s1, S.add n s2))
        g (S.empty, S.empty)
    in

    (* predicate to determine if a node is a leader *)
    let is_leader n =
      if n = root then true (* the root node is always a leader *)
      else
        match G.pred g n with
        | [] ->
          true
        (* this would be dead code --
           it has no predecessor so make it a leader anyway *)
        | x::[] -> begin match G.succ g x with
            | [] ->
              assert false (* -> inconsistency in the graph implementation *)
            | _::[] ->
              false (* this is a straight, continuous control flow *)
            | _ ->
              true (* predecessor has multiple successors *)
          end
        | _ -> true (* more than one predecessor *)
    in
    let (leader, entourage) = partition_vertices is_leader g in
    (* build a basic block *)
    let basic_block x =
      let rec basic_block x bb =
        match G.succ g x with
        | [] -> x::bb (* no successors -- end of basic block *)
        | y::_ -> begin match S.mem y entourage with
            | true ->
              (* successor is not a leader, continue collecting *)
              basic_block y (x::bb)
            | false ->
              x :: bb (* successor is a leader -- end of basic block *)
          end
      in
      (* blocks a are built in reverse order for performance reasons --
         correct that *)
      List.rev (basic_block x [])
    in
    let basic_block_list = S.fold (fun x ss -> (basic_block x)::ss) leader [] in
    List.rev basic_block_list
    (* this will bring the head of the lists in ascending order --
       as given by Set.S *)
end

