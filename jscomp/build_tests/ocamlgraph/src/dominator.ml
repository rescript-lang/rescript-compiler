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

(*
  Copyright © 2009 Carnegie-Mellon University, David Brumley, and Ivan Jager.
  From the BAP library; see http://bap.ece.cmu.edu
  Modified by OCamlGraph's authors.
*)

(* stuff to read:
   http://www.hipersoft.rice.edu/grads/publications/dom14.pdf
   Modern Compiler Implementation in ML, by Appel
   Introduction to Algorithms, Cormen et al
*)

exception Unreachable

module type G = sig
  type t
  module V : Sig.COMPARABLE
  val pred : t -> V.t -> V.t list
  val succ : t -> V.t -> V.t list
  val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  val nb_vertex : t -> int
end

module type I = sig
  include G
  val create: ?size:int -> unit -> t
  val add_edge: t -> V.t -> V.t -> unit
end

module type S = sig
  type t
  type vertex
  module S : Set.S with type elt = vertex
  type idom = vertex -> vertex
  type idoms = vertex -> vertex -> bool
  type dom_tree = vertex -> vertex list
  type dominators = vertex -> vertex list
  type dom = vertex -> vertex -> bool
  type sdom = vertex -> vertex -> bool
  type dom_frontier = vertex -> vertex list
  val compute_idom: t -> vertex -> vertex -> vertex
  val dominators_to_dom: ('a -> S.t) -> vertex -> 'a -> bool
  val dominators_to_sdom: (vertex -> S.t) -> vertex -> vertex -> bool
  val dom_to_sdom: (vertex -> vertex -> bool) -> vertex -> vertex -> bool
  val dominators_to_sdominators: (vertex -> S.t) -> vertex -> S.t
  val dominators_to_idoms: (vertex -> S.t) -> vertex -> vertex -> bool
  val dominators_to_dom_tree:
    t ->
    ?pred:(t -> vertex -> vertex list) -> (vertex -> S.t) -> vertex -> S.t
  val idom_to_dom_tree: t -> (vertex -> vertex) -> vertex -> vertex list
  val idom_to_idoms: idom -> vertex -> vertex -> bool
  val compute_dom_frontier: t -> dom_tree -> idom -> vertex -> vertex list
  val idom_to_dominators: ('a -> 'a) -> 'a -> 'a list
  val idom_to_dom: (vertex -> vertex) -> vertex -> vertex -> bool
end

module Make(G : G) = struct

  type t = G.t
  type vertex = G.V.t

  module H = Hashtbl.Make(G.V)
  module S = Set.Make(G.V)

  (** function from [n] to [n]'s immediate dominator *)
  type idom = vertex -> vertex

  (** [idoms x y] is true when [x] is [y]'s immediate dominator *)
  type idoms = vertex -> vertex -> bool

  (** function from [x] to a list of nodes immediately dominated by [x] *)
  type dom_tree = vertex -> vertex list

  (** function from node to a list of nodes that dominate it. *)
  type dominators = vertex -> vertex list

  (** [dom x y] returns true iff [x] dominates [y] *)
  type dom = vertex -> vertex -> bool

  (** [sdom x y] returns true iff [x] strictly dominates [y]. *)
  type sdom = vertex -> vertex -> bool

  (** function from [x] to a list of nodes not dominated by [x], but with
      predecessors which are dominated by [x] *)
  type dom_frontier = vertex -> vertex list

  let set_of_list x = List.fold_left (fun set v -> S.add v set) S.empty x

  (** Computes the dominator tree, using the Lengauer-Tarjan algorithm.
      [compute_idom cfg s0] returns a function [idom : V.t -> V.t] s.t.
      [idom x] returns the immediate dominator of [x]
  *)
  let compute_idom cfg s0 =
    (* based on the Tiger book, section 19.2.
       This uses path compression, but doesn't yet do balanced path
       compression, so the runtime is O(N log(N)) rather than
       O(N inverseackerman(N))
    *)
    let size = G.nb_vertex cfg in
    let bucket = H.create size (* node n -> *)
    and dfnum_h = H.create size (* node -> DFS number *)
    and parent = H.create size (* node -> parent in DFS tree*)
    and semi_h = H.create size (* node -> semidominator *)
    and ancestor = H.create size (* node -> *)
    and best = H.create size (* node -> *)
    and samedom = H.create size (* node -> node with same idom *)
    and idom = H.create size (* node n -> idom n *)
    and vertex = Array.make size s0 (* DFS number -> node *)
    and nn = ref 0 in
    let dfnum x = try  H.find dfnum_h x with Not_found -> raise Unreachable
    and semi = H.find semi_h in
    let dfs n0 =
      let stack = Stack.create () in
      let loop () =
        while not (Stack.is_empty stack) do
          let n,p = Stack.pop stack in
          if not (H.mem dfnum_h n) then begin
            let enn = !nn in
            H.add dfnum_h n enn;
            vertex.(enn) <- n;
            begin match p with
              | Some p -> H.add parent n p
              | None -> () end;
            nn := enn + 1;
            G.iter_succ
              (fun m ->
                 if not (H.mem dfnum_h m) then Stack.push (m, Some n) stack)
              cfg n
          end
        done
      in
      Stack.push (n0,None) stack;
      loop ()
    in
    let rec ancestor_with_lowest_semi v =
      try
        let a = H.find ancestor v in
        let b = ancestor_with_lowest_semi a in
        let () = H.replace ancestor v (H.find ancestor a) in
        let best_v = H.find best v in
        if dfnum(semi b) < dfnum(semi best_v)
        then (H.replace best v b; b)
        else best_v
      with Not_found -> H.find best v
    in
    let link p n =
      H.replace ancestor n p;
      H.replace best n n;
    in
    let semidominator n =
      let s = H.find parent n in
      List.fold_left
        (fun s v ->
           try (* FIXME: do we want to allow unreachable nodes? *)
             let s' =
               if dfnum v <= dfnum n
               then v
               else semi(ancestor_with_lowest_semi v)
             in
             if dfnum s' < dfnum s then s' else s
           with Unreachable -> (* maybe switch to Not_found later *)
             s (* v is unreachable from s0 *)
        )
        s
        (G.pred cfg n)
    in
    let () = dfs s0 in
    let lastn = !nn - 1 in
    while decr nn; !nn > 0 do (* skip over the root node *)
      let i = !nn in
      let n = vertex.(i) in
      let p = H.find parent n in
      let s = semidominator n in
      H.add semi_h n s;
      H.add bucket s  n;
      link p n;
      (* now that the path from p to v is in the forest,
         calculate the dominator of v based on the first clause of the
         Dominator Theorem, otherwise defer until y's dominator is known *)
      List.iter
        (fun v ->
           let y = ancestor_with_lowest_semi v in
           if G.V.equal (semi y) (semi v)
           then H.add idom v p
           else H.add samedom v y;
           H.remove bucket p (*could use H.remove_all if we used extlib*)
        )
        (H.find_all bucket p)
    done;
    (* now all the defered calculations can be done *)
    for i = 1 to lastn do
      let n = vertex.(i) in
      try
        H.add idom n (H.find idom (H.find samedom n))
      with Not_found -> ()
    done;
    H.find idom


  (** Given a function from a node to it's dominators, returns a function
      [dom : V.t -> V.t -> bool] s.t. [dom x y] returns true when
      [x] dominates [y]
  *)
  let dominators_to_dom dominators x y =
    S.mem x (dominators y)

  (** Given a function from a node to it's dominators, returns a function
      [sdom : V.t -> V.t -> bool] s.t. [sdom x y] returns true when
      [x] strictly dominates [y] *)
  let dominators_to_sdom dominators x y =
    not(G.V.equal x y) && dominators_to_dom dominators x y

  let dom_to_sdom dom x y =
    not(G.V.equal x y) && dom x y

  (** Given a a function from a node to it's dominators, returns a function
      from a node to it's strict dominators. *)
  let dominators_to_sdominators dominators x =
    S.remove x (dominators x)


  (** Given a function from a node to it's dominators, returns a function
      [idoms : G.V.t -> G.V.t -> bool] s.t. [idoms x y] returns true when
      [x] is the immediate dominator of [y].
  *)
  let dominators_to_idoms dominators =
    let sdom = dominators_to_sdom dominators in
    (fun x y ->
       sdom x y
       && let sdoms = dominators_to_sdominators dominators y in
       S.for_all (fun w -> G.V.equal x w || not(sdom x w)) sdoms
    )


  (** Computes a dominator tree (function from x to a list of nodes immediately
      dominated by x) for the given CFG and dominator function.
      Note: The dominator tree is also called [IDom] by Muchnick.
      Note: If you are computing a post-dominator tree, then the
      optional argument pred should be G.succ.
  *)
  let dominators_to_dom_tree cfg ?(pred=G.pred) dominators =
    let idoms = dominators_to_idoms dominators in
    let tree = H.create 97 in
    let () =
      G.iter_vertex
        (fun y ->
           match pred cfg y with
             [x] -> (
               (* a node that is not reachable from start has no
                  idom *)
               if S.is_empty (dominators x) then () else
                 H.add tree x y
             )
           | _ -> (
               S.iter
                 (fun x -> if idoms x y then H.add tree x y)
                 (dominators y)
             )
        )
        cfg
    in
    (* FIXME: maybe faster to convert eagerly *)
    fun x -> set_of_list(H.find_all tree x)

  (** Computes a dominator tree (function from x to a list of nodes immediately
      dominated by x) for the given CFG and idom function. *)
  let idom_to_dom_tree cfg idom =
    let tree = H.create (G.nb_vertex cfg) in
    let () =
      G.iter_vertex
        (fun v ->
           try H.add tree (idom v) v
           with Not_found -> () (* s0 doesn't have an idom *)
        )
        cfg
    in
    H.find_all tree


  let idom_to_idoms (idom:idom) x y =
    try G.V.equal x (idom y)
    with Not_found -> false (* s0 doesn't have an idom *)

  (** Computes the dominance frontier.
      As specified in section 19.1 of Modern Compiler Implementation in ML
      by Andrew Appel.
  *)
  let compute_dom_frontier cfg (dom_tree: dom_tree) (idom: idom) =
    let children = dom_tree in
    let idoms = idom_to_idoms idom in
    let df_cache = H.create 57 in
    let df_local n =
      (* the successors of n that are not strictly dominated by n *)
      List.filter (fun y -> not (idoms n y)) (G.succ cfg n)
    in
    (* written in CPS to prevent stack overflow *)
    let rec df n k =
      match try Some (H.find df_cache n) with Not_found -> None with
      | Some r -> k r
      | None ->
        let s = df_local n in
        add_df_ups s n (fun res -> H.add df_cache n res; k res) (children n)
    and add_df_ups s n k = function
      | [] -> k s
      | c :: chl ->
        df c (fun dfc ->
            add_df_ups
              (List.fold_left
                 (* the appel errata uses sdom, but Muchnick uses idoms, which
                    should be a bit faster and is the same *)
                 (fun s w  -> if idoms n w then s else w :: s) s dfc)
              n k chl)
    in
    fun n -> df n (fun x -> x)

  let idom_to_dominators idom x =
    let rec d y list =
      try
        let i = idom y in
        d i (i::list)
      with Not_found ->
        list
    in
    d x []

  let rec idom_to_dom idom x y =
    try
      let d = idom y in
      G.V.equal x d || idom_to_dom idom x d
    with Not_found ->
      false

end

module Make_graph(G: I) = struct

  include Make(G)

  type dom_graph = unit -> t

  type dom_functions = {
    idom : idom;
    idoms: idoms;
    dom_tree: dom_tree;
    dominators: dominators;
    dom: dom;
    sdom: sdom;
    dom_frontier: dom_frontier;
    dom_graph : dom_graph;
  }

  let compute_dom_graph cfg dom_tree =
    let g = G.create ~size:(G.nb_vertex cfg) () in
    G.iter_vertex (fun p ->
        try
          List.iter (G.add_edge g p) (dom_tree p)
        with Not_found -> ()
      ) cfg;
    g

  (** Computes all dominance functions.
      This function computes some things eagerly and some lazily, so don't
      worry about it doing extra work to compute functions you don't need,
      but also don't call it if you aren't going to use anything it returns.
      @return a record containing all dominance functions for the given graph
      and entry node.
  *)
  let compute_all cfg s0 =
    let idom = compute_idom cfg s0 in
    let idoms = idom_to_idoms idom in
    let dom_tree = lazy(idom_to_dom_tree cfg idom) in
    let dominators = idom_to_dominators idom in
    let dom = idom_to_dom idom in
    let sdom = dom_to_sdom dom in
    let dom_frontier =
      lazy(compute_dom_frontier cfg (Lazy.force dom_tree) idom)
    in
    {
      idom=idom;
      idoms=idoms;
      dom_tree=(fun x -> Lazy.force dom_tree x);
      dominators=dominators;
      dom=dom;
      sdom=sdom;
      dom_frontier=(fun x -> Lazy.force dom_frontier x);
      dom_graph=(fun () -> compute_dom_graph cfg (Lazy.force dom_tree));
    }

end
