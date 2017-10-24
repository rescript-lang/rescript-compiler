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

(* $Id: mcs_m.mli,v 1.2 2004-10-19 15:21:44 signoles Exp $ *)

module MaximalCardinalitySearch = struct

  module WeightedV(V : Sig.COMPARABLE) = struct
    include Util.DataV(struct type t = int end)(V)
    let weight = data
    let set_weight = set_data
  end

  module P(Gr : Sig.P) = struct
    type edgelist = (Gr.V.t * Gr.V.t) list

    module NewV = WeightedV(Gr.V)
    module G = Persistent.Graph.Concrete(NewV)
    module EdgeSet = Set.Make(G.E)
    module VerticesSet = Set.Make(NewV)
    module Choose = Oper.Choose(G)
    module H = Hashtbl.Make(NewV)

    let check_path g u v =
      let h = H.create 97 in
      let maxw = NewV.weight u in
      let rec aux x : bool =
        if H.mem h x then
          false
        else
        if x = v then true
        else
        if NewV.weight x < maxw || x = u then
          begin
            H.add h x ();
            G.fold_succ
              (fun x found ->
                 if not found then aux x
                 else found)
              g x false
          end
        else (H.add h x (); false)
      in aux u

    module Copy = Gmap.Vertex(Gr)(struct include G include Builder.P(G) end)

    let fold f d =
      let rec aux = function
          (true, a) -> aux (f a)
        | (false, a) -> a
      in aux d

    let mcsm g =
      let g' = Copy.map (NewV.create 0) g in
      let (_, _, ord, triang) =
        fold
          (fun ((i, g', a, f) as x)->
             if i = 0 then (false, x)
             else
               let v =
                 G.fold_vertex
                   (fun x max ->
                      if NewV.weight x > NewV.weight max then x else max)
                   g' (ref 0, snd (Choose.choose_vertex g'))
               in
               let s =
                 G.fold_vertex
                   (fun x s ->
                      if x = v then s
                      else
                      if check_path g' x v then
                        VerticesSet.add x s
                      else s)
                   g' VerticesSet.empty
               in
               let f' =
                 VerticesSet.fold
                   (fun x f ->
                      NewV.set_weight x (succ (NewV.weight x));
                      if not (G.mem_edge g' x v) then
                        EdgeSet.add (x,v) f
                      else f)
                   s f
               in
               let g' = G.remove_vertex g' v in
               let a' = (i, NewV.label v) :: a in
               (true, (i - 1, g', a', f')))
          (true, (Gr.nb_vertex g, g', [], EdgeSet.empty))
      in
      (List.rev ord,
       EdgeSet.fold
         (fun (x, y) e -> (NewV.label x, NewV.label y) :: e)
         triang [])

    let triangulate g =
      let (_, triang) = mcsm g in
      List.fold_left (fun g (x, y) -> Gr.add_edge g x y) g triang
  end

  module I(Gr : Sig.I) = struct
    type edgelist = (Gr.V.t * Gr.V.t) list

    module NewV = WeightedV(Gr.V)
    module G = Imperative.Graph.Concrete(NewV)
    module EdgeSet = Set.Make(G.E)
    module VerticesSet = Set.Make(NewV)
    module Choose = Oper.Choose(G)
    module H = Hashtbl.Make(NewV)

    let check_path g u v =
      let h = H.create 97 in
      let maxw = NewV.weight u in
      let rec aux x : bool =
        if H.mem h x then
          false
        else
        if x = v then true
        else
        if NewV.weight x < maxw || x = u then begin
          H.add h x ();
          G.fold_succ
            (fun x found ->
               if not found then aux x
               else found)
            g x false
        end else (H.add h x (); false)
      in aux u

    module Copy = Gmap.Vertex(Gr)(struct include G include Builder.I(G) end)

    let mcsm g =
      let f = ref EdgeSet.empty
      and a = ref []
      and g' = Copy.map (NewV.create 0) g in
      for i = Gr.nb_vertex g downto 1 do
        let v =
          G.fold_vertex
            (fun x max ->
               if NewV.weight x > NewV.weight max then x else max)
            g' (ref 0, snd (Choose.choose_vertex g'))
        in
        let s =
          G.fold_vertex
            (fun x s ->
               if x = v then s
               else
               if check_path g' x v then
                 VerticesSet.add x s
               else s)
            g' VerticesSet.empty
        in
        let f' =
          VerticesSet.fold
            (fun x f ->
               NewV.set_weight x (succ (NewV.weight x));
               if not (G.mem_edge g' x v) then
                 EdgeSet.add (x,v) f
               else f)
            s !f
        in
        f := f';
        G.remove_vertex g' v;
        a := (i, NewV.label v) :: !a;
      done;
      (List.rev !a,
       EdgeSet.fold
         (fun (x, y) e -> (NewV.label x, NewV.label y) :: e)
         !f [])

    let triangulate g =
      let (_, triang) = mcsm g in
      List.iter (fun (x, y) -> Gr.add_edge g x y) triang
  end
end
