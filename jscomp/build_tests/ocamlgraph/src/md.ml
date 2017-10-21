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

module P(G : Sig.P) = struct

  module VertexSet = Set.Make(G.V)
  module CT = Cliquetree.CliqueTree(G)
  module Choose = Oper.Choose(G)

  type edgeset = (G.V.t * G.V.t) list

  let md g =
    let gref = ref g in
    let gtri = ref g in
    let n = G.nb_vertex g in
    let tri = ref [] in
    let ord = ref [] in
    let i = ref 0 in
    while not (CT.is_chordal !gtri) && !i < n do
      let v =
        let x =
          G.fold_vertex
            (fun v' x ->
               let deg' = G.out_degree !gref v' in
               match x with
                 Some (_,deg) when deg' > deg -> x
               | _ -> Some (v', deg'))
            !gref None
        in match x with
          Some (v,_) -> v
        | None -> failwith "Expecting some vertex"
      in
      let ng = G.succ !gref v in
      let g', tri' =
        List.fold_left
          (fun (g, tri) v ->
             let tri' =
               List.fold_left
                 (fun tri v' ->
                    if v <> v' && not (G.mem_edge g v v') then
                      (v, v') :: tri
                    else tri)
                 tri ng
             in
             let g' =
               List.fold_left
                 (fun g v' ->
                    if v <> v' then
                      G.add_edge g v v'
                    else g)
                 g ng
             in
             (g', tri'))
          (!gref, []) ng
      in
      ord := v :: !ord;
      gtri := List.fold_left
          (fun g (x,y) -> G.add_edge g x y)
          !gtri tri';
      gref := G.remove_vertex g' v;
      tri := tri' @ !tri;
      incr i;
    done;
    (!gtri, !tri, !ord)

  let triangulate g =
    let gtri, _, _ = md g in
    gtri

end

module I(G : Sig.I) = struct

  module VertexSet = Set.Make(G.V)
  module CT = Cliquetree.CliqueTree(G)
  module Choose = Oper.Choose(G)

  type edgeset = (G.V.t * G.V.t) list

  module Copy = Gmap.Vertex(G)(struct include G include Builder.I(G) end)

  let md g =
    let gtri = Copy.map (fun x -> x) g in
    let gcur = Copy.map (fun x -> x) g in
    let n = G.nb_vertex g in
    let tri = ref [] in
    let ord = ref [] in
    let i = ref 0 in
    while not (CT.is_chordal gtri) && !i < n do
      let v =
        let x =
          G.fold_vertex
            (fun v' x ->
               let deg' = G.out_degree gcur v' in
               match x with
                 Some (_,deg) when deg' > deg -> x
               | _ -> Some (v', deg'))
            gcur None
        in match x with
          Some (v,_) -> v
        | None -> failwith "Expecting some vertex"
      in
      let ng = G.succ gcur v in
      let tri' =
        List.fold_left
          (fun tri v ->
             List.fold_left
               (fun tri v' ->
                  let tri' =
                    if v <> v' && not (G.mem_edge g v v') then
                      (v, v') :: tri
                    else
                      tri
                  in
                  List.iter (fun v' -> if v <> v' then G.add_edge gcur v v') ng;
                  tri')
               tri ng)
          [] ng
      in
      ord := v :: !ord;
      List.iter
        (fun (x,y) -> G.add_edge gtri x y)
        tri';
      G.remove_vertex gcur v;
      tri := tri' @ !tri;
      incr i;
    done;
    (gtri, !tri, !ord)

  let triangulate g =
    let gtri, _, _ = md g in
    gtri

end
