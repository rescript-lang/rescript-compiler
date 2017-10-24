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

module CliqueTree(Gr : Sig.G) = struct

  (* Original vertex set (of Gr) *)
  module OVSet = Set.Make(Gr.V)

  (* Vertex signature *)
  module rec CliqueV :
  sig
    type t
    val compare : t -> t -> int
    val hash : t -> int
    val equal : t -> t -> bool
    val label : t -> t
    val create : Gr.V.t -> t
    val vertex : t -> Gr.V.t
    val number : t -> int
    val set_number : t -> int -> unit
    val clique : t -> int
    val set_clique : t -> int -> unit
    val mark : t -> int
    val incr_mark : t -> unit
    val m : t -> CVS.t
    val set_m : t -> CVS.t -> unit
    val last : t -> t
    val set_last : t -> t -> unit
  end =
  struct
    type t = {
      mutable mark: int;
      orig: Gr.V.t;
      mutable m: CVS.t;
      mutable last: t option;
      mutable number: int;
      mutable clique: int;
    }

    let compare x y = Gr.V.compare x.orig y.orig
    let hash x = Gr.V.hash x.orig
    let equal x y = Gr.V.equal x.orig y.orig

    let label x = x

    let create o = {
      mark = 0;
      orig = o;
      m = CVS.empty;
      last = None;
      number = 0;
      clique = -1;
    }

    let vertex x = x.orig

    let clique x = x.clique
    let set_clique x v = x.clique <- v

    let number x = x.number
    let set_number x v = x.number <- v

    let mark x = x.mark
    let incr_mark x =
      (*Printf.printf "Increasing mark of %s to %i\n%!"
        (Gr.v_to_string x.orig) (succ x.mark);*)
      x.mark <- succ x.mark

    let m x = x.m
    let set_m x v = x.m <- v

    let last x =
      match x.last with
        Some v -> v
      | None -> failwith "last not set"

    let set_last x v = x.last <- Some v

  end
  (* Clique tree vertex set *)
  and CVS : Set.S with type elt = CliqueV.t = Set.Make(CliqueV)

  (* The final clique tree vertex type:
     - set of original vertexes ordered by mark.
     - clique number.
  *)
  module CliqueTreeV =
    Util.DataV
      (struct type t = CliqueV.t list * CVS.t end)
      (struct
        type t = int
        let compare : t -> t -> int = Pervasives.compare
        let hash = Hashtbl.hash
        let equal x y = x = y
      end)

  module CliqueTreeE = struct
    type t = int * CVS.t

    let compare (x, _ : t) (y, _ : t) = Pervasives.compare x y

    let default = (0, CVS.empty)

    let create n s = (n, s)

    let vertices = snd

    let width g tri (_, x) =
      let vertices = List.map CliqueV.vertex (CVS.elements x) in
      let w =
        List.fold_left
          (fun w v ->
             List.fold_left
               (fun w v' ->
                  if v <> v' then
                    if not (Gr.mem_edge g v v') && Gr.mem_edge tri v v'
                    then succ w
                    else w
                  else w)
               w vertices)
          0 vertices
      in
      assert(w mod 2 = 0);
      w / 2
  end

  (* The returned tree *)
  module CliqueTree =
    Persistent.Digraph.ConcreteLabeled(CliqueTreeV)(CliqueTreeE)

  (* Intermediate graph *)
  module G = Persistent.Graph.Concrete(CliqueV)

  (* Convenient types *)
  module EdgeSet = Set.Make(G.E)
  module H = Hashtbl.Make(CliqueV)

  (* Used to choose some vertex in the intermediate graph *)
  module Choose = Oper.Choose(G)

  (* Creates the intermediate graph from the original *)
  module Copy = Gmap.Vertex(Gr)(struct include G include Builder.P(G) end)

  open CliqueV

  let mcs_clique g =
    (* initializations *)
    let n = Gr.nb_vertex g in
    let g' = Copy.map CliqueV.create g in
    let unnumbered = ref (G.fold_vertex CVS.add g' CVS.empty) in
    let pmark = ref (-1) in
    let order = ref [] in
    let cliques = Array.make n ([], CVS.empty) in
    let ties = ref [] in
    let j = ref 0 in
    (* loop, taking each unnumbered vertex in turn *)
    for i = n downto 1 do
      (* Find greatest unnumbered vertex
         if CVS.is_empty !unnumbered then
         Printf.printf "No more unnumbered vertices\n%!"
         else
         Printf.printf "%i unnumbered vertices remaining\n%!"
         (CVS.cardinal !unnumbered);
      *)
      let x, mark =
        let choosed = CVS.choose !unnumbered in
        CVS.fold
          (fun x ((_maxx, maxv) as max) ->
             let v = mark x in
             if v > maxv then (x, v) else max)
          !unnumbered (choosed, mark choosed)
      in
      (* peo construction *)
      order := x :: !order;
      (* now numbered *)
      unnumbered := CVS.remove x !unnumbered;
      if mark <= !pmark then begin
        (* Create a new clique (lemma 8) *)
        incr j;
        (* m x is the neighborhoud of x in the previous clique *)
        cliques.(!j) <- ([x], CVS.add x (m x));
        (* Use reverse map of cliques to find what clique
           we're connected to. m x is the width of the ties *)
        let clast = clique (last x) in
        ties := (clast, m x, !j) :: !ties;
      end else begin
        let l, c = cliques.(!j) in
        cliques.(!j) <- (x::l, CVS.add x c);
      end;
      G.iter_succ
        (fun y ->
           if number y == 0 then begin
             incr_mark y;
             set_m y (CVS.add x (m y));
           end;
           set_last y x)
        g' x;
      pmark := mark;
      set_number x i;
      set_clique x !j;
    done;
    let cliques =
      Array.mapi
        (fun i (l, c) -> CliqueTreeV.create (List.rev l, c) i)
        (Array.sub cliques 0 (succ !j))
    in
    let tree =
      Array.fold_left CliqueTree.add_vertex CliqueTree.empty cliques
    in
    let tree, _ =
      List.fold_left
        (fun (g, n) (i, verts, j) ->
           let label = CliqueTreeE.create n verts in
           let edge = CliqueTree.E.create cliques.(i) label cliques.(j) in
           (CliqueTree.add_edge_e g edge, succ n))
        (tree, 1) !ties
    in
    List.map CliqueV.vertex !order, tree, cliques.(0)

  let sons g x = CliqueTree.fold_succ (fun x y -> x :: y) g x []

  exception NotClique

  let test_simpliciality_first' l sons =
    List.for_all
      (fun son ->
         match !son with
         | [] -> false
         | xi :: _ ->
           let other = m xi in
           CVS.subset other l)
      sons

  let test_simpliciality_next vertices _sons =
    match vertices with
    | x :: tl ->
      begin
        try
          ignore(
            List.fold_left
              (fun vm v' ->
                 let vm' = CliqueV.m v' in
                 if CVS.equal vm' vm then
                   CVS.add v' vm'
                 else raise NotClique)
              (CVS.add x (m x)) tl);
          true
        with NotClique -> false
      end
    | _ -> true

  let is_chordal g =
    let _order, tree, root = mcs_clique g in
    let rec aux c =
      let csons = sons tree c in
      let s = List.map CliqueTreeV.data csons in
      let l = CliqueTreeV.data c in
      let sons () = List.map (fun (x,_) -> ref x) s in
      let first = test_simpliciality_first' (snd l) (sons ()) in
      let next = test_simpliciality_next (fst l) (sons ()) in
      first && next && (List.for_all aux csons)
    in
    aux root

  let maxwidth g tri tree =
    CliqueTree.fold_edges_e
      (fun e res ->
         let w = CliqueTreeE.width g tri (CliqueTree.E.label e) in
         max res w)
      tree 0

end
