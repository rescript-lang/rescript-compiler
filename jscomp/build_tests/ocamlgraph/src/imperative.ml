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

open Sig
open Blocks

module type S = sig

  (** Imperative Unlabeled Graphs *)
  module Concrete (V: COMPARABLE) :
    Sig.I with type V.t = V.t and type V.label = V.t and type E.t = V.t * V.t
                                                     and type E.label = unit

  (** Abstract Imperative Unlabeled Graphs *)
  module Abstract(V: sig type t end) :
    Sig.IM with type V.label = V.t and type E.label = unit
                                   and type E.label = unit

  (** Imperative Labeled Graphs *)
  module ConcreteLabeled (V: COMPARABLE)(E: ORDERED_TYPE_DFT) :
    Sig.I with type V.t = V.t and type V.label = V.t
                              and type E.t = V.t * E.t * V.t and type E.label = E.t

  (** Abstract Imperative Labeled Graphs *)
  module AbstractLabeled (V: sig type t end)(E: ORDERED_TYPE_DFT) :
    Sig.IM with type V.label = V.t and type E.label = E.t

end

module I = Make(Make_Hashtbl)

type 'a abstract_vertex = { tag : int; label : 'a; mutable mark : int }

(* Implement module type [MARK]. *)
module Make_Mark
    (X: sig
       type graph
       type label
       val iter_vertex : (label abstract_vertex -> unit) -> graph -> unit
     end) =
struct
  type vertex = X.label abstract_vertex
  type graph = X.graph
  let get v = v.mark
  let set v m = v.mark <- m
  let clear g = X.iter_vertex (fun v -> set v 0) g
end

(* Vertex for abstract imperative graphs:
   comparing to vertex for abstract **persistent** graphs, marks are added. *)
module AbstractVertex(V: sig type t end) = struct
  type label = V.t
  type t = label abstract_vertex
  let compare x y = Pervasives.compare x.tag y.tag
  let hash x = x.tag
  let equal x y = x.tag = y.tag
  let label x = x.label
  let create l =
    if !cpt_vertex = first_value_for_cpt_vertex - 1 then
      invalid_arg "Too much vertices";
    incr cpt_vertex;
    { tag = !cpt_vertex; label = l; mark = 0 }
end

module Digraph = struct

  module Concrete(V: COMPARABLE) = struct
    include I.Digraph.Concrete(V)
    let add_vertex g v = ignore (add_vertex g v)
    let add_edge g v1 v2 = ignore (add_edge g v1 v2)
    let remove_edge g v1 v2 = ignore (remove_edge g v1 v2)
    let remove_edge_e g e = ignore (remove_edge_e g e)
    let add_edge_e g e = ignore (add_edge_e g e)
    let remove_vertex g v =
      if HM.mem v g then begin
        ignore (HM.remove v g);
        HM.iter (fun k s -> ignore (HM.add k (S.remove v s) g)) g
      end
  end

  module ConcreteLabeled(V: COMPARABLE)(E: ORDERED_TYPE_DFT) = struct
    include I.Digraph.ConcreteLabeled(V)(E)
    let add_vertex g v = ignore (add_vertex g v)
    let remove_edge g v1 v2 = ignore (remove_edge g v1 v2)
    let remove_edge_e g e = ignore (remove_edge_e g e)
    let add_edge_e g e = ignore (add_edge_e g e)
    let add_edge g v1 v2 = ignore (add_edge g v1 v2)
    let remove_vertex g v =
      if HM.mem v g then begin
        ignore (HM.remove v g);
        let remove v = S.filter (fun (v2, _) -> not (V.equal v v2)) in
        HM.iter (fun k s -> ignore (HM.add k (remove v s) g)) g
      end
  end

  module ConcreteBidirectional(V: COMPARABLE) = struct

    include I.Digraph.ConcreteBidirectional(V)

    let add_vertex g v = ignore (add_vertex g v)
    let add_edge g v1 v2 = ignore (add_edge g v1 v2)
    let add_edge_e g (v1, v2) = add_edge g v1 v2

    let remove_edge g v1 v2 = ignore (remove_edge g v1 v2)
    let remove_edge_e g e = ignore (remove_edge_e g e)

    let remove_vertex g v =
      if HM.mem v g then begin
        iter_pred_e (fun e -> remove_edge_e g e) g v;
        iter_succ_e (fun e -> remove_edge_e g e) g v;
        ignore (HM.remove v g)
      end

  end

  module ConcreteBidirectionalLabeled(V:COMPARABLE)(E:ORDERED_TYPE_DFT) = struct

    include I.Digraph.ConcreteBidirectionalLabeled(V)(E)

    let add_vertex g v = ignore (add_vertex g v)
    let add_edge g v1 v2 = ignore (add_edge g v1 v2)
    let add_edge_e g (v1, l, v2) = ignore (add_edge_e g (v1, l, v2))

    let remove_edge g v1 v2 = ignore (remove_edge g v1 v2)
    let remove_edge_e g e = ignore (remove_edge_e g e)

    let remove_vertex g v =
      if HM.mem v g then begin
        iter_pred_e (fun e -> remove_edge_e g e) g v;
        iter_succ_e (fun e -> remove_edge_e g e) g v;
        ignore (HM.remove v g)
      end

  end

  module Abstract(V: sig type t end) = struct

    include I.Digraph.Abstract(AbstractVertex(V))

    let add_vertex g v =
      if not (HM.mem v g.edges) then begin
        g.size <- Pervasives.succ g.size;
        ignore (G.unsafe_add_vertex g.edges v)
      end

    let add_edge g v1 v2 =
      add_vertex g v1;
      add_vertex g v2;
      ignore (unsafe_add_edge g.edges v1 v2)

    let add_edge_e g (v1, v2) = add_edge g v1 v2

    let remove_vertex g v =
      if HM.mem v g.edges then
        let e = g.edges in
        ignore (HM.remove v e);
        HM.iter (fun k s -> ignore (HM.add k (S.remove v s) e)) e;
        g.size <- Pervasives.pred g.size

    module Mark =
      Make_Mark
        (struct
          type graph = t
          type label = V.label
          let iter_vertex = iter_vertex
        end)

    let remove_edge g v1 v2 = ignore (remove_edge g v1 v2)
    let remove_edge_e g e = ignore (remove_edge_e g e)

  end

  module AbstractLabeled(V: sig type t end)(Edge: ORDERED_TYPE_DFT) = struct

    include I.Digraph.AbstractLabeled(AbstractVertex(V))(Edge)

    let add_vertex g v =
      if not (HM.mem v g.edges) then begin
        g.size <- Pervasives.succ g.size;
        ignore (G.unsafe_add_vertex g.edges v)
      end

    let add_edge_e g (v1, l, v2) =
      add_vertex g v1;
      add_vertex g v2;
      ignore (unsafe_add_edge g.edges v1 (v2, l))

    let add_edge g v1 v2 = add_edge_e g (v1, Edge.default, v2)

    let remove_vertex g v =
      if HM.mem v g.edges then
        let remove s =
          S.fold
            (fun (v2, _ as e) s -> if not (V.equal v v2) then S.add e s else s)
            s S.empty
        in
        let e = g.edges in
        ignore (HM.remove v e);
        HM.iter (fun k s -> ignore (HM.add k (remove s) e)) e;
        g.size <- Pervasives.pred g.size

    module Mark =
      Make_Mark
        (struct
          type graph = t
          type label = V.label
          let iter_vertex = iter_vertex
        end)

    let remove_edge g v1 v2 = ignore (remove_edge g v1 v2)
    let remove_edge_e g e = ignore (remove_edge_e g e)

  end

end

module Graph = struct

  module Concrete(V: COMPARABLE) = struct

    module G = struct include Digraph.Concrete(V) type return = unit end
    include Graph(G)

    (* Redefine the [add_edge] and [remove_edge] operations *)

    let add_edge g v1 v2 =
      if not (mem_edge g v1 v2) then begin
        G.add_edge g v1 v2;
        assert (G.HM.mem v1 g && G.HM.mem v2 g);
        ignore (G.unsafe_add_edge g v2 v1)
      end

    let add_edge_e g (v1, v2) = add_edge g v1 v2

    let remove_edge g v1 v2 =
      G.remove_edge g v1 v2;
      assert (G.HM.mem v1 g && G.HM.mem v2 g);
      ignore (G.unsafe_remove_edge g v2 v1)

    let remove_edge_e g (v1, v2) = remove_edge g v1 v2

  end

  module ConcreteLabeled (V: COMPARABLE)(Edge: ORDERED_TYPE_DFT) = struct

    module G = struct
      include Digraph.ConcreteLabeled(V)(Edge)
      type return = unit
    end
    include Graph(G)

    (* Redefine the [add_edge] and [remove_edge] operations *)

    let add_edge_e g (v1, l, v2 as e) =
      if not (mem_edge_e g e) then begin
        G.add_edge_e g e;
        assert (G.HM.mem v1 g && G.HM.mem v2 g);
        ignore (G.unsafe_add_edge g v2 (v1, l))
      end

    let add_edge g v1 v2 = add_edge_e g (v1, Edge.default, v2)

    let remove_edge g v1 v2 =
      G.remove_edge g v1 v2;
      assert (G.HM.mem v1 g && G.HM.mem v2 g);
      ignore (G.unsafe_remove_edge g v2 v1)

    let remove_edge_e g (v1, l, v2 as e) =
      G.remove_edge_e g e;
      assert (G.HM.mem v1 g && G.HM.mem v2 g);
      ignore (G.unsafe_remove_edge_e g (v2, l, v1))

  end

  module Abstract(V: sig type t end) = struct

    module G = struct include Digraph.Abstract(V) type return = unit end
    include Graph(G)

    (* Export some definitions of [G] *)
    module Mark = G.Mark

    (* Redefine the [add_edge] and [remove_edge] operations *)

    let add_edge g v1 v2 =
      G.add_edge g v1 v2;
      assert (G.HM.mem v1 g.G.edges && G.HM.mem v2 g.G.edges);
      ignore (G.unsafe_add_edge g.G.edges v2 v1)

    let add_edge_e g (v1, v2) = add_edge g v1 v2

    let remove_edge g v1 v2 =
      G.remove_edge g v1 v2;
      assert (G.HM.mem v1 g.G.edges && G.HM.mem v2 g.G.edges);
      ignore (G.unsafe_remove_edge g.G.edges v2 v1)

    let remove_edge_e g (v1, v2) = remove_edge g v1 v2

  end

  module AbstractLabeled (V: sig type t end)(Edge: ORDERED_TYPE_DFT) = struct

    module G = struct
      include Digraph.AbstractLabeled(V)(Edge)
      type return = unit
    end
    include Graph(G)

    (* Export some definitions of [G] *)
    module Mark = G.Mark

    (* Redefine the [add_edge] and [remove_edge] operations *)

    let add_edge_e g (v1, l, v2 as e) =
      G.add_edge_e g e;
      assert (G.HM.mem v1 g.G.edges && G.HM.mem v2 g.G.edges);
      ignore (G.unsafe_add_edge g.G.edges v2 (v1, l))

    let add_edge g v1 v2 = add_edge_e g (v1, Edge.default, v2)

    let remove_edge g v1 v2 =
      G.remove_edge g v1 v2;
      assert (G.HM.mem v1 g.G.edges && G.HM.mem v2 g.G.edges);
      ignore (G.unsafe_remove_edge g.G.edges v2 v1)

    let remove_edge_e g (v1, l, v2 as e) =
      ignore (G.remove_edge_e g e);
      assert (G.HM.mem v1 g.G.edges && G.HM.mem v2 g.G.edges);
      ignore (G.unsafe_remove_edge_e g.G.edges (v2, l, v1))

  end

end

module Matrix = struct

  module type S = sig
    include Sig.I with type V.t = int and type V.label = int
                                      and type E.t = int * int
    val make : int -> t
  end

  module Digraph = struct

    module V = struct
      type t = int
      type label = int
      let compare : t -> t -> int = Pervasives.compare
      let hash = Hashtbl.hash
      let equal = (==)
      let create i = i
      let label i = i
    end

    module E = struct
      type t = V.t * V.t
      type vertex = V.t
      let compare : t -> t -> int = Pervasives.compare
      type label = unit
      let create v1 _ v2 = (v1, v2)
      let src = fst
      let dst = snd
      let label _ = ()
    end

    type t = Bitv.t array
    type vertex = V.t
    type edge = E.t

    let create ?size:_ () =
      failwith
        "[ocamlgraph] do not use Matrix.create; please use Matrix.make instead"

    let make n =
      if n < 0 then invalid_arg "[ocamlgraph] Matrix.make";
      Array.init n (fun _ -> Bitv.create n false)

    let is_directed = true

    let nb_vertex = Array.length
    let is_empty g = nb_vertex g = 0
    let nb_edges =
      Array.fold_left (Bitv.fold_left (fun n b -> if b then n+1 else n)) 0

    let mem_vertex g v = 0 <= v && v < nb_vertex g
    let mem_edge g i j = Bitv.get g.(i) j
    let mem_edge_e g (i,j) = Bitv.get g.(i) j
    let find_edge g i j = if mem_edge g i j then i, j else raise Not_found
    let find_all_edges g i j = try [ find_edge g i j ] with Not_found -> []

    (* constructors *)
    let add_edge g i j = Bitv.set g.(i) j true
    let add_edge_e g (i,j) = Bitv.set g.(i) j true

    let remove_edge g i j = Bitv.set g.(i) j false
    let remove_edge_e g (i,j) = Bitv.set g.(i) j false

    let unsafe_add_edge g i j =
      Bitv.unsafe_set (Array.unsafe_get g i) j true
    let unsafe_remove_edge g i j =
      Bitv.unsafe_set (Array.unsafe_get g i) j false

    let remove_vertex _ _ = ()
    let add_vertex _ _ = ()

    let clear g =
      Array.iter (fun b -> Bitv.iteri (fun j _ -> Bitv.set b j false) b) g

    let copy g = Array.init (nb_vertex g) (fun i -> Bitv.copy g.(i))

    (* iter/fold on all vertices/edges of a graph *)
    let iter_vertex f g =
      for i = 0 to nb_vertex g - 1 do f i done

    let iter_edges f g =
      for i = 0 to nb_vertex g - 1 do
        Bitv.iteri (fun j b -> if b then f i j) g.(i)
      done

    let fold_vertex f g a =
      let n = nb_vertex g in
      let rec fold i a = if i = n then a else fold (i+1) (f i a) in fold 0 a

    let fold_edges f g a =
      fold_vertex
        (fun i a ->
           Bitv.foldi_right (fun j b a -> if b then f i j a else a) g.(i) a)
        g a

    (* successors and predecessors of a vertex *)
    let succ g i =
      Bitv.foldi_left (fun l j b -> if b then j::l else l) [] g.(i)

    let pred g i =
      fold_vertex
        (fun j a -> if Bitv.unsafe_get g.(j) i then j :: a else a)
        g []

    (* iter/fold on all successor/predecessor of a vertex. *)
    let iter_succ f g i =
      let si = g.(i) in
      for j = 0 to nb_vertex g - 1 do if Bitv.unsafe_get si j then f j done
    (* optimization w.r.t.
       [Bitv.iteri (fun j b -> if b then f j) g.(i)]
    *)

    let iter_pred f g i =
      for j = 0 to nb_vertex g - 1 do if Bitv.unsafe_get g.(j) i then f j done

    let fold_succ f g i a =
      Bitv.foldi_right (fun j b a -> if b then f j a else a) g.(i) a

    let fold_pred f g i a =
      fold_vertex
        (fun j a -> if Bitv.unsafe_get g.(j) i then f j a else a)
        g a

    (* degree *)
    let out_degree g i = fold_succ (fun _ n -> n + 1) g i 0

    let in_degree g i = fold_pred (fun _ n -> n + 1) g i 0

    (* map iterator on vertex *)
    let map_vertex f g =
      let n = nb_vertex g in
      let g' = make n in
      iter_edges
        (fun i j ->
           let fi = f i in
           let fj = f j in
           if fi < 0 || fi >= n || fj < 0 || fj >= n then
             invalid_arg "[ocamlgraph] map_vertex";
           Bitv.unsafe_set g'.(fi) fj true)
        g;
      g'

    (* labeled edges going from/to a vertex *)
    (* successors and predecessors of a vertex *)
    let succ_e g i =
      Bitv.foldi_left (fun l j b -> if b then (i,j)::l else l) [] g.(i)

    let pred_e g i =
      fold_vertex
        (fun j a -> if Bitv.unsafe_get g.(j) i then (j,i) :: a else a)
        g []

    (* iter/fold on all labeled edges of a graph *)
    let iter_edges_e f g =
      for i = 0 to nb_vertex g - 1 do
        Bitv.iteri (fun j b -> if b then f (i,j)) g.(i)
      done

    let fold_edges_e f g a =
      fold_vertex
        (fun i a ->
           Bitv.foldi_right (fun j b a -> if b then f (i,j) a else a) g.(i) a)
        g a

    (* iter/fold on all edges going from/to a vertex *)
    let iter_succ_e f g i =
      let si = g.(i) in
      for j = 0 to nb_vertex g - 1 do if Bitv.unsafe_get si j then f (i,j) done

    let iter_pred_e f g i =
      for j = 0 to nb_vertex g - 1 do
        if Bitv.unsafe_get g.(j) i then f (j,i)
      done

    let fold_succ_e f g i a =
      Bitv.foldi_right (fun j b a -> if b then f (i,j) a else a) g.(i) a

    let fold_pred_e f g i a =
      fold_vertex
        (fun j a -> if Bitv.unsafe_get g.(j) i then f (j,i) a else a)
        g a

  end

  module Graph = struct

    module G = struct include Digraph type return = unit end
    include Blocks.Graph(G)

    (* Export some definitions of [G] *)
    let make = G.make

    (* Redefine the [add_edge] and [remove_edge] operations *)

    let add_edge g v1 v2 =
      G.add_edge g v1 v2;
      ignore (G.unsafe_add_edge g v2 v1)

    let add_edge_e g (v1, v2) = add_edge g v1 v2

    let remove_edge g v1 v2 =
      G.remove_edge g v1 v2;
      ignore (G.unsafe_remove_edge g v2 v1)

    let remove_edge_e g (v1, v2) = remove_edge g v1 v2

  end

end

(* Faster implementations when vertices are not shared between graphs. *)
(****
   module UV = struct

   let cpt_vertex = ref min_int

   type ('label, 'succ) vertex = {
    tag : int;
    label : 'label;
    mutable mark : int;
    mutable succ : 'succ;
   }

   module Digraph = struct

    module Abstract(L: ANY_TYPE) :
      Sig.IM with type V.label = L.t and type E.label = unit
    =
    struct

      module rec V :
        VERTEX with type label = L.t and type t = (L.t, S.t) vertex  =
      struct
   type label = L.t
   type t = (L.t, S.t) vertex

   let compare x y = compare x.tag y.tag
   let hash x = Hashtbl.hash x.tag
   let equal x y = x.tag = y.tag
   let label x = x.label

   let create l =
    assert (!cpt_vertex < max_int);
    incr cpt_vertex;
    { tag = !cpt_vertex; label = l; mark = 0; succ = S.empty }
      end
      and S : Set.S with type elt = V.t = Set.Make(V)

      type vertex = V.t

      module E = struct
   type t = V.t * V.t
   type vertex = V.t
   let compare = Pervasives.compare
   type label = unit
   let create v1 _ v2 = (v1, v2)
   let src = fst
   let dst = snd
   let label _ = ()
      end

      type edge = E.t

      type t = {
   mutable vertices : S.t;
      }

      let create ?size () = { vertices = S.empty }
      let is_directed = true
      let is_empty g = S.is_empty g.vertices
      let nb_vertex g = S.cardinal g.vertices
      let out_degree _ v = S.cardinal v.succ
      let clear g = g.vertices <- S.empty

      let add_vertex g v = g.vertices <- S.add v g.vertices
      let mem_vertex g v = S.mem v g.vertices
      let iter_vertex f g = S.iter f g.vertices
      let fold_vertex f g = S.fold f g.vertices
      let succ _ v = S.elements v.succ
      let succ_e _ v = List.map (fun w -> (v, w)) (S.elements v.succ)
      let iter_succ f _ v = S.iter f v.succ
      let iter_succ_e f _ v = S.iter (fun w -> f (v, w)) v.succ
      let fold_succ f _ v acc = S.fold f v.succ acc
      let fold_succ_e f _ v acc = S.fold (fun w acc -> f (v, w) acc) v.succ acc

      let add_edge _ v1 v2 = v1.succ <- S.add v2 v1.succ
      let add_edge_e g (v1, v2) = add_edge g v1 v2
      let mem_edge _ v1 v2 = S.mem v2 v1.succ
      let mem_edge_e g (v1, v2) = mem_edge g v1 v2
      let remove_edge _ v1 v2 = v1.succ <- S.remove v2 v1.succ
      let remove_edge_e g (v1, v2) = remove_edge g v1 v2
      let nb_edges g = fold_vertex (fun v n -> n + S.cardinal v.succ) g 0

      let find_edge g i j = if mem_edge g i j then i, j else raise Not_found
      let find_all_edges g i j = try [ find_edge g i j ] with Not_found -> []

      module Mark = struct
        type graph = t
        type vertex = V.t
        let clear g = S.iter (fun v -> v.mark <- 0) g.vertices
        let get v = v.mark
        let set v m = v.mark <- m
      end
    end

    module AbstractLabeled (V: ANY_TYPE)(E: ORDERED_TYPE_DFT) :
      Sig.IM with type V.label = V.t and type E.label = E.t
    =
    AbstractLabeled
      (V)(struct type t = unit let compare _ _ = 0 let default = () end)

   end

   (**
   module Graph = struct

    module Abstract(V: ANY_TYPE) :
      Sig.IM with type V.label = V.t and type E.label = unit

    module AbstractLabeled (V: ANY_TYPE)(E: ORDERED_TYPE_DFT) :
      Sig.IM with type V.label = V.t and type E.label = E.t

   end
 **)
   end
 ****)

(*
Local Variables:
compile-command: "make -C .."
End:
*)
