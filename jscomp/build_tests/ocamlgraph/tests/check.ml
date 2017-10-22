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

open Graph

module Int = struct
  type t = int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = 0
end

(* pair with equality which ignores the second component *)
module Pair = struct
  type t = int * int
  let compare (x, _) (y, _) = Int.compare x y
  let hash (x, _) = Int.hash x
  let equal (x, _) (y, _) = x = y
  let default = 0, 0
end

module W(E:sig type t type label val label: t -> label end) = struct
  type edge = E.t
  type t = int
  let weight = E.label
  let zero = 0
  let add = (+)
  let compare = compare
end

(********************************************)
(* Generic functions                        *)
(********************************************)

module Generic = struct

  (* Generic tests for imperative graphs *)
  module Make
    (G : Sig.I with type V.label = int)
    (V : sig val v: int val e: int end) =
  struct

    module O = Oper.I(G)
    let test_mirror g =
      if G.is_directed then begin (* TODO: remove *)
	let g' = O.mirror g in
	assert (G.nb_vertex g = G.nb_vertex g');
	G.iter_edges (fun v1 v2 -> assert (G.mem_edge g' v2 v1)) g;
	G.iter_edges (fun v1 v2 -> assert (G.mem_edge g v2 v1)) g';
	()
      end

    let g = G.create ()
    let () =
      let v1 = G.V.create 1 in
      let v2 = G.V.create 2 in
      let v3 = G.V.create 3 in
      test_mirror g;
      G.add_edge g v1 v2;
      G.add_edge g v1 v3;
      G.add_edge g v2 v1;
      G.add_edge g v2 v2;
      G.add_edge g v2 v2;
      test_mirror g;
      assert (G.nb_vertex g = V.v && G.nb_edges g = V.e);
      G.remove_vertex g v1;
      assert (G.nb_vertex g = 2 && G.nb_edges g = 1);
      G.remove_vertex g v2;
      assert (G.nb_vertex g = 1 && G.nb_edges g = 0);
      test_mirror g;
      G.clear g;
      assert (G.nb_vertex g = 0 && G.nb_edges g = 0)

  end

  let () =
    let module A = Make
      (Imperative.Digraph.ConcreteLabeled(Int)(Int))
      (struct let v = 3 let e = 4 end)
    in
    let module A = Make
      (Imperative.Graph.ConcreteLabeled(Int)(Int))
      (struct let v = 3 let e = 3 end)
    in
    let module A = Make
      (Imperative.Digraph.AbstractLabeled(Int)(Int))
      (struct let v = 3 let e = 4 end)
    in
    let module A = Make
      (Imperative.Graph.AbstractLabeled(Int)(Int))
      (struct let v = 3 let e = 3 end)
    in
    let module A = Make
      (Imperative.Digraph.Concrete(Int))
      (struct let v = 3 let e = 4 end)
    in
    let module A = Make
      (Imperative.Graph.Concrete(Int))
      (struct let v = 3 let e = 3 end)
    in
    let module A = Make
      (Imperative.Digraph.Abstract(Int))
      (struct let v = 3 let e = 4 end)
    in
    let module A = Make
      (Imperative.Graph.Abstract(Int))
      (struct let v = 3 let e = 3 end)
    in
    let module A = Make
      (Imperative.Digraph.ConcreteBidirectional(Int))
      (struct let v = 3 let e = 4 end)
    in
    let module A = Make
      (Imperative.Digraph.ConcreteBidirectionalLabeled(Int)(Int))
      (struct let v = 3 let e = 4 end)
    in
    ()

  (* Generic tests for persistent graphs *)
  module MakeP
    (G : Sig.P with type V.label = int)
    (V : sig val v: int val e: int end) =
  struct

    module O = Oper.P(G)
    let test_mirror g =
      let g' = O.mirror g in
      assert (G.nb_vertex g = G.nb_vertex g')

    let () =
      let g = G.empty in
      let v1 = G.V.create 1 in
      let v2 = G.V.create 2 in
      let v3 = G.V.create 3 in
      test_mirror g;
      let g = G.add_edge g v1 v2 in
      let g = G.add_edge g v1 v3 in
      let g = G.add_edge g v2 v1 in
      let g = G.add_edge g v2 v2 in
      let g = G.add_edge g v2 v2 in
      test_mirror g;
      assert (G.nb_vertex g = V.v && G.nb_edges g = V.e);
      let g = G.remove_vertex g v1 in
      assert (G.nb_vertex g = 2 && G.nb_edges g = 1);
      let g = G.remove_vertex g v2 in
      assert (G.nb_vertex g = 1 && G.nb_edges g = 0);
      test_mirror g

  end

  let () =
    let module A = MakeP
      (Persistent.Digraph.ConcreteLabeled(Int)(Int))
      (struct let v = 3 let e = 4 end)
    in
    let module A = MakeP
      (Persistent.Graph.ConcreteLabeled(Int)(Int))
      (struct let v = 3 let e = 3 end)
    in
    let module A = MakeP
      (Persistent.Digraph.AbstractLabeled(Int)(Int))
      (struct let v = 3 let e = 4 end)
    in
    let module A = MakeP
      (Persistent.Graph.AbstractLabeled(Int)(Int))
      (struct let v = 3 let e = 3 end)
    in
    let module A = MakeP
      (Persistent.Digraph.Concrete(Int))
      (struct let v = 3 let e = 4 end)
    in
    let module A = MakeP
      (Persistent.Graph.Concrete(Int))
      (struct let v = 3 let e = 3 end)
    in
    let module A = MakeP
      (Persistent.Digraph.Abstract(Int))
      (struct let v = 3 let e = 4 end)
    in
    let module A = MakeP
      (Persistent.Graph.Abstract(Int))
      (struct let v = 3 let e = 3 end)
    in
    let module A = MakeP
      (Persistent.Digraph.ConcreteBidirectional(Int))
      (struct let v = 3 let e = 4 end)
    in
    let module A = MakeP
      (Persistent.Digraph.ConcreteBidirectionalLabeled(Int)(Int))
      (struct let v = 3 let e = 4 end)
    in
    ()

  (* Generic tests for imperative concrete graphs with custom equality *)
  module Make_pair
    (G : Sig.I with type V.label = int * int)
    (V : sig val v: int val e: int end) =
  struct

    module O = Oper.I(G)
    let test_mirror g =
      if G.is_directed then begin (* TODO: remove *)
	let g' = O.mirror g in
	assert (G.nb_vertex g = G.nb_vertex g');
	G.iter_edges (fun v1 v2 -> assert (G.mem_edge g' v2 v1)) g;
	G.iter_edges (fun v1 v2 -> assert (G.mem_edge g v2 v1)) g';
	()
      end

    let g = G.create ()
    let () =
      let v1 = G.V.create (1, 0) in
      let v2 = G.V.create (2, 0) in
      let v3 = G.V.create (2, 1) in
      test_mirror g;
      G.add_edge g v1 v2;
      G.add_edge g v2 v1;
      G.add_edge g v1 v3;
      G.iter_vertex (fun v -> assert (snd (G.V.label v) = 0)) g;
      test_mirror g;
      assert (G.nb_vertex g = V.v && G.nb_edges g = V.e);
      G.remove_vertex g v3;
      assert (G.nb_vertex g = 1 && G.nb_edges g = 0);
      test_mirror g;
      G.clear g;
      assert (G.nb_vertex g = 0 && G.nb_edges g = 0)

  end

  let () =
    let module A = Make_pair
      (Imperative.Digraph.ConcreteLabeled(Pair)(Pair))
      (struct let v = 2 let e = 2 end)
    in
    let module A = Make_pair
      (Imperative.Graph.ConcreteLabeled(Pair)(Pair))
      (struct let v = 2 let e = 1 end)
    in
    let module A = Make_pair
      (Imperative.Digraph.Concrete(Pair))
      (struct let v = 2 let e = 2 end)
    in
    let module A = Make_pair
      (Imperative.Graph.Concrete(Pair))
      (struct let v = 2 let e = 1 end)
    in
    let module A = Make_pair
      (Imperative.Digraph.ConcreteBidirectional(Pair))
      (struct let v = 2 let e = 2 end)
    in
    let module A = Make_pair
      (Imperative.Digraph.ConcreteBidirectionalLabeled(Pair)(Pair))
      (struct let v = 2 let e = 2 end)
    in
    ()

  (* find_edge *)

  module Make2
    (G : Sig.I
     with type V.t = int and type E.label = int and type E.t = int * int * int)
    =
  struct

    let g = G.create ()

    let test_exn v1 v2 =
      assert (G.find_all_edges g v1 v2 = []);
      try
	let _ = G.find_edge g v1 v2 in
	assert false
      with Not_found ->
	()

    let () =
      let e1 = 1, 0, 2 in
      let e2 = 1, 1, 3 in
      let e2' = 1, 2, 3 in
      let e3 = 2, 2, 1 in
      G.add_edge_e g e1;
      G.add_edge_e g e2;
      G.add_edge_e g e2';
      G.add_edge_e g e3;
      G.add_edge_e g e3;
      assert (G.find_edge g 1 2 = e1);
      assert (List.length (G.find_all_edges g 1 3) = 2);
      test_exn 2 3;
      test_exn 2 4;
      test_exn 5 2;
      G.remove_vertex g 2;
      assert (G.nb_vertex g = 2 && G.nb_edges g = 2)

  end

  let () =
    let module D = Make2(Imperative.Digraph.ConcreteLabeled(Int)(Int)) in
    D.test_exn 3 1;
    let module G = Imperative.Graph.ConcreteLabeled(Int)(Int) in
    let module G2 = Make2(G) in
    assert (G.find_edge G2.g 3 1 = (3, 1, 1))

end

(********************************************)
(* Dijkstra                                 *)
(********************************************)

module Dijkstra = struct

  module TestDijkstra
    (G: Sig.G with type V.label = int and type E.label = int)
    (B: Builder.S with module G = G) =
  struct

    let g = B.empty ()
    let v1 = G.V.create 1
    let g = B.add_vertex g v1
    let v2 = G.V.create 2
    let g = B.add_vertex g v2
    let v3 = G.V.create 3
    let g = B.add_vertex g v3
    let v4 = G.V.create 4
    let g = B.add_vertex g v4
    let v5 = G.V.create 5
    let g = B.add_vertex g v5

    let g = B.add_edge_e g (G.E.create v1 10 v2)
    let g = B.add_edge_e g (G.E.create v2 50 v3)
    let g = B.add_edge_e g (G.E.create v1 30 v4)
    let g = B.add_edge_e g (G.E.create v1 100 v5)
    let g = B.add_edge_e g (G.E.create v3 10 v5)
    let g = B.add_edge_e g (G.E.create v4 20 v3)
    let g = B.add_edge_e g (G.E.create v4 60 v5)

    module Dij = Path.Dijkstra(G)(W(G.E))
    module Dfs = Traverse.Dfs(G)

    let test g i j w l =
      let p,w' = Dij.shortest_path g i j in
      assert (w' = w && List.length p = l)
    let test_not_found g i j =
      try let _ = Dij.shortest_path g i j in assert false with Not_found -> ()

    let () = test g v1 v5 60 3
    let () = test g v1 v1 0 0
    let () = if G.is_directed then test_not_found g v5 v1
    let () = assert (not (Dfs.has_cycle g))
    let gc = B.add_edge_e g (G.E.create v5 10 v1)
    let v6 = G.V.create 6
    let gc = B.add_vertex gc v6
    let () = if G.is_directed then test gc v1 v5 60 3
    let () = test gc v5 v1 10 1
    let () = test_not_found gc v1 v6

    let () = assert (Dfs.has_cycle gc)

  end

  (* Dijkstra on Persistent Directed Labeled Graphs *)

  module G = Persistent.Digraph.ConcreteLabeled(Int)(Int)
  module Test1 = TestDijkstra(G)(Builder.P(G))

  (* Dijkstra on Persistent Directed Abstract Labeled Graphs *)

  module G2 = Persistent.Digraph.AbstractLabeled(Int)(Int)
  module Test2 = TestDijkstra(G2)(Builder.P(G2))

  (* Dijkstra on Imperative Hashed Directed Labeled Graphs *)

  module G3 = Imperative.Digraph.ConcreteLabeled(Int)(Int)
  module Test3 = TestDijkstra(G3)(Builder.I(G3))

end

(********************************************)
(* Traversal                                *)
(********************************************)

module Traversal = struct

  module G = Imperative.Digraph.AbstractLabeled(Int)(Int)
  module Dfs = Traverse.Dfs(G)
  module Mark = Traverse.Mark(G)

  let g = G.create ()
  let newv g = let v = G.V.create 0 in G.add_vertex g v; v
  let v1 = newv g
  let v2 = newv g
  let v3 = newv g
  let v4 = newv g
  let v5 = newv g
  let add_edge g v1 l v2 = G.add_edge_e g (G.E.create v1 l v2)
  let () =
    add_edge g v1 10 v2;
    add_edge g v2 50 v3;
    add_edge g v1 30 v4;
    add_edge g v1 100 v5;
    add_edge g v3 10 v5;
    add_edge g v4 20 v3;
    add_edge g v4 60 v5
  let () = assert (not (Mark.has_cycle g) && not (Dfs.has_cycle g))
  let v6 = newv g
  let () = assert (not (Mark.has_cycle g) && not (Dfs.has_cycle g))
  let () = add_edge g v5 10 v1
  let () = assert (Mark.has_cycle g && Dfs.has_cycle g)

(* debug dfs / Cormen p 479 *)

  let g = G.create ()
  let newv i = let v = G.V.create i in G.add_vertex g v; v
  let u = newv 1
  let v = newv 2
  let w = newv 3
  let x = newv 4
  let y = newv 5
  let z = newv 6
  let edge a b = add_edge g a 0 b
  let () =
    edge u v; edge u x;
    edge v y;
    edge w y; edge w z;
    edge x v;
    edge y x;
    edge z z
  open Format
  let pre v = printf "pre %d@." (G.V.label v)
  let post v = printf "post %d@." (G.V.label v)
  let () = printf "iter:@."; Dfs.iter_component ~pre ~post g w
  let () = printf "prefix:@."; Dfs.prefix_component pre g w
  let () =
    printf "step:@.";
    let rec visit it =
      let v = Dfs.get it in
      printf "visit %d@." (G.V.label v);
      visit (Dfs.step it)
    in
    try visit (Dfs.start g) with Exit -> ()

end

(********************************************)
(* Ford-Fulkerson and Goldberg              *)
(********************************************)

module FF_Goldberg = struct

  module G = Persistent.Digraph.ConcreteLabeled(Int)(Int)

  let add_edge g v1 l v2 = G.add_edge_e g (G.E.create v1 l v2)
  let g = G.empty
  let g = add_edge g 1 16 2
  let g = add_edge g 1 13 3
  let g = add_edge g 2 10 3
  let g = add_edge g 3 4 2
  let g = add_edge g 2 12 4
  let g = add_edge g 4 9 3
  let g = add_edge g 3 14 5
  let g = add_edge g 5 7 4
  let g = add_edge g 4 20 6
  let g = add_edge g 5 4 6

  module F = struct
    type label = int
    type t = int
    let max_capacity x = x
    let min_capacity _ = 0
    let flow _ = 0
    let add = (+)
    let sub = (-)
    let compare = compare
    let zero = 0
  end

  module FF = Flow.Ford_Fulkerson(G)(F)
  module Gold = Flow.Goldberg(G)(F)

  let () =
    assert (snd (FF.maxflow g 1 6) = 23);
    assert (snd (Gold.maxflow g 1 6) = 23);
    assert (snd (FF.maxflow g 1 1) = 0);
    assert (snd (Gold.maxflow g 1 1) = 0)

  module G2 =
    Persistent.Digraph.ConcreteLabeled
      (Int)
      (struct include Util.OTProduct(Int)(Int) let default = 0, 0 end)

  let add_edge g v1 l v2 = G2.add_edge_e g (G2.E.create v1 l v2)
  let g = G2.empty
  let g = add_edge g 1 (1, 1) 2
  let g = add_edge g 1 (3, 0) 3
  let g = add_edge g 2 (1, 1) 3
  let g = add_edge g 3 (1, 0) 2
  let g = add_edge g 2 (3, 0) 4
  let g = add_edge g 3 (1, 1) 4

  module F2 = struct
    type label = int * int
    type t = int
    let max_capacity = fst
    let min_capacity _ = 0
    let flow = snd
    let add = (+)
    let sub = (-)
    let compare = compare
    let zero = 0
  end

  module FF2 = Flow.Ford_Fulkerson(G2)(F2)
  module Gold2 = Flow.Goldberg(G2)(F2)

  let () =
    assert (snd (FF2.maxflow g 1 4) = 2);
    assert (snd (Gold2.maxflow g 1 4) = 2)

end

(********************************************)
(* Neighbourhood                            *)
(********************************************)

module Neighbourhood = struct

  module G = Graph.Imperative.Graph.Concrete(Int)
  open G

  let g = create ()
  let add = add_edge g
  let () =
    add 1 2;
    add 1 3;
    add 1 4;
    add 2 5;
    add 3 5;
    add 4 5;
    add 5 6

  module N = Oper.Neighbourhood(G)
  module V = N.Vertex_Set
  let s2 = V.add 1 (V.singleton 5)
  let () = assert (V.equal (N.set_from_vertex g 2) s2)
  let s25 = V.add 1 (V.add 3 (V.add 4 (V.singleton 6)))
  let () = assert (V.equal (N.set_from_vertices g [ 2; 5 ]) s25)

end

(********************************************)
(* Minimal seperators                       *)
(********************************************)

module Minsep = struct

  module P = struct

    module G = Graph.Persistent.Graph.Concrete(Int)
    open G

    let g = empty
    let g = add_edge g 1 2
    let g = add_edge g 1 3
    let g = add_edge g 1 4
    let g = add_edge g 2 5
    let g = add_edge g 3 5
    let g = add_edge g 4 5
    let g = add_edge g 5 6

    module M = Minsep.P(G)
    module S = M.Vertex_Set
    module VS = M.VSetset
    let s5 = S.singleton 5
    let s15 = S.add 1 s5
    let s234 = S.add 2 (S.add 3 (S.singleton 4))
    let bigs = VS.add s5 (VS.add s15 (VS.singleton s234))
    let () = assert (VS.equal (M.set_of_allminsep g) bigs)

  end

  module I = struct

    module G = Graph.Imperative.Graph.Abstract(struct type t = unit end)
    open G

    let g = create ()
    let v1 = V.create ()
    let v2 = V.create ()
    let v3 = V.create ()
    let v4 = V.create ()
    let v5 = V.create ()
    let v6 = V.create ()
    let add = add_edge g
    let () =
      add v1 v2;
      add v1 v3;
      add v1 v4;
      add v2 v5;
      add v3 v5;
      add v4 v5;
      add v5 v6

    module M = Minsep.I(G)
    module S = M.Vertex_Set
    module VS = M.VSetset
    let s5 = S.singleton v5
    let s15 = S.add v1 s5
    let s234 = S.add v2 (S.add v3 (S.singleton v4))
    let bigs = VS.add s5 (VS.add s15 (VS.singleton s234))
    let () =
      let _ =  G.copy g in
      assert (VS.equal (M.set_of_allminsep g) bigs)

  end

end

(********************************************)
(* Checking signature                       *)
(********************************************)

(* check that signature [Sig_pack.S] (which is manually expanded) does not
   forget anything *)
module type RightSigPack = sig
  include Sig.IM with type V.label = int and type E.label = int
  val find_vertex : t -> int -> V.t
  include Oper.S with type g = t
  module Dfs : sig
    val iter : ?pre:(V.t -> unit) ->
               ?post:(V.t -> unit) -> t -> unit
    val prefix : (V.t -> unit) -> t -> unit
    val postfix : (V.t -> unit) -> t -> unit

    val iter_component :
               ?pre:(V.t -> unit) ->
               ?post:(V.t -> unit) -> t -> V.t -> unit
    val prefix_component : (V.t -> unit) -> t -> V.t -> unit
    val postfix_component : (V.t -> unit) -> t -> V.t -> unit

    val has_cycle : t -> bool
  end
  module Bfs : sig
    val iter : (V.t -> unit) -> t -> unit
    val iter_component : (V.t -> unit) -> t -> V.t -> unit
  end
  module Marking : sig
    val dfs : t -> unit
    val has_cycle : t -> bool
  end
  module Classic : sig
    val divisors : int -> t
    val de_bruijn : int -> t
    val vertex_only : int -> t
    val full : ?self:bool -> int -> t
  end
  module Rand : sig
    val graph : ?loops:bool -> v:int -> e:int -> unit -> t
    val labeled :
      (V.t -> V.t -> E.label) ->
	?loops:bool -> v:int -> e:int -> unit -> t
  end
  module Components : sig
    val scc : t -> int*(V.t -> int)
    val scc_array : t -> V.t list array
    val scc_list : t -> V.t list list
  end
  val shortest_path : t -> V.t -> V.t -> E.t list * int
  val ford_fulkerson : t -> V.t -> V.t -> (E.t -> int) * int
  val goldberg : t -> V.t -> V.t -> (E.t -> int) * int
  val dot_output : t -> string -> unit
end

module TestSigPack : RightSigPack = struct
  include Pack.Digraph
  type g = t
end

include Test_clique

(*
Local Variables:
compile-command: "make -C .. check"
End:
*)
