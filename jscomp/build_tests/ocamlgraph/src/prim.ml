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

module type G = sig
  type t
  module V : Sig.COMPARABLE
  module E : sig
    type t
    type label
    val label : t -> label
    val dst : t -> V.t
    val src : t -> V.t
    val compare : t -> t -> int
  end
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_edges_e : (E.t -> unit) -> t ->  unit
  val iter_succ_e : (E.t -> unit) -> t -> V.t -> unit
end

module Make
    (G: G)
    (W: Sig.WEIGHT with type edge = G.E.t) =
struct
  open G.E
  module H = Hashtbl.Make(G.V)

  module Elt = struct
    type t = W.t * G.V.t

    (* weights are compared first, and minimal weights come first in the
       queue *)
    let compare (w1,v1) (w2,v2) =
      let cw = W.compare w2 w1 in
      if cw != 0 then cw else G.V.compare v1 v2
  end

  module Q = Heap.Imperative(Elt)

  let spanningtree_from g r =
    let visited = H.create 97 in
    let key = H.create 97 in
    let q = Q.create 17 in
    Q.add q (W.zero, r);
    while not (Q.is_empty q) do
      let (_,u) = Q.pop_maximum q in
      if not (H.mem visited u) then begin
        H.add visited u ();
        G.iter_succ_e (fun e ->
            let v = dst e in
            if not (H.mem visited v) then begin
              let wuv = W.weight e in
              let improvement =
                try W.compare wuv (fst (H.find key v)) < 0 with Not_found -> true
              in
              if improvement then begin
                H.replace key v (wuv, e);
                Q.add q (wuv, v)
              end;
            end) g u
      end
    done;
    H.fold (fun _ (_, e) acc -> e :: acc) key []

  let spanningtree g =
    let r = ref None in
    try
      G.iter_vertex (fun v -> r := Some v; raise Exit) g;
      invalid_arg "spanningtree"
    with Exit ->
    match !r with
    | None -> assert false
    | Some r -> spanningtree_from g r

end
