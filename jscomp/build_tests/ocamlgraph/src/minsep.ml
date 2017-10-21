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
  val succ: t -> V.t -> V.t list
  val iter_succ: (V.t -> unit) -> t -> V.t -> unit
  val fold_succ: (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
  val iter_vertex: (V.t -> unit) -> t -> unit
  val fold_vertex: (V.t -> 'a -> 'a) -> t -> 'a -> 'a
end

module type MINSEP = sig
  module G : G
  module Vertex_Set : Set.S with type elt = G.V.t
  module VSetset : Set.S with type elt = Vertex_Set.t
  val allminsep : G.t -> Vertex_Set.t list
  val list_of_allminsep : G.t -> G.V.t list list
  val set_of_allminsep : G.t -> VSetset.t
end

module Make
    (G : sig
       include G
       val cc: t -> V.t list -> V.t list list
       (** compute the set of connected components of G(V \ l) *)
     end) =
struct

  module N = Oper.Neighbourhood(G)
  module Vertex_Set: Set.S with type t = N.Vertex_Set.t
                            and type elt = G.V.t = N.Vertex_Set
  module VSetset = Set.Make(N.Vertex_Set)
  (* Use [N.Vertex_Set] instead of [Vertex_Set] in order to avoid an error with
     ocamldoc 4.02. However this change requires to add extra type annotations
     to module [Vertex_Set] above in order to prevent compilation error with
     OCaml <= 4.0 :-(. *)

  let initialisation g =
    let cc = G.cc g in
    let neighbourhood = N.list_from_vertex g in
    let neighbourhoods = N.set_from_vertices g in
    G.fold_vertex
      (fun v s ->
         List.fold_left
           (fun s l -> neighbourhoods l :: s)
           s (cc (v :: neighbourhood v)))
      g []

  let generation g =
    let neighbourhood = N.list_from_vertex g in
    let neighbourhoods = N.set_from_vertices g in
    let cc = G.cc g in
    let rec gen_aux seen bigs = function
      | [] -> bigs
      | s :: tl ->
        let l = Vertex_Set.elements s in
        let seen = VSetset.add s seen in
        let bigs, tl =
          Vertex_Set.fold
            (fun v _ ->
               let add_neighbourhoods (bigs, tl) l =
                 let s = neighbourhoods l in
                 s :: bigs, if VSetset.mem s seen then tl else s :: tl
               in
               List.fold_left
                 add_neighbourhoods
                 (bigs, tl) (cc (l @ neighbourhood v)))
            s (bigs, tl)
        in
        gen_aux seen bigs tl
    in
    fun bigs -> gen_aux VSetset.empty bigs bigs

  let allminsep g = generation g (initialisation g)

  let set_of_allminsep g =
    List.fold_left
      (fun bigs s -> VSetset.add s bigs) VSetset.empty (allminsep g)

  let list_of_allminsep g = List.map Vertex_Set.elements (allminsep g)

end

module P(G : sig include G val remove_vertex : t -> V.t -> t end) = struct
  module G = G
  include Make(struct
      include G
      let cc =
        let module CC = Components.Make(G) in
        fun g l ->
          let g = List.fold_left remove_vertex g l in
          CC.scc_list g
    end)
end

module I(G : sig
    include G
    module Mark : Sig.MARK with type graph = t and type vertex = V.t
  end) =
struct
  module G = G
  include Make(struct
      include G
      let cc =
        let module CC =
          Components.Make
            (struct
              include G
              let iter_vertex f =
                iter_vertex (fun v -> if Mark.get v=0 then f v)
            end)
        in
        fun g l ->
          G.Mark.clear g;
          List.iter (fun v -> G.Mark.set v 1) l;
          CC.scc_list g
    end)
end
