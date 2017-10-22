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

module type GM = sig
  val is_directed : bool
  type t
  val nb_vertex : t -> int
  module V : Sig.COMPARABLE
  val out_degree : t -> V.t -> int
  val iter_vertex : (V.t -> unit) -> t -> unit
  val fold_vertex : (V.t -> 'a -> 'a) -> t  -> 'a -> 'a
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  val fold_succ : (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
  module Mark : sig
    val get : V.t -> int
    val set : V.t -> int -> unit
  end
end

(** Graph coloring with marking.
    Only applies to imperative graphs with marks. *)
module Mark(G : GM) = struct

  exception NoColoring

  module Bfs = Traverse.Bfs(G)

  let coloring g k =
    (* first step: we eliminate vertices with less than [k] successors *)
    let stack = Stack.create () in
    let nb_to_color = ref (G.nb_vertex g) in
    let count = ref 1 in
    while !count > 0 do
      count := 0;
      let erase v = incr count; G.Mark.set v (k+1); Stack.push v stack in
      G.iter_vertex
        (fun v -> if G.Mark.get v = 0 && G.out_degree g v < k then erase v)
        g;
      (*Format.printf "eliminating %d nodes@." !count;*)
      nb_to_color := !nb_to_color - !count
    done;
    (* second step: we k-color the remaining of the graph *)
    (* [try_color v i] tries to assign color [i] to vertex [v] *)
    let try_color v i =
      G.Mark.set v i;
      G.iter_succ (fun w -> if G.Mark.get w = i then raise NoColoring) g v
    in
    let uncolor v = G.Mark.set v 0 in
    if !nb_to_color > 0 then begin
      let rec iterate iter =
        let v = Bfs.get iter in
        let m = G.Mark.get v in
        if m > 0 then
          iterate (Bfs.step iter)
        else begin
          for i = 1 to k do
            try try_color v i; iterate (Bfs.step iter)
            with NoColoring -> ()
          done;
          uncolor v;
          raise NoColoring
        end
      in
      try iterate (Bfs.start g) with Exit -> ()
    end;
    (* third step: we color the eliminated vertices, in reverse order *)
    Stack.iter
      (fun v ->
         try
           for i = 1 to k do
             try try_color v i; raise Exit with NoColoring -> ()
           done;
           raise NoColoring (* it may still fail on a self edge v->v *)
         with Exit -> ())
      stack

end

(** Graph coloring for graphs without marks: we use an external hashtbl *)

module type G = sig
  val is_directed : bool
  type t
  val nb_vertex : t -> int
  module V : Sig.COMPARABLE
  val out_degree : t -> V.t -> int
  val iter_vertex : (V.t -> unit) -> t -> unit
  val fold_vertex : (V.t -> 'a -> 'a) -> t  -> 'a -> 'a
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  val fold_succ : (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
end

module Make(G: G) = struct

  module H = Hashtbl.Make(G.V)

  let coloring g k =
    let h = H.create 97 in
    let module M =
      Mark(struct
        include G
        module Mark = struct
          let get v = try H.find h v with Not_found -> 0
          let set v n = H.replace h v n
        end
      end )
    in
    M.coloring g k;
    h

end
