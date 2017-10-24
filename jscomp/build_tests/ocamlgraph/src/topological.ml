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
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
end

module Make(G: G) = struct

  module Scc = Components.Make(G)

  let fold f g acc =
    (* build the graph of strongly-connected components *)
    let n, scc = Scc.scc g in
    let vertices = Array.make n [] in
    let edges = Array.make n [] in
    let degree = Array.make n 0 in (* in-degree *)
    let add_vertex x =
      let ix = scc x in
      vertices.(ix) <- x :: vertices.(ix);
      let add_edge y =
        let iy = scc y in
        if ix <> iy then begin
          edges.(ix) <- iy :: edges.(ix);
          degree.(iy) <- degree.(iy) + 1
        end
      in
      G.iter_succ add_edge g x
    in
    G.iter_vertex add_vertex g;
    (* standard topological sort on a DAG *)
    let todo = Queue.create () in
    let rec walk acc =
      if Queue.is_empty todo then
        acc
      else
        let i = Queue.pop todo in
        let acc = List.fold_right f vertices.(i) acc in
        List.iter
          (fun j ->
             let d = degree.(j) in
             assert (d > 0); (* no back edge *)
             if d = 1 then Queue.push j todo else degree.(j) <- d-1)
          edges.(i);
        walk acc
    in
    for i = 0 to n-1 do if degree.(i) = 0 then Queue.push i todo done;
    walk acc

  let iter f g = fold (fun v () -> f v) g ()

end

module Make_stable(G: sig include G val in_degree : t -> V.t -> int end) =
struct

  module H = Hashtbl.Make(G.V)
  module C = Path.Check(G)

  let choose ~old (v, n) =
    let l, min = old in
    if n = min then v :: l, n
    else if n < min then [ v ], n
    else old

  module Q = struct
    module S = Set.Make(G.V)
    let create () = ref S.empty
    let push v s = s := S.add v !s
    let pop s =
      let r = S.min_elt !s in
      s := S.remove r !s;
      r
    let is_empty s = S.is_empty !s
    let choose ~old new_ =
      let l, n = choose ~old new_ in
      List.sort G.V.compare l, n
  end

  (* in case of multiple cycles, choose one vertex in a cycle which
     does not depend of any other. *)
  let find_top_cycle checker vl =
    (* choose [v] if each other vertex [v'] is in the same cycle
       (a path from v to v') or is in a separate component
       (no path from v' to v).
       So, if there is a path from v' to without any path from v to v',
       discard v. *)
    let on_top_cycle v =
      List.for_all
        (fun v' ->
           G.V.equal v v' ||
           C.check_path checker v v' || not (C.check_path checker v' v))
        vl
    in
    List.filter on_top_cycle vl

  let fold f g acc =
    let checker = C.create g in
    let degree = H.create 97 in
    let todo = Q.create () in
    let push x =
      H.remove degree x;
      Q.push x todo
    in
    let rec walk acc =
      if Q.is_empty todo then
        (* let's find any node of minimal degree *)
        let min, _ =
          H.fold (fun v d old -> Q.choose ~old (v, d)) degree ([], max_int)
        in
        match min with
        | [] -> acc
        | _ ->
          let vl = find_top_cycle checker min in
          List.iter push vl;
          (* let v = choose_independent_vertex checker min in push v; *)
          walk acc
      else
        let v = Q.pop todo in
        let acc = f v acc in
        G.iter_succ
          (fun x->
             try
               let d = H.find degree x in
               if d = 1 then push x else H.replace degree x (d-1)
             with Not_found ->
               (* [x] already visited *)
               ())
          g v;
        walk acc
    in
    G.iter_vertex
      (fun v ->
         let d = G.in_degree g v in
         if d = 0 then Q.push v todo
         else H.add degree v d)
      g;
    walk acc

  let iter f g = fold (fun v () -> f v) g ()

end


(*
Local Variables:
compile-command: "make -C .."
End:
*)
