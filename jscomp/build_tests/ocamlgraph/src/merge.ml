(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2012                                               *)
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

module type S = sig
  type graph
  type vertex
  type edge
  type edge_label
  val merge_vertex: graph -> vertex list -> graph
  val merge_edges_e:
    ?src:vertex -> ?dst:vertex -> graph -> edge list -> graph
  val merge_edges_with_label:
    ?src:vertex -> ?dst:vertex -> ?label:edge_label -> graph -> edge_label
    -> graph
  val merge_isolabelled_edges: graph -> graph
  val merge_ends: ?strict:bool -> ?specified_vertex:vertex -> graph -> graph
  val merge_starts: ?strict:bool -> ?specified_vertex:vertex -> graph -> graph
  val merge_scc:
    ?loop_killer:bool -> ?specified_vertex:(vertex list -> vertex) -> graph ->
    graph
end

module B(B: Builder.S) = struct

  type graph = B.G.t
  type vertex = B.G.vertex
  type edge = B.G.edge
  type edge_label = B.G.E.label

  let mem x ec = List.exists (fun y -> B.G.V.equal x y) ec

  let identify x ec = match ec with
    | [] -> false , x
    | y :: ec -> if mem x ec then true, y else false, x

  let identify_extremities g vl =
    let f e accu =
      let sx, x = identify (B.G.E.src e) vl in
      let sy, y = identify (B.G.E.dst e) vl in
      if sx || sy
      then B.G.E.(create x (label e) y) :: accu
      else accu
    in
    B.G.fold_edges_e f g []

  (* – former buggy version – the case where v is neither the source nor the
     destination of some arrow was not taken into account, so that vertices were
     just removed

     let merge_vertex g vl = match vl with
     | [] -> g
     | _ :: vl' ->
     let to_be_added = identify_extremities g vl in
     let g = List.fold_left B.remove_vertex g vl' in
     List.fold_left B.add_edge_e g to_be_added
  *)

  let merge_vertex g vl = match vl with
    | [] -> g
    | v :: vl' ->
      let to_be_added = identify_extremities g vl in
      let g = List.fold_left B.remove_vertex g vl' in
      if to_be_added = []
      then B.add_vertex g v
      else List.fold_left B.add_edge_e g to_be_added

  let merge_edges_e ?src ?dst g el = match el with
    | e :: el' ->
      let el' = List.filter (B.G.mem_edge_e g) el' in
      if el' <> []
      then
        (let el = e :: el' in
         let extremities e = B.G.E.(src e, dst e) in
         let sources , destinations = List.split (List.map extremities el) in
         let remove accu e =
           try B.remove_edge_e accu e
           with Invalid_argument _ -> g in
         let g = List.fold_left remove g el in
         if List.exists (fun v -> mem v destinations) sources then
           let v = match src with
             | None ->
               (match dst with
                | None -> List.hd sources
                | Some w -> w)
             | Some v -> v in
           let g = merge_vertex g (v :: sources @ destinations) in
           B.add_edge_e g B.G.E.(create v (label e) v)
         else
           let v = match src with None -> List.hd sources | Some v -> v in
           let w = match src with
             | None -> List.hd destinations
             | Some w -> w in
           let g = merge_vertex g sources in
           let g = merge_vertex g destinations in
           B.add_edge_e g B.G.E.(create v (label e) w))
      else g
    | [] -> g

  let merge_edges_with_label ?src ?dst ?label g l =
    let update_label e = match label with
      | None -> e
      | Some l -> B.G.E.(create (src e) l (dst e))
    in
    let collect_edge e accu =
      if B.G.E.label e = l
      then (update_label e) :: accu
      else accu
    in
    let edges_to_be_merged = B.G.fold_edges_e collect_edge g [] in
    merge_edges_e ?src ?dst g edges_to_be_merged

  (* To deduce a comparison function on labels from a comparison function on
     edges *)

  let compare_label g =
    try
      let default_vertex =
        let a_vertex_of_g = ref None in
        (try B.G.iter_vertex (fun v -> a_vertex_of_g := Some v ; raise Exit) g
         with Exit -> ());
        match !a_vertex_of_g with
        | Some v -> v
        | None -> raise Exit (*hence g is empty*) in
      fun l1 l2 ->
        let e1 = B.G.E.create default_vertex l1 default_vertex in
        let e2 = B.G.E.create default_vertex l2 default_vertex in
        B.G.E.compare e1 e2
    with Exit -> (fun _ _ -> 0)

  let merge_isolabelled_edges g =
    let module S = Set.Make(B.G.V) in
    let do_meet s1 s2 = S.exists (fun x -> S.mem x s2) s1 in
    let module M =
      (* TODO: using [compare] here is really suspicious ...
         DONE – yet not so clean *)
      Map.Make(struct type t = B.G.E.label let compare = compare_label g end)
    in
    let accumulating e accu =
      let l = B.G.E.label e in
      try
        let s , d = M.find l accu in
        let s , d = B.G.E.(S.add (src e) s , S.add (dst e) d) in
        M.add l (s, d) accu
      with Not_found ->
        M.add l B.G.E.(S.singleton (src e), S.singleton (dst e)) accu
    in
    let to_be_identified = B.G.fold_edges_e accumulating g M.empty in
    let gathering _ (s, d) accu =
      let to_be_gathered, others = List.partition (do_meet s) accu in
      let accu =
        List.fold_left (fun accu x -> S.union accu x) s to_be_gathered
        :: others
      in
      let to_be_gathered , others = List.partition (do_meet d) accu in
      List.fold_left (fun accu x -> S.union accu x) d to_be_gathered :: others
    in
    let to_be_identified = M.fold gathering to_be_identified [] in
    List.fold_left
      (fun accu s -> merge_vertex accu (S.elements s))
      g
      to_be_identified

  let merge_ends ?(strict=false) ?specified_vertex g =
    let accumulator v accu =
      if
        let out_d = B.G.out_degree g v in
        out_d = 0 ||
        ((not strict) && out_d = List.length (B.G.find_all_edges g v v))
      then v :: accu
      else accu
    in
    let ends = B.G.(fold_vertex accumulator g []) in
    let to_be_merged =
      match specified_vertex with
      | Some v -> v :: ends
      | None -> ends
    in
    merge_vertex g to_be_merged

  let merge_starts ?(strict=false) ?specified_vertex g =
    let accumulator v accu =
      if
        let in_d = B.G.in_degree g v in
        in_d = 0 ||
        ((not strict) && in_d = List.length (B.G.find_all_edges g v v))
      then v :: accu
      else accu
    in
    let starts = B.G.(fold_vertex accumulator g []) in
    let to_be_merged =
      match specified_vertex with
      | Some v -> v :: starts
      | None -> starts
    in
    merge_vertex g to_be_merged

  let merge_scc ?(loop_killer=false) ?specified_vertex g =
    let module C = Components.Make(B.G) in
    let components = C.scc_list g in
    let alter accu to_be_identified =
      let to_be_identified =
        match specified_vertex with
        | None -> to_be_identified
        | Some f -> (f to_be_identified) :: to_be_identified in
      let v = List.hd to_be_identified in
      let accu = merge_vertex accu to_be_identified in
      if loop_killer
      then B.remove_edge accu v v
      else accu in
    List.fold_left alter g components

end

module P(G: Sig.P) = B(Builder.P(G))

module I(G: Sig.I) = struct
  include B(Builder.I(G))
  let merge_vertex g vl = ignore (merge_vertex g vl)
  let merge_edges_e ?src ?dst g el = ignore (merge_edges_e ?src ?dst g el)
  let merge_edges_with_label ?src ?dst ?label g l =
    ignore (merge_edges_with_label ?src ?dst ?label g l)
  let merge_isolabelled_edges g = ignore (merge_isolabelled_edges g)
  let merge_ends ?strict ?specified_vertex g =
    ignore (merge_ends ?strict ?specified_vertex g)
  let merge_starts ?strict ?specified_vertex g =
    ignore (merge_starts ?strict ?specified_vertex g)
  let merge_scc ?loop_killer ?specified_vertex g =
    ignore (merge_scc ?loop_killer ?specified_vertex g)
end

(*
Local Variables:
compile-command: "make -C .."
End:
*)
