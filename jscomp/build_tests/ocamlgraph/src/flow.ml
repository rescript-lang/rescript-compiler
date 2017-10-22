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

module type FLOW = sig
  type t
  type label
  val max_capacity : label -> t
  val min_capacity : label -> t
  val flow : label -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val zero : t
  val compare : t -> t -> int
end

module type G_GOLDBERG = sig
  type t
  module V : Sig.COMPARABLE
  module E : Sig.EDGE with type vertex = V.t
  val nb_vertex : t -> int
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_edges_e : (E.t -> unit) -> t -> unit
  val fold_succ_e : (E.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
  val fold_pred_e : (E.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
end

module Goldberg(G: G_GOLDBERG)(F: FLOW with type label = G.E.label) =
struct

  module V = Hashtbl.Make(G.V)
  module E = Hashtbl.Make(Util.HTProduct(G.V)(G.V))
  module Se = Set.Make(G.E)
  module Sv = Set.Make(G.V)

  let excedents = V.create 97
  let hauteur = V.create 97
  let flot = E.create 97

  let fold_booleen f = List.fold_left (fun r x->(f x) || r) false

  let capacite_restante _ e =
    F.sub (F.max_capacity (G.E.label e)) (E.find flot (G.E.src e, G.E.dst e))

  let reste_excedent x = F.compare (V.find excedents x) F.zero > 0

  let flux_et_reflux g x =
    let s =
      G.fold_succ_e
        (fun e s->
           if F.compare
               (capacite_restante g e) (F.min_capacity (G.E.label e))
              > 0
           then e::s else s)
        g x []
    in
    G.fold_pred_e
      (fun e s ->
         if F.compare
             (E.find flot (G.E.src e, G.E.dst e)) (F.min_capacity (G.E.label e))
            > 0
         then (G.E.create (G.E.dst e) (G.E.label e) (G.E.src e))::s else s)
      g x s

  let pousser g e l =
    let x, y = G.E.src e, G.E.dst e in
    let ex = V.find excedents x in
    let cxy = capacite_restante g e in
    if F.compare ex F.zero > 0 &&
       F.compare cxy (F.min_capacity (G.E.label e)) > 0 &&
       V.find hauteur x = (V.find hauteur y + 1)
    then
      let d = if F.compare ex cxy < 0 then ex else cxy in
      let fxy = E.find flot (x,y) in
      let ex = V.find excedents x in
      let ey = V.find excedents y in
      E.replace flot (x,y) (F.add fxy d);
      E.replace flot (y,x) (F.sub F.zero (F.add fxy d));
      V.replace excedents x (F.sub ex d);
      V.replace excedents y (F.add ey d);
      if reste_excedent x then l:=Sv.add x !l;
      if reste_excedent y then l:=Sv.add y !l;
      true
    else
      (if F.compare ex F.zero > 0 then l:=Sv.add x !l;
       false)

  let elever g p x =
    let u = flux_et_reflux g x in
    reste_excedent x
    && not (G.V.equal x p)
    &&
    List.for_all
      (fun e -> (V.find hauteur (G.E.src e)) <= (V.find hauteur (G.E.dst e))) u
    &&
    (let min =
       List.fold_left (fun m e -> min (V.find hauteur (G.E.dst e)) m) max_int u
     in
     V.replace hauteur x (1+min);
     true)

  let init_preflot g s _ =
    G.iter_vertex (fun x -> V.add excedents x F.zero; V.add hauteur x 0) g;
    G.iter_edges_e
      (fun e ->
         let x,y = G.E.src e, G.E.dst e in
         E.add flot (x,y) (F.flow (G.E.label e));
         E.add flot (y,x) (F.sub F.zero (F.flow (G.E.label e))))
      g;
    V.add hauteur s (G.nb_vertex g);
    G.fold_succ_e
      (fun e l ->
         let y = G.E.dst e in
         let c = F.max_capacity (G.E.label e) in
         E.add flot (s,y) c;
         E.add flot (y,s) (F.sub F.zero c);
         V.add excedents y c;
         y::l)
      g s []

  let maxflow g s p =
    let push_and_pull l x =
      G.fold_succ_e (fun e r->pousser g e l || r) g x false
      || G.fold_pred_e (fun e r->pousser g e l || r) g x false
    in
    let todo = ref (init_preflot g s p) in
    while
      (fold_booleen (elever g p) !todo) ||
      (let l = ref Sv.empty in
       let r = fold_booleen (push_and_pull l) !todo in
       todo:=Sv.elements !l; r)
    do () done;
    let flot_max =
      G.fold_pred_e (fun e f -> F.add (E.find flot (G.E.src e,p)) f) g p F.zero
    in
    let flot_init =
      G.fold_pred_e (fun e f -> F.add (F.flow (G.E.label e)) f) g p F.zero
    in
    let f e =
      let x,y = G.E.src e, G.E.dst e in
      try E.find flot (x,y)
      with Not_found -> F.flow (G.E.label e)
    in
    f, F.sub flot_max flot_init
end

(*****************************************************************************)

module type G_FORD_FULKERSON = sig
  type t
  module V : Sig.HASHABLE
  module E : sig
    type t
    type label
    val src : t -> V.t
    val dst : t -> V.t
    val label : t -> label
  end
  val iter_succ_e : (E.t -> unit) -> t -> V.t -> unit
  val iter_pred_e : (E.t -> unit) -> t -> V.t -> unit
end

module Ford_Fulkerson
    (G: G_FORD_FULKERSON)
    (F: FLOW with type label = G.E.label) =
struct

  (* redefinition of F *)
  module F = struct
    include F

    type u =
      | Flow of F.t
      | Infinity

    let min x y = match x, y with
      | Flow _, Infinity -> x
      | Flow fx, Flow fy when F.compare fx fy < 0 -> x
      | (Infinity, _) | (Flow _, Flow _) -> y
  end

  module Mark = struct
    module H = Hashtbl.Make(G.V)
    type mark = Plus | Minus

    let marked = H.create 97
    let unvisited = Queue.create ()

    let clear () = H.clear marked

    let mem = H.mem marked

    let set s e tag =
      assert (not (mem s));
      H.add marked s (e, tag);
      Queue.add s unvisited

    let get s : G.E.t * mark =
      let e, tag = H.find marked s in
      (match e with None -> assert false | Some e -> e), tag

    let next () = Queue.pop unvisited
  end

  module Result = struct
    module H =
      Hashtbl.Make
        (struct
          open G
          type t = E.t
          module U = Util.HTProduct(V)(V)
          let equal e1 e2 = U.equal (E.src e1, E.dst e1) (E.src e2, E.dst e2)
          let hash e = U.hash (E.src e, E.dst e)
        end)

    let create () = H.create 97

    let find = H.find

    let flow r e =
      try
        find r e
      with Not_found ->
        let f = F.flow (G.E.label e) in
        H.add r e f;
        f

    let change op r e f =
      try
        H.replace r e (op (find r e) f);
      with Not_found ->
        assert false

    let grow = change F.add
    let reduce = change F.sub
  end

  let is_full r e =
    F.compare (F.max_capacity (G.E.label e)) (Result.flow r e) = 0

  let is_empty r e =
    F.compare (F.min_capacity (G.E.label e)) (Result.flow r e) = 0

  let set_flow r s t a =
    let rec loop t =
      if not (G.V.equal s t) then
        let e, tag = Mark.get t in
        match tag with
        | Mark.Plus -> Result.grow r e a; loop (G.E.src e)
        | Mark.Minus -> Result.reduce r e a; loop (G.E.dst e)
    in
    loop t

  let grow_flow r s t a =
    let rec loop u b =
      if G.V.equal s u then begin
        match b with
        | F.Infinity -> (* source = destination *)
          assert (G.V.equal s t);
          a
        | F.Flow f ->
          set_flow r s t f;
          F.add a f
      end else
        let e, tag = Mark.get u in
        let l = G.E.label e in
        match tag with
        | Mark.Plus ->
          loop
            (G.E.src e)
            (F.min b (F.Flow (F.sub (F.max_capacity l) (Result.flow r e))))
        | Mark.Minus ->
          loop
            (G.E.dst e)
            (F.min b (F.Flow (F.sub (Result.flow r e) (F.min_capacity l))))
    in
    loop t F.Infinity

  let maxflow g s t =
    let r = Result.create () in
    let succ s =
      G.iter_succ_e
        (fun e ->
           assert (G.V.equal s (G.E.src e));
           let t = G.E.dst e in
           if not (Mark.mem t || is_full r e) then
             Mark.set t (Some e) Mark.Plus)
        g s
    in
    let pred s =
      G.iter_pred_e
        (fun e ->
           assert (G.V.equal s (G.E.dst e));
           let t = G.E.src e in
           if not (Mark.mem t || is_empty r e) then
             Mark.set t (Some e) Mark.Minus)
        g s
    in
    let internal_loop a =
      try
        while true do let s = Mark.next () in succ s; pred s done;
        assert false
      with Queue.Empty ->
        if Mark.mem t then grow_flow r s t a else a
    in
    let rec external_loop a =
      Mark.clear ();
      Mark.set s None Mark.Plus;
      let a' = internal_loop a in
      if a = a' then a else external_loop a'
    in
    let a = external_loop F.zero in
    (fun e -> try Result.find r e with Not_found -> F.flow (G.E.label e)), a

end
