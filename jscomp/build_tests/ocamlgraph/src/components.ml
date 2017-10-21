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

  module H = Hashtbl.Make(G.V)

  (* iterative code using a stack (variable [cont] below) *)

  type action =
    | Finish of G.V.t * int
    | Visit of G.V.t * G.V.t
    | Test of G.V.t * G.V.t

  let scc g =
    let root = H.create 997 in
    let hashcomp = H.create 997 in
    let stack = ref [] in
    let numdfs = ref 0 in
    let numcomp = ref 0 in
    let rec pop x = function
      | (y, w) :: l when y > x ->
        H.add hashcomp w !numcomp;
        pop x l
      | l -> l
    in
    let cont = ref [] in
    let visit v =
      if not (H.mem root v) then begin
        let n = incr numdfs; !numdfs in
        H.add root v n;
        cont := Finish (v, n) :: !cont;
        G.iter_succ
          (fun w ->
             cont := Visit (v, w) :: Test (v, w) :: !cont)
          g v;
      end
    in
    let rec finish () = match !cont with
      | [] -> ()
      | action :: tail ->
        cont := tail;
        begin match action with
          | Finish (v, n) ->
            if H.find root v = n then begin
              H.add hashcomp v !numcomp;
              let s = pop n !stack in
              stack:= s;
              incr numcomp
            end else
              stack := (n, v) :: !stack;
          | Visit (_, w) -> visit w
          | Test (v, w) ->
            if not (H.mem hashcomp w) then
              H.replace root v (min (H.find root v) (H.find root w))
        end;
        finish ()
    in
    let visit_and_finish v =
      visit v;
      finish ()
    in
    G.iter_vertex visit_and_finish g;
    !numcomp, (fun v -> H.find hashcomp v)

  let scc_array g =
    let n,f = scc g in
    let t = Array.make n [] in
    G.iter_vertex (fun v -> let i = f v in t.(i) <- v :: t.(i)) g;
    t

  let scc_list g =
    let a = scc_array g in
    Array.fold_right (fun l acc -> l :: acc) a []

end

(** Connected components (for undirected graphs) *)

module type U = sig
  type t
  module V : Sig.COMPARABLE
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_edges : (V.t -> V.t -> unit) -> t -> unit
end

module Undirected(G: U) = struct

  module UF = Unionfind.Make(G.V)
  module H = Hashtbl.Make(G.V)

  let components g =
    let vertices = ref [] in
    G.iter_vertex (fun v -> vertices := v :: !vertices) g;
    let uf = UF.init !vertices in
    let visit u v = UF.union u v uf in
    G.iter_edges visit g;
    let count = ref 0 in
    let comp = H.create 5003 in
    let visit v =
      let v = UF.find v uf in
      if not (H.mem comp v) then begin H.add comp v !count; incr count end in
    G.iter_vertex visit g;
    !count, (fun v -> H.find comp (UF.find v uf))

  let components_array g =
    let n,f = components g in
    let t = Array.make n [] in
    G.iter_vertex (fun v -> let i = f v in t.(i) <- v :: t.(i)) g;
    t

  let components_list g =
    let a = components_array g in
    Array.fold_right (fun l acc -> l :: acc) a []

end
