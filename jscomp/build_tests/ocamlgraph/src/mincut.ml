(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2013-2014                                               *)
(*  David Monniaux, Gabriel Radanne                                       *)
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
  module V : Sig.VERTEX
  val succ : t -> V.t -> V.t list
end

module Make (G : G) = struct

  module H = Hashtbl.Make (G.V)

  let find_default htbl x =
    try H.find htbl x
    with Not_found -> false

  let min_cutset gr first_node =
    let n_labels = H.create 97 in
    let l_labels = H.create 97 in

    let already_processed = H.create 97 in
    let is_already_processed x = find_default already_processed x in

    let on_the_stack = H.create 97 in
    let is_on_the_stack x = find_default on_the_stack x in

    let cut_set = ref [] in
    let counter = ref 1 in

    let rec step2 top rest_of_stack =
      assert (not (is_already_processed top));
      assert (not (is_on_the_stack top));
      H.add on_the_stack top true;
      H.add n_labels top !counter;
      counter := !counter + 1;
      H.add l_labels top 0;
      H.add already_processed top true;
      step3 (G.succ gr top) top rest_of_stack

    and step3 successors top rest_of_stack = match successors with
      | successor :: other_successors ->
        if not (is_already_processed successor)
        (* step 4 *)
        then step2 successor ((top,successors)::rest_of_stack)
        (* step 5 *)
        else begin
          let x =
            if is_on_the_stack successor
            then H.find n_labels successor
            else H.find l_labels successor
          in
          H.add l_labels top
            (max (H.find l_labels top) x) ;
          step3 other_successors top rest_of_stack
        end

      | [] -> begin
          (* step 7 *)
          if H.find l_labels top = H.find n_labels top
          then begin
            cut_set := top::!cut_set ;
            H.add l_labels top 0 ;
          end ;

          (* check added between algorithms C and D *)
          if H.find l_labels top > H.find n_labels top
          then raise (Invalid_argument "Graph.Mincut: graph not reducible")

          (* step 8 *)
          else match rest_of_stack with
            | [] -> !cut_set (* SUCCESS *)
            | (new_top, new_successors)::new_tail -> begin
                H.add on_the_stack top false;
                H.add l_labels new_top
                  (max (H.find l_labels top) (H.find l_labels new_top)) ;
                step3 new_successors new_top new_tail
              end
        end in

    (* step 2 *)
    step2 first_node []

end
