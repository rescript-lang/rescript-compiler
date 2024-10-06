(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type edges = { id : int; deps : Vec_int.t }

module Edge_vec = Vec.Make (struct
  type t = edges

  let null = { id = 0; deps = Vec_int.empty () }
end)

type t = Edge_vec.t

(** 
    This graph is different the graph used in [scc] graph, since 
    we need dynamic shrink the graph, so for each vector the first node is it self ,
    it will also change the input.

    TODO: error handling (cycle handling) and defensive bad input (missing edges etc)
*)

let layered_dfs (g : t) =
  let queue = Queue.create () in
  let rec aux g =
    let new_entries =
      Edge_vec.inplace_filter_with
        (fun (x : edges) -> not (Vec_int.is_empty x.deps))
        ~cb_no:(fun x acc -> Set_int.add acc x.id)
        Set_int.empty g
    in
    if not (Set_int.is_empty new_entries) then (
      Queue.push new_entries queue;
      Edge_vec.iter g (fun edges ->
          Vec_int.inplace_filter
            (fun x -> not (Set_int.mem new_entries x))
            edges.deps);
      aux g)
  in
  aux g;
  queue
