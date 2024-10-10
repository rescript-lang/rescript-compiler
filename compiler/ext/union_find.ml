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

type t = {id: int array; sz: int array; mutable components: int}

let init n =
  let id = Array.make n 0 in
  for i = 0 to n - 1 do
    Array.unsafe_set id i i
  done;
  {id; sz = Array.make n 1; components = n}

let rec find_aux id_store p =
  let parent = Array.unsafe_get id_store p in
  if p <> parent then find_aux id_store parent else p

let find store p = find_aux store.id p

let union store p q =
  let id_store = store.id in
  let p_root = find_aux id_store p in
  let q_root = find_aux id_store q in
  if p_root <> q_root then
    let () = store.components <- store.components - 1 in
    let sz_store = store.sz in
    let sz_p_root = Array.unsafe_get sz_store p_root in
    let sz_q_root = Array.unsafe_get sz_store q_root in
    let bigger = sz_p_root + sz_q_root in
    (* Smaller root point to larger to make
       it more balanced
       it will introduce a cost for small root find,
       but major will not be impacted
    *)
    if sz_p_root < sz_q_root then (
      Array.unsafe_set id_store p q_root;
      Array.unsafe_set id_store p_root q_root;
      Array.unsafe_set sz_store q_root bigger (* little optimization *))
    else (
      Array.unsafe_set id_store q p_root;
      Array.unsafe_set id_store q_root p_root;
      Array.unsafe_set sz_store p_root bigger (* little optimization *))

let count store = store.components
