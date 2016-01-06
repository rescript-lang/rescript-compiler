(* OCamlScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Author: Hongbo Zhang  *)



module Ident_set = Lambda.IdentSet 

(* 
    We have a current ident set 
    (mostly exports and variables held by side effect calls) and there is a data set,
    for each variable, its dependency 
    -- 
 *)
let calculate_used_idents 
    (ident_free_vars : (Ident.t, Ident_set.t) Hashtbl.t)
    (initial_idents : Ident.t list) = 
  let s = Ident_set.of_list initial_idents in
  let current_ident_sets = ref s in
  let delta = ref s in
  while 
    Ident_set.(
    delta := 
      diff (fold (fun  id acc  ->
        union acc (Hashtbl.find ident_free_vars id ))  !delta empty)
        !current_ident_sets;
     not (is_empty !delta)) do
    current_ident_sets := Ident_set.(union !current_ident_sets !delta)
  done;
  !current_ident_sets
